import scala.language.experimental.macros
import scala.tools.nsc.Global

object AsyncMini {

  import scala.reflect.macros.Context

  def spliceAndDice[T](t0: T) = macro fooImpl[T]

  def fooImpl[T: c0.WeakTypeTag](c0: Context)(t0: c0.Expr[T]): c0.Expr[Any] = {
    val c = powerMode(c0)
    val t = t0.asInstanceOf[c.Expr[T]]

    import c.universe._, Flag._

    val splicee = t.tree // no resetLocalAttrs(t.tree) needed! Yay!

    val helper = new MacroImpl[c.universe.type](c.universe)

    //
    // Converts foo(expr1, ..., exprN) to { val arg$1 = expr1; ... val arg$N = exprN; foo(arg$1, ..., arg$N)
    //
    class DumbAnf(ctx: analyzer.Context) extends helper.TypingTransformer(ctx) {
      import treeInfo.isExprSafeToInline
      override def transform(tree: Tree): Tree = tree match {
        case Apply(fun, args) if args.exists(x => !isExprSafeToInline(x)) => // doesn't handle multiple param lists.
          val temps: List[ValDef] = transformTrees(args).map { a =>
            val name = localTyper.context.unit.freshTermName("arg$")
            val sym = currentOwner0.newTermSymbol(name, tree.pos).setInfo(a.tpe)
            ValDef(sym, a)
          }
          localTyper.typedPos(tree.pos) {
            val refs = temps.map(_.symbol).map(gen.mkAttributedStableRef(_)) // doesn't handle varargs, by-names
            Block(temps, treeCopy.Apply(tree, transform(fun), refs))
          }
        case _: Function | _: DefDef | _: ImplDef => // probably not exhaustive
          tree // don't descend
        case _ =>
          super.transform(tree)
      }
    }
    val spliceeAnf: Tree = new DumbAnf(c.callsiteTyper.context).transform(splicee)

    // Lift a stable var for each Val-/Var-Def in the provided block into the class C
    val origValDefs = spliceeAnf match {
      case Block(stats, expr) => (stats :+ expr).collect { case vd: ValDef => vd }
      case x                  => List()
    }
    val classValDefs = origValDefs map { vd =>
      val mods = Modifiers(MUTABLE | PRIVATE | LOCAL)
      val name = newTermName(vd.name + "$lifted")
      ValDef(mods, name, TypeTree(vd.symbol.tpe), EmptyTree)
    }

    // def this() = super()
    val defaultConstructor = DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(()))))

    // This is the method under which we will splice the tree.
    // def foo: <<splicee type>> = ???
    //
    // We'll fill in the method body later.
    //
    val FooName = newTermName("foo")
    val fooDefDef = DefDef(Modifiers(), FooName, List(), List(), TypeTree(spliceeAnf.tpe), gen.mkZero(spliceeAnf.tpe))

    // Create the class def
    // { class C { def foo = ???; <STABLE> var x$lifted = _; ... }; val c = new C; c.foo }
    val classDef = Block(List(
      ClassDef(Modifiers(), newTypeName("C"), List(),
        Template(List(Ident(definitions.AnyRefClass)), emptyValDef, (defaultConstructor :: classValDefs) :+ fooDefDef)
      ),
      ValDef(Modifiers(), newTermName("c"), TypeTree(), Apply(Select(New(Ident(newTypeName("C"))), nme.CONSTRUCTOR), List()))),
      Select(Ident(newTermName("c")), FooName)
    )

    // Typecheck what we have so far. This will assign symbols to the class and lifted vars.
    val cd1 = c.typeCheck(classDef)

    // Stabilize the vars. We are declaring them "write once", which allows
    // us to use them as a prefix for a path, if the original code needs it.
    classValDefs.map(_.symbol.setFlag(reflect.internal.Flags.STABLE))

    val callSiteOwner = c.callsiteTyper.context.owner
    val fromSyms = origValDefs.map(_.symbol)
    val toSyms   = classValDefs.map(_.symbol)

    // subsituted the old symbols for the new; this is needed if the types of the lifted
    // val defs refer to the symbols of other vals that have been lifted.
    val cd2 = cd1.substituteSymbols(fromSyms, toSyms)


    // Replace the ValDefs in the splicee with Assigns to the corresponding lifted
    // fields. Similarly, replace references to them with references to the field.
    //
    // This transform will be only be run on the RHS of `def foo`.
    class UseFields(ctx: analyzer.Context) extends helper.TypingTransformer(ctx) {
      override def transform(tree: Tree): Tree = tree match {
        case ValDef(_, _, _, rhs) if toSyms.contains(tree.symbol) =>
          val fieldSym = tree.symbol
          val set = Assign(gen.mkAttributedStableRef(fieldSym.owner.thisType, fieldSym), transform(rhs))
          localTyper.typedPos(tree.pos)(set)
        case Ident(name) if toSyms.contains(tree.symbol) =>
          val fieldSym = tree.symbol
          localTyper.typedPos(tree.pos)(
            gen.mkAttributedStableRef(fieldSym.owner.thisType, fieldSym)
          ).setType(tree.tpe)
        case _ =>
          super.transform(tree)
      }
    }

    val transformed = helper.transformAt(c.callsiteTyper.context, cd2) {
      case dd @ DefDef(mods, name @ FooName, tparams, vparamss, tpt, rhs) =>
        (context: analyzer.Context) => {
          // spliceeAnf.changeOwner(callSiteOwner -> dd.symbol) is not enough!
          new helper.ChangeOwnerAndModuleClassTraverser(callSiteOwner, dd.symbol).traverse(spliceeAnf)
          // substitute old symbols for the new. We have to use the `UseFields` transform
          // afterwards to complete things.
          val spliceeAnfFixedOwnerSyms = spliceeAnf.substituteSymbols(fromSyms, toSyms)
          val newRhs = new UseFields(context).transform(spliceeAnfFixedOwnerSyms)
          treeCopy.DefDef(dd, mods, name, tparams, vparamss, tpt, newRhs)
        }
    }

    c.Expr(transformed).asInstanceOf[c0.Expr[Any]]
  }

  def powerMode(c: Context) = {
    c.asInstanceOf[c.type {val universe: Global; val callsiteTyper: universe.analyzer.Typer}]
  }
}

final class MacroImpl[G <: Global with Singleton](val g: G) {
  import g._

  class ChangeOwnerAndModuleClassTraverser(oldowner: Symbol, newowner: Symbol)
    extends ChangeOwnerTraverser(oldowner, newowner) {

    override def traverse(tree: Tree) {
      tree match {
        case _: DefTree => change(tree.symbol.moduleClass)
        case _ =>
      }
      super.traverse(tree)
    }
  }

  def repairOwners(t: Tree, macroCallSiteOwner: Symbol): g.Tree = {
    object repairer extends Transformer {
      def currentOwner0: Symbol = currentOwner
      override def transform(t: Tree): Tree = {
        // TODO see `fixerUpper` in the pattern matcher for a slightly simpler way to do this.
        if (currentOwner0.hasTransOwner(macroCallSiteOwner) && currentOwner0.owner != macroCallSiteOwner)
          new ChangeOwnerAndModuleClassTraverser(macroCallSiteOwner, currentOwner0)(t)
        else super.transform(t)
      }
    }
    repairer transform t
  }

  def transformAt(ctx: analyzer.Context, tree: Tree)(f: PartialFunction[Tree, (analyzer.Context => Tree)]) = {
    object trans extends TypingTransformer(ctx) {
      override def transform(tree: Tree): Tree = {
        if (f.isDefinedAt(tree)) {
          f(tree)(localTyper.context)
        } else super.transform(tree)
      }
    }
    trans.transform(tree)
  }

  // duplicated in part from TypingTransfomers
  abstract class TypingTransformer(ctx: analyzer.Context) extends Transformer {
    currentOwner = ctx.owner
    var localTyper: analyzer.Typer = analyzer.newTyper(ctx)
    protected var curTree: Tree = _
    protected def typedPos(pos: Position)(tree: Tree) = localTyper typed { atPos(pos)(tree) }
    def currentOwner0: Symbol = currentOwner

    override final def atOwner[A](owner: Symbol)(trans: => A): A = atOwner(curTree, owner)(trans)

    def atOwner[A](tree: Tree, owner: Symbol)(trans: => A): A = {
      val savedLocalTyper = localTyper
      localTyper = localTyper.atOwner(tree, if (owner.isModule) owner.moduleClass else owner)
      try super.atOwner(owner)(trans)
      finally localTyper = savedLocalTyper
    }

    override def transform(tree: Tree): Tree = {
      curTree = tree
      tree match {
        case Template(_, _, _) =>
          // enter template into context chain
          atOwner(currentOwner) { super.transform(tree) }
        case PackageDef(_, _) =>
          atOwner(tree.symbol) { super.transform(tree) }
        case _ =>
          super.transform(tree)
      }
    }
  }
}
