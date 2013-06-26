import scala.language.experimental.macros
import scala.tools.nsc.Global

object FooMacros {

  import scala.reflect.macros.Context

  def foo[T](t0: T) = macro fooImpl[T]

  def fooImpl[T: c0.WeakTypeTag](c0: Context)(t0: c0.Expr[T]): c0.Expr[Any] = {
    val c = powerMode(c0)
    val t = t0.asInstanceOf[c.Expr[T]]

    import c.universe._, Flag._

    val splicee = t.tree // no resetLocalAttrs(t.tree) needed! Yay!

    val inputValDefs = t.tree match { case Block(stats, expr) => (stats :+ expr).collect { case vd: ValDef => vd}}
    // Lift a stable var for each Val-/Var-Def in the provided block into the class C
    val classValDefs = inputValDefs map {
      vd =>
        ValDef(Modifiers(MUTABLE | PRIVATE | LOCAL), newTermName(vd.name + "$lifted"), TypeTree(vd.symbol.tpe), EmptyTree)
    }

    val defaultConstructor = DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(()))))

    // Splice the tree as-is into the RHS of `C#foo`
    val fooDefDef = DefDef(Modifiers(), newTermName("foo"), List(), List(), TypeTree(definitions.AnyTpe), gen.mkZero(definitions.AnyTpe))

    val classDef = Block(List(
      // class C { def foo = ???; <STABLE> var x$lifted = _; ... }
      ClassDef(Modifiers(), newTypeName("C"), List(),
        Template(List(Ident(definitions.AnyRefClass)), emptyValDef, List(defaultConstructor, fooDefDef) ++ classValDefs)
      ),
      // val c = new C
      ValDef(Modifiers(), newTermName("c"), TypeTree(), Apply(Select(New(Ident(newTypeName("C"))), nme.CONSTRUCTOR), List())),
      // c.foo
      Select(Ident(newTermName("c")), newTermName("foo"))),
      // c: Any
      Typed(Ident(newTermName("c")), Ident(definitions.AnyClass))
    )

    // Typecheck what we have so far. This will assign symbols to the class and lifted vars.
    val cd1 = c.typeCheck(classDef)

    // Stabilize the vars. We are declaring them "write once", which allows
    // us to use them as a prefix for a path, if the original code needs it.
    classValDefs.map(_.symbol.setFlag(reflect.internal.Flags.STABLE))

    // Repair the owners of splicees to reflect the new location.
    // Alternatively, we could have typechecked the shell before splicing, and
    // patched in the splicee with a Transformer and an explicit changeOwner on
    // the splicee.
    val helper = new MacroImpl[c.universe.type](c.universe)
    val callSiteOwner = c.callsiteTyper.context.owner

    val fromSyms = inputValDefs.map(_.symbol)
    val toSyms   = classValDefs.map(_.symbol)

    class UseFields(ctx: analyzer.Context) extends helper.TypingTransformer(ctx) {
      override def transform(tree: Tree): Tree = tree match {
        case _ if Set(callSiteOwner, callSiteOwner.owner).contains(currentOwner) =>
          // Don't transform until we
          super.transform(tree)
        case ValDef(_, _, _, rhs) if toSyms.contains(tree.symbol) =>
          val fieldSym = tree.symbol
          val set = Assign(Select(gen.mkAttributedThis(fieldSym.owner), fieldSym), transform(rhs))
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
    val FooName = newTermName("foo")
    val transformed = helper.transformAt(c.callsiteTyper.context, cd1) {
      case dd @ DefDef(mods, name @ FooName, tparams, vparamss, tpt, rhs) =>
        (context: analyzer.Context) => {
          val changedOwner = splicee.changeOwner(callSiteOwner -> context.owner)
          val substed1 = changedOwner.substituteSymbols(fromSyms, toSyms)
          val newRhs = new UseFields(context).transform(substed1)
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
      override def transform(t: Tree): Tree = {
        // TODO see `fixerUpper` in the pattern matcher for a slightly simpler way to do this.
        if (currentOwner.hasTransOwner(macroCallSiteOwner) && currentOwner.owner != macroCallSiteOwner)
          new ChangeOwnerAndModuleClassTraverser(macroCallSiteOwner, currentOwner)(t)
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
    var localTyper: analyzer.Typer = analyzer.newTyper(ctx)
    protected var curTree: Tree = _
    protected def typedPos(pos: Position)(tree: Tree) = localTyper typed { atPos(pos)(tree) }

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
