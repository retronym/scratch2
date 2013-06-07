import scala.reflect.runtime.universe._
import scala.language.experimental.macros
import scala.reflect.runtime.{ currentMirror => cm }
import scala.reflect.runtime.{ universe => ru }
import scala.tools.reflect.ToolBox

class A[T] {
  def foo(lam: T => Any): Unit = macro FooMacros.fooImpl[T]
}

object B {
  def fooTree[T](lam: T => Any, expr: ru.Expr[T => Any]): Unit = {
    expr.tree.asInstanceOf[Function].vparams.foreach(t=>t.tpe)
  }
}

object FooMacros {
  import scala.reflect.macros.Context

  def fooImpl[T](c: Context)(lam: c.Expr[T => Any]): c.Expr[Unit] = {
    import c.{ universe => cu }
    val fExpr = c.Expr[Expr[T => Any]](c.reifyTree(cu.treeBuild.mkRuntimeUniverseRef, cu.EmptyTree, c.typeCheck(lam.tree)))
    cu.reify { B.fooTree(lam.splice, fExpr.splice) }
  }
}
