object MacroTest {
  def main(args: Array[String]): Unit = {
    AsyncMini.spliceAndDice[String] {
      val y = ""
      val x: y.type = y;
      def foo(p: y.type): y.type = y
      def bar[A] = ()
      bar[y.type]

      var seq: Seq[_ <: x.type] = Nil
      seq = seq

      val z: y.type = foo({"boo"; x})
      (() => {
        println("called");
        x
      }).apply()
    }

    AsyncMini.spliceAndDice {
      def conjure[T]: T = {println("conjured"); null.asInstanceOf[T]}
      val p: List[Option[_]] = identity(conjure[List[Option[_]]])
      p
    }

    println("done")
  }
}
