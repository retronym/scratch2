object MacroTest {
  def main(args: Array[String]): Unit = {
    AsyncMini.spliceAndDice[String] {
      val y = ""
      val x: y.type = y;
      def foo(p: y.type): y.type = y
      def bar[A] = ()
      bar[y.type]
      val z: y.type = foo({"boo"; x})
      (() => {
        println("called");
        x
      }).apply()
    }
    println("done")
  }
}
