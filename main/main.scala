object MacroTest {
  def main(args: Array[String]): Unit = {
    FooMacros.foo[String] {
      val y = ""
      val x: y.type = y;
      (() => {
        println("called");
        x
      }).apply()
    }
    println("done")
  }
}
