class Trade(val id: String, val symbol: String, val amount: Int)

object FreeVarTest {

  val c = "xx"

  def main(args: Array[String]): Unit = {
    var freevar = "ms"
    val am = 20
    val a = new A[Trade]
    a.foo(t => freevar)
    println("succeed")

    println("try more")
    a.foo(t => t.id == freevar && (t.amount < am || t.symbol != c))
  }
}
