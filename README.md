scratch2
========

Prototype of redesigned type-checking/transformation for scala-async.

 - [macro](https://github.com/retronym/scratch2/blob/master/macros/macros.scala)
 - [usage](https://github.com/retronym/scratch2/blob/master/main/main.scala)

Here's what it does.

```scala
performing macro expansion AsyncMini#6986.spliceAndDice#11869[String#6553]({
  val y#12006: String#1799 = "";
  val x#12007: y#12006.type = y#12006;
  def foo#12008(p#12013: y#12006.type): String#1799 = y#12006;
  def bar#12009[A#12010 >: Nothing#3295 <: Any#3294]: Unit#1511 = ();
  bar#12009[y#12006.type];
  val z#12012: y#12006.type = foo#12008({
    "boo";
    x#12007
  });
  (() => {
  scala#23.this.Predef#801.println#6701("called");
  x#12007
})#12014.apply#12018()
}) at source-/Users/jason/code/scratch2/main/main.scala,line-3,offset=97
{
  class C#12041 extends AnyRef#2160 {
    def <init>#12043(): C#12041 = {
      C#12041.super.<init>#4664();
      ()
    };
    <stable> private[this] var y$lifted#12044: String#1799 = _;
    <stable> private[this] var x$lifted#12045: y$lifted#12044.type = _;
    <stable> private[this] var z$lifted#12046: y$lifted#12044.type = _;
    def foo#12047: String#1799 = {
      C#12041.this.y$lifted#12044 = "";
      C#12041.this.x$lifted#12045 = C#12041.this.y$lifted#12044;
      def foo#12008(p#12013: y$lifted#12044.type): String#1799 = C#12041.this.y$lifted#12044;
      def bar#12009[A#12010 >: Nothing#3295 <: Any#3294]: Unit#1511 = ();
      bar#12009[y$lifted#12044.type];
      C#12041.this.z$lifted#12046 = {
        val arg$1#12040: x$lifted#12045.type = {
          "boo";
          C#12041.this.x$lifted#12045
        };
        foo#12008(arg$1#12040)
      };
      (() => {
  scala#23.this.Predef#801.println#6701("called");
  C#12041.this.x$lifted#12045
})#12014.apply#12018()
    }
  };
  val c#12042: C#12041 = new C#12041();
  c#12042.foo#12047
}
```

TODO: expand the test with existentials
