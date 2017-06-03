// 初期化順序の話。

trait traitA {
  val foo: String
}

trait traitB extends traitA {
  val bar = foo + "World"
}

class traitC extends traitB {
  val foo = "Hello"

  def printBar(): Unit = println(bar)
}

// 初期化順序の回避
trait traitB2 extends traitA {
  // def または lazy valを使うことで遅延評価が可能になる。
  // デメリット: lazy val は valより重い。 def は毎回値が評価されてしまう。
  // val の値で val を作り出すような場合、 lazy val や def は検討されて良い。
  lazy val bar = foo + "World"
}

class traitC2 extends traitB2 {
  val foo = "Hello"

  def printBar(): Unit = println()
}

object traitOder {
  def call (): Unit = {
    ( new traitC ).printBar()
    // -> nullWorld
    // スーパークラスから順に初期化されていくので、
    // Bが初期化された時点ではfooはnullだったのでnullWorldになってしまう。
    ( new traitC2 ).printBar()
    // -> HelloWorld
    // 遅延評価され、意図した順番で初期化された。
  }
}
