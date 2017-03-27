// 型パラメータの指定の仕方
class Cell[A](var value: A) {
  def put(newValue: A): Unit = {
    value = newValue
  }

  def get(): A = value
}

// 型パラメータを使うとタプルが作れる。
class Pair[A, B](val a: A, val b: B) {
  override def toString(): String = "(" + a + "," + b + ")"
}

// 共変ヴァージョン
class coPair[+A, +B](val a: A, val b: B) {
  override def toString(): String = "(" + a + "," + b + ")"
}

object typeParameter {
  def call (): Unit = {



    val cell = new Cell[Int](1)
    cell.put(2)
    cell.get()
    // NG
    // cell.put("something")

    // 除算して、商とあまりをタプルに入れて返す関数。
    def divide(m: Int, n: Int): Pair[Int, Int] = new Pair[Int, Int](m / n, m % n)

    divide(7, 3)
    // -> Pair[Int,Int] = (2,1)

    val m = 7; val n = 3;
    val div_ret = (m / n, m % n) // これだけでタプルができる。
    println(div_ret)

    // 非変とは
    // 型パラメータAとBがあったとき、A = Bの時のみ以下の代入が許される
    // val : G[A] = G[B]

    // 共変とは
    // 型パラメータAとBがあったとき、A が B を継承しているときにのみ、以下の代入が許される
    // val : G[B] = G[A]
    // Scalaでは、クラス定義時に
    // class G[+A]
    // とすると共編になる。

    // 共変でも安全な例
    val pair: coPair[AnyRef, AnyRef] = new coPair[String, String]("foo", "bar")
    //一般的には、一度作成したら変更できない（immutable）などの型パラメータは共変にしても多くの場合問題がありません。


    // 上限境界
    // 型パラメータが何を継承しているか指定する
    abstract class Show {
      def show: String
    }
    // 型パラメータがAの上限境界にShowを指定する。
    class ShowablePair[A <: Show, B <: Show](val a: A, val b: B) extends Show {
      override def show: String = "(" + a.show + "," + b.show + ")"
    }

    // 下限境界
    // 型パラメータがどのような型のスーパータイプであるかを指定する
    abstract class Stack[+A]{
      // NG なんでNGなのかはよくわからない...
      // def push(element: A): Stack[A]
      // OK
      def push[E >: A](element: E): Stack[E]
      def top: A
      def pop: Stack[A]
      def isEmpty: Boolean
    }
  }
}
