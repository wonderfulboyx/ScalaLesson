//m: Additive[T]と値t1: T, t2: T, t3: Tは、次の条件を満たす必要があります。
//
//m.plus(m.zero, t1) == t1  // 単位元
//m.plus(t1, m.zero) == t1  // 単位元
//m.plus(t1, m.plus(t2, t3)) == m.plus(m.plus(t1, t2), t3) // 結合則
//このような条件を満たす型Tと単位元zero、演算plusを探し出し、Additive[T]を定義しましょう。この際、条件が満たされていることをいくつかの入力に対して確認してみましょう。また、定義したAdditive[T]をimplicitにして、Tの合計値を先ほどのsumで計算できることを確かめてみましょう。
//
//ヒント：このような条件を満たすものは無数にありますが、思いつかない人はたとえばx座標とy座標からなる点を表すクラスPointを考えてみると良いでしょう。
class vector(val _x:Int,val _y:Int){
  val x = _x
  val y = _y

  def plus(a:vector):vector={
    new vector(x + a.x, y + a.y)
  }
}


object ImplicitClass {

  trait Additive[A] {
    def plus(a: A, b: A): A
    def zero: A
  }

  object StringAdditive extends Additive[String] {
    def plus(a: String, b: String): String = a + b
    def zero: String = ""
  }

  object IntAdditive extends Additive[Int] {
    def plus(a: Int, b: Int): Int = a + b
    def zero: Int = 0
  }

  object VectorAdditive extends Additive[vector]{
    def plus(a: vector, b: vector): vector = a.plus(b)
    def zero: vector = new vector(0,0)

  }

  def sum[A](lst: List[A])(m: Additive[A]) = lst.foldLeft(m.zero)((x, y) => m.plus(x, y))


  def main(args: Array[String]): Unit = {
    val l = List(Some(2),Some(3),Some(5),Some(7),Some(11))
    val rslt = l(0).flatMap(i0 => l(1).flatMap(i1 => l(2).flatMap(i2 => l(3).flatMap(i3 =>  l(4).map(i4 => i0 * i1 * i2 * i3 * i4)))))
    println(rslt)
  }
}