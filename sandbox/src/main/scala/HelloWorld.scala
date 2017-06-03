//二分木（子の数が最大で2つであるような木構造）を表す型TreeとBranch, Emptyを考えます：
//
//sealed abstract class Tree
//case class Branch(value: Int, left: Tree, right: Tree) extends Tree
//case object Empty extends Tree
//子が2つで左の子の値が2、右の子の値が3、自分自身の値が1の木構造はたとえば次のようにして定義することができます。
//
//scala> val tree: Tree = Branch(1, Branch(2, Empty, Empty), Branch(3, Empty, Empty))
//tree: Tree = Branch(1,Branch(2,Empty,Empty),Branch(3,Empty,Empty))
//このような木構造に対して、
//
//最大値を求めるmaxメソッド：
//最小値を求めるminメソッド：
//深さを求めるdepthメソッド：
//def max(tree: Tree): Int = ???
//def min(tree: Tree): Int = ???
//def depth(tree: Tree): Int = ???
//をそれぞれ定義してみましょう。なお、
//
//depth(Empty) == 0
//depth(Branch(10, Empty, Empty)) == 1
//depth(Branch(10, Branch(20,
//  Empty,
//  Empty
//), Empty)) == 2
//// 右のBranchの方が、左のBranchよりも深い
//depth(Branch(10, Branch(20,
//  Empty,
//  Empty
//), Branch(30,
//  Branch(40,
//    Empty,
//    Empty
//  ),
//  Empty))) == 3
//です。
//
//余裕があれば木構造を、
//
//左の子孫の全ての値 <= 自分自身の値 < 右の子孫の全部の値
//となるような木構造に変換するsortメソッド：
//
//def sort(tree: Tree): Tree = ???
//を定義してみましょう。なお、sortメソッドは、葉ノードでないノードの個数と値が同じであれば元の構造と同じでなくても良いものとします。

sealed abstract class Tree
case class Branch(value: Int, left: Tree, right: Tree) extends Tree
case object Empty extends Tree


//def min(tree: Tree): Int = ???



object HelloWorld {
  def max(tree: Tree): Int = tree match {
    case Branch(v, Empty, Empty) => v
    case Branch(v, left, Empty)=> {
      val l = max(left)
      if(l < v) v else l
    }
    case Branch(v, Empty, right) => {
      val r = max(right)
      if(r < v) v else r
    }
    case Branch(v,left,right) => {
      val l = max(left)
      val temp = if(l < v) v else l
      val r = max(right)
      if(r < temp) temp else r
    }
  }

  def depth(tree: Tree): Int = tree match {
    case Empty => 0
    case Branch(v,right,left) => {
      val r = depth(right)
      val l = depth(left)
      (if(l < r) r else l) + 1
    }
  }

  def main (args: Array[String]): Unit = {
    val tree: Tree = Branch(1, Branch(2, Empty, Empty), Branch(3, Empty, Empty))
    println(max(tree))
    println(depth(tree))
  }
}