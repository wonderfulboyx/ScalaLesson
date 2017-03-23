// traitのはなし
// https://dwango.github.io/scala_text/trait.html

trait Greeter {
  def greet(): Unit
}

trait Robot {
  // 自分型.
  // こうすることでGreeterのgreetを隠せる=インスタンスから呼べなくなる。
  self: Greeter =>
  def start(): Unit = greet()
}

trait Robot2 extends Greeter {
  // 普通に継承してgreet()を返す。
  // しかし普通に継承されているのでgreetはRobot2クラスのメソッドにもなっている。
  def start(): Unit = greet()
}

object GreetRobot {
  def call (args: Array[String]): Unit = {

    // あとからGreetを継承したtraitをつくってやって...
    trait HelloGreeter extends Greeter {
      def greet(): Unit = println("Hello!")
    }

    // Robotに渡してやる。
    //このようにあとから利用するモジュールの実装を与えることを依存性の注入(Dependency Injection)と呼ぶ
    val robot = new Robot with HelloGreeter
    robot.start()
    robot.greet() //これは呼べないはず...だったんだけど呼べちゃう。

    val robot2 = new Robot2 with HelloGreeter
    robot2.start()
    robot2.greet()
  }
}
