
object HelloWorld {
  def main (args: Array[String]): Unit = {

    trait HelloGleeter extends Greeter {
      def greet(): Unit = println("Hello!")
    }

    val robot2 = new Robot2 with HelloGleeter
    robot2.start()
    robot2.greet()

    val robot = new Robot with HelloGleeter
    robot.start()
    robot.greet()
  }
}
