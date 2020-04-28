package referentialTransparency

/*
  see: https://typelevel.org/blog/2017/05/02/io-monad-for-cats.html
 */
object NotRefTransparent extends util.App {

  @scala.annotation.nowarn("cat=other-pure-statement&msg=a pure expression does nothing in statement position:ws")
  def func(ioa1: Unit, ioa2: Unit): Unit = {
    ioa1
    ioa2
  }

  func(println("hi"), println("hi")) // prints "hi" twice
  //=> hi
  //=> hi

  println("-----")

  val x: Unit = println("hi")
  func(x, x) // prints "hi" once
  //=> hi

  // 'func' IS NOT referentially transparent!

}
