//package ml.wolfe.examples
//
//import ml.wolfe.{MaxProduct, Wolfe}
//import ml.wolfe.macros.{Library, OptimizedOperators}
//import Wolfe._
//import ml.wolfe.macros.OptimizedOperators._
//import cc.factorie.optimize.{AveragedPerceptron, OnlineTrainer}
//
///**
// * Created by rockt on 16/04/2014.
// */
//object MatrixFactorizationExample {
//  case class User(name: String, items: Seq[Item])
//  case class Item(id: String)
//  case class UserItem(user: User, item: Item, rating: Double)
//
//  //dimension of embeddings
//  val k = 10
//
//  //model
//  def s(w: Vector)(u: UserItem) =
//    sum { over(0 until k) of (i => w(u.item -> i) * w(u.user -> i)) } +
//    sum { over(u.user.items) (i => w(i -> u.item)) }
//
//  // training loss over observed cells
//  def loss(data: Seq[UserItem])(w:Vector) = sum { over(data) (d => math.pow(d.rating - s(w)(d), 2)) }
//
//  //@OptimizeByLearning(new OnlineTrainer(_, new AveragedPerceptron, 2, 10))
//  def learn(data: Seq[UserItem]) = argmin { over[Vector] of loss(data) }
//
//  def main(args: Array[String]) {
//    import cc.factorie.random
//    val items = (0 to 10).map(i => Item(i.toString))
//    val users = (0 to 10).map(i => User(i.toString, random.shuffle(items).take(random.nextInt(10))))
//    val userItems = (for {
//      user <- users
//      item <- items
//    } yield UserItem(user, item, random.nextDouble())).filter(u => random.nextDouble() > 0.3)
//
//    println("Users:\n" + users)
//    println()
//    println("Items:\n" + items)
//
//    learn(userItems)
//  }
//}
