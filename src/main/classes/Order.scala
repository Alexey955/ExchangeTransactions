package main.classes
import collection.mutable
class Order(val name:String, val operation:Char, val stockName:Char, val priceStock:Int, var amountStock:Int) {

}

object Order{
  def createOrdersList(seq:Seq[String], amount:Int): mutable.Buffer[Order] = {
    val ordersList:mutable.Buffer[Order] = mutable.Buffer()
    for(i <- 0 until amount) {
      var strFromSeq = seq(i)
      var subStr = strFromSeq.split("\t")
      var order = new Order(subStr(0), subStr(1).charAt(0), subStr(2).charAt(0), subStr(3).toInt, subStr(4).toInt)
      ordersList.append(order)
    }
    ordersList
  }
}