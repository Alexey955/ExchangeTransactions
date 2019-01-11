package main.classes
import collection.mutable
class Order(val name:String, val operation:Char, val stockName:Char, var amountStock:Int, val priceStock:Int) {

}

object Order{
  def createOrdersList(seq:Seq[String]): mutable.Buffer[Order] = {
    val ordersList:mutable.Buffer[Order] = mutable.Buffer()
    for(i <- seq.indices) {
      var strFromSeq = seq(i)
      var subStr = strFromSeq.split("\t")
      var order = new Order(subStr(0), subStr(1).charAt(0), subStr(2).charAt(0), subStr(3).toInt, subStr(4).toInt)
      ordersList.append(order)
    }
    ordersList
  }
}