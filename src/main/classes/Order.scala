package classes
import collection.mutable
class Order(private val name:String, private val operation:Char, private val stockName:Char,
            private var amountStock:Int, private val priceStock:Int) {

  def getName: String = {
    name
  }

  def getOperation: Char = {
    operation
  }

  def getStockName: Char = {
    stockName
  }

  def getPriceStock: Int = {
    priceStock
  }

  def getAmountStock: Int = {
    amountStock
  }

  def minusStock(amount:Int): Unit = {
    amountStock += amount
  }
}

object Order{
  def createOrdersList(seq:Seq[String]): mutable.Buffer[Order] = {

    val ordersList:mutable.Buffer[Order] = mutable.Buffer()

    for(i <- seq.indices) {
      val strFromSeq = seq(i)
      val subStr = strFromSeq.split("\t")
      val order = new Order(subStr(0), subStr(1).charAt(0), subStr(2).charAt(0), subStr(3).toInt, subStr(4).toInt)

      ordersList.append(order)
    }
    ordersList
  }
}