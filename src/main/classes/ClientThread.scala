package main.classes
import collection.mutable

class ClientThread(val client: Client, val ordersBuffer:mutable.Buffer[Order],
                   val buySellOrdersBuffer:mutable.Buffer[Order],
                   clientsBuffer:mutable.Buffer[Client]) extends Thread(new Runnable {

  override def run(): Unit = {
    var order:Order = null
    val allOwnOrders = findAllOwnOrders(ordersBuffer)
    buySellOrdersBuffer.append(allOwnOrders.head)


    for(i <- allOwnOrders.indices) {
      order = allOwnOrders(i)

      if(order.operation == 's') {
        sellOperation(order)
      }else {
        buyOperation(order)
      }
    }

  }
    def findAllOwnOrders(ordersBuffer:mutable.Buffer[Order]): mutable.Buffer[Order] = {
      val allOwnOrdersBuffer:mutable.Buffer[Order] = mutable.Buffer()
      for(i <- ordersBuffer.indices) {
        if(client.name.equals(ordersBuffer(i).name)) {
          allOwnOrdersBuffer.append(ordersBuffer(i))
        }
      }
      allOwnOrdersBuffer
    }
    def sellOperation(order: Order): Unit = {
      while (true) {
        for (i <- buySellOrdersBuffer.indices) {
          var candidateToSell = buySellOrdersBuffer(i)

          if (!order.operation.equals(candidateToSell.operation) && order.stockName.equals(candidateToSell.stockName) &&
            order.priceStock.compareTo(candidateToSell.priceStock) == -1) {
            println("Order: " + order.name + ", " + order.operation + ", " +
              order.stockName + ", " + order.priceStock + ", " + order.amountStock)

            println("Candidate: " + candidateToSell.name + ", " + candidateToSell.operation + ", " +
              candidateToSell.stockName + ", " + candidateToSell.priceStock + ", " + candidateToSell.amountStock + "\n")

            return
          }
        }
      }
    }
    def buyOperation(order: Order): Unit = {
      while (true) {
      for(i <- buySellOrdersBuffer.indices) {
        var candidateToBuy = buySellOrdersBuffer(i)


        if(!order.operation.equals(candidateToBuy.operation) && order.stockName.equals(candidateToBuy.stockName) &&
          order.priceStock.compareTo(candidateToBuy.priceStock) == 1) {

          println("Order: " + order.name + ", " + order.operation + ", " +
            order.stockName + ", " + order.priceStock + ", " + order.amountStock)

          println("Candidate: " + candidateToBuy.name + ", " + candidateToBuy.operation + ", " +
            candidateToBuy.stockName + ", " + candidateToBuy.priceStock + ", " + candidateToBuy.amountStock + "\n")

          return
        }
      }
      }
    }
    def executeBuy(order: Order, orderId:Int, candidateToBuy: Order, candidateId:Int,
                   buySellOrdersBuffer:mutable.Buffer[Order]): Unit = {
      if(order != null) {
        var copyOrder = order
        var copyCandidate = candidateToBuy

        buySellOrdersBuffer.remove(orderId)
        buySellOrdersBuffer.remove(candidateId)

        for(i <- clientsBuffer.indices) {

        }

      }
    }
}){

}
object ClientThread{
  val clientsThreadList:mutable.Buffer[ClientThread] = mutable.Buffer()
  def createClientThread(clientsBuffer:mutable.Buffer[Client], amount:Int, ordersBuffer:mutable.Buffer[Order],
                         buySellOrdersBuffer:mutable.Buffer[Order]): mutable.Buffer[ClientThread]= {
    for(i <- 0 until amount) {
      var clientThread = new ClientThread(clientsBuffer(i), ordersBuffer, buySellOrdersBuffer, clientsBuffer)
      clientsThreadList.append(clientThread)
      clientThread.start()
    }
    clientsThreadList
  }
}