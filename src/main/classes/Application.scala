package main.classes

import scala.io.Source
import collection.mutable
object Application extends App{

  private val ordersPath = "d:ExchangeTransactions/orders.txt"
//  val ordersAmount = Source.fromFile(ordersPath).getLines().size
  val seqOrder = Source.fromFile(ordersPath).getLines().toSeq
  val ordersBuffer = Order.createOrdersList(seqOrder)
//  ordersBuffer.foreach(x => println(x.name + " " + x.operation + " " + x.stockName + " " + x.amountStock + " " + x.priceStock))

  private val clientsPath = "d:ExchangeTransactions/clients.txt"
//  val clientsAmount = Source.fromFile(clientsPath).getLines().size
  val seqClients = Source.fromFile(clientsPath).getLines().toSeq
  val clientsBuffer = Client.createClients(seqClients)
//  clientsBuffer.foreach(x => println(x.name + " " + x.money + " " + x.A + " " + x.B + " " + x.C + " " + x.D))

  val clientsWithOrdersBuffer = ClientWithOrdersBuffer.createClientsWithOrdersBuffer(ordersBuffer,clientsBuffer)

  val buySellOrdersBuffer:mutable.Buffer[Order] = mutable.Buffer()


  var reverseCounter = 1
  var numFrom = 5
  var numTo = 1
  var numMediator = 1
//  for(i <- seqClients.indices) {
  var counter = 0
  var stopCounter = 0
  while (stopCounter != seqClients.size){
    for(j <- seqClients.indices) {
//      if(i == 1) buySellOrdersBuffer.foreach(x => println(x.name + " " + x.operation + " " + x.stockName + " " + x.amountStock + " " + x.priceStock))
      if(clientsWithOrdersBuffer(j).ownOrdersBuffer(counter) != null) {
        buySellOrdersBuffer.append(clientsWithOrdersBuffer(j).ownOrdersBuffer(counter))
      }else{
       stopCounter += 1
      }
    }
    if(stopCounter != seqClients.size) {
      stopCounter = 0
    }
    counter += 1

    if(reverseCounter == 1) {
      numFrom = 0
      numTo = seqClients.size
      numMediator = 1
      reverseCounter = 0
    }else{
      numFrom = seqClients.size
      numTo = 0
      numMediator = -1
      reverseCounter = 1
    }
    for(j <- numFrom to (numTo, numMediator)) {
      var ownBuySellOrdersBuffer = buySellOrdersBuffer.filter(x => x.name == clientsWithOrdersBuffer(j).name)
      var clientId = j
      for(k <- ownBuySellOrdersBuffer.indices) {
        if(ownBuySellOrdersBuffer(k).operation == 's') {
          sellOperation(ownBuySellOrdersBuffer(k), buySellOrdersBuffer, clientId)
        }else{
          //buy def
        }
      }
    }
  }

  def sellOperation(order: Order, buySellOrdersBuffer:mutable.Buffer[Order], sellerId:Int): Unit = {
    for(i <- buySellOrdersBuffer.indices) {
      var candidateOrder = buySellOrdersBuffer(i)
      if(!order.name.equals(candidateOrder.name) && !order.operation.equals(candidateOrder.operation) &&
          order.stockName.equals(candidateOrder.stockName) &&
          order.priceStock.compareTo(candidateOrder.priceStock) != 1) {

        var candidateId = 0
        if((candidateId = checkMoney(clientsWithOrdersBuffer, candidateOrder.name, candidateOrder, order)) != 0) {
//          var candidateId = checkMoney(clientsWithOrdersBuffer, candidateOrder.name, candidateOrder, order)
          sellStock(clientsWithOrdersBuffer, order, sellerId, candidateOrder,candidateId)
//          println("SURPRISE")
        }
      }
    }
  }

  def checkMoney(clientsWithOrdersBuffer:mutable.Buffer[ClientWithOrdersBuffer], candidateName:String, candidateOrder:Order,
                 order: Order):Int = {
    var result = 0
    for(i <- clientsWithOrdersBuffer.indices) {
      if(clientsWithOrdersBuffer(i).name.equals(candidateName)) {
        if(clientsWithOrdersBuffer(i).money.compareTo(candidateOrder.priceStock) != -1) {
          result = i
        }else{
          result = 0
        }
      }
    }
    result
  }

  def sellStock(clientsWithOrdersBuffer:mutable.Buffer[ClientWithOrdersBuffer], sellerOrder:Order, sellerId:Int,
                candidateOrder:Order, candidateId:Int): Unit = {
    var candidate = clientsWithOrdersBuffer(candidateId)
    for(i <- 1 until sellerOrder.amountStock) {
      if(candidate.money <= (candidateOrder.priceStock * i)) {
        //if money less
      }
    }
    if(sellerOrder.amountStock == candidateOrder.amountStock){
      changeWhenStockEqual(clientsWithOrdersBuffer(sellerId),sellerOrder,candidate,candidateOrder)
//      println("SURPRISE")
    }else{
      //change if candidateOrder.amountStock more
    }
  }

  def deleteOrders(): Unit = {

  }

  def changeWhenStockEqual(seller:ClientWithOrdersBuffer, sellerOrder:Order,
                           candidate:ClientWithOrdersBuffer, candidateOrder:Order): Unit = {

    seller.money += candidateOrder.amountStock * candidateOrder.priceStock
    candidate.money -= candidateOrder.amountStock * candidateOrder.priceStock

    if(sellerOrder.stockName == 'A') {
      seller.A -= candidateOrder.amountStock
      candidate.A += candidateOrder.amountStock
    }else if(sellerOrder.stockName == 'B') {
      seller.B -= candidateOrder.amountStock
      candidate.B += candidateOrder.amountStock
    }else if(sellerOrder.stockName == 'C') {
      seller.C -= candidateOrder.amountStock
      candidate.C += candidateOrder.amountStock
    }else {
      seller.D -= candidateOrder.amountStock
      candidate.D += candidateOrder.amountStock
    }
    var returnCounter = 0
    for(i <- 0 until(buySellOrdersBuffer.size)) {
      if(buySellOrdersBuffer(i) == sellerOrder){
        buySellOrdersBuffer.remove(i)
        returnCounter += 1
      }
      if(buySellOrdersBuffer(i) == candidateOrder) {
        buySellOrdersBuffer.remove(i)
        returnCounter += 1
      }
      if(returnCounter == 2)
        return
    }
  }
  

  println("End\n")
//  Source.fromFile("d:ExchangeTransactions/orders.txt").getLines().foreach(println)

}
