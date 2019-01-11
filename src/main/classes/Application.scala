package main.classes

import scala.io.Source
import collection.mutable
object Application extends App{
  var theMainCounter:Long = 0
  private val ordersPath = "d:ExchangeTransactions/orders.txt"
  val seqOrder = Source.fromFile(ordersPath).getLines().toSeq
  val ordersBuffer = Order.createOrdersList(seqOrder)

  private val clientsPath = "d:ExchangeTransactions/clients.txt"
  val seqClients = Source.fromFile(clientsPath).getLines().toSeq
  val clientsBuffer = Client.createClients(seqClients)

  val clientsWithOrdersBuffer = ClientWithOrdersBuffer.createClientsWithOrdersBuffer(ordersBuffer,clientsBuffer)

  val buySellOrdersBuffer:mutable.Buffer[Order] = mutable.Buffer()

  var reverseCounter = 1
  var numFrom = 5
  var numTo = 1
  var numMediator = 1
  var counter = 0
  var stopCounter = 0
  while (stopCounter != seqClients.size){
    for(j <- seqClients.indices) {
        if(counter < clientsWithOrdersBuffer(j).ownOrdersBuffer.size) {
        buySellOrdersBuffer.append(clientsWithOrdersBuffer(j).ownOrdersBuffer(counter))
      }else{
       stopCounter += 1
      }
      /*if(stopCounter > 0) {
        println(">0")
      }
      if(stopCounter > 1) {
        println(">1")
      }
      if(stopCounter > 3) {
        println(">3")
      }
      if(stopCounter > 7) {
        println(">7")
      }*/
    }
    if(stopCounter != seqClients.size) {
      stopCounter = 0
    }

    if(stopCounter == seqClients.size) {
      println("WTF")
      println("TheMainCounter = " + theMainCounter)
    }


    counter += 1

    if(reverseCounter == 1) {
      numFrom = 0
      numTo = seqClients.size - 1
      numMediator = 1
      reverseCounter = 0
    }else{
      numFrom = seqClients.size - 1
      numTo = 0
      numMediator = -1
      reverseCounter = 1
    }
    for(j <- numFrom to (numTo, numMediator)) {
      var ownBuySellOrdersBuffer = buySellOrdersBuffer.filter(x => x.name == clientsWithOrdersBuffer(j).name)
      var clientId = j

      pickSellOrBuy(ownBuySellOrdersBuffer, clientId)
    }
    var inttt = 10
  }

  def pickSellOrBuy(ownBuySellOrdersBuffer:mutable.Buffer[Order], clientId:Int): Unit = {
    for(k <- ownBuySellOrdersBuffer.indices) {
    if(ownBuySellOrdersBuffer(k).operation == 's') {

      sellOperation(ownBuySellOrdersBuffer(k), buySellOrdersBuffer, clientId)
      if (k == ownBuySellOrdersBuffer.size - 1)
        return

    }else{
      buyOperation(ownBuySellOrdersBuffer(k), buySellOrdersBuffer, clientId)
      if(k == ownBuySellOrdersBuffer.size - 1) {
        return
      }
    }
  }
}

  def buyOperation(order: Order, buySellOrdersBuffer:mutable.Buffer[Order], clientId:Int): Unit = {
    for(i <- buySellOrdersBuffer.indices) {

      if(i >= buySellOrdersBuffer.size)
        return

      var candidateOrder = buySellOrdersBuffer(i)
      if(!order.name.equals(candidateOrder.name) && !order.operation.equals(candidateOrder.operation) &&
        order.stockName.equals(candidateOrder.stockName) &&
        order.priceStock.compareTo(candidateOrder.priceStock) != -1) {

        var candidateId = checkOwnMoney(clientsWithOrdersBuffer, clientId, order, candidateOrder)
        if(candidateId != 0) {
          buyStock(clientsWithOrdersBuffer, order, clientId, candidateOrder, candidateId)
        }
      }
    }
  }

  def buyStock(clientsWithOrdersBuffer:mutable.Buffer[ClientWithOrdersBuffer], buyerOrder:Order, buyerId:Int,
               candidateOrder:Order, candidateId:Int): Unit = {

    var candidate = clientsWithOrdersBuffer(candidateId)
    var buyer = clientsWithOrdersBuffer(buyerId)
    for(i <- 0 until buyerOrder.amountStock) {
      var amountToBuy = 0
      if(buyer.money < (candidateOrder.priceStock * i)) {
        if(buyer.money < (candidateOrder.priceStock * 1)) {
          return
        }

        amountToBuy = i - 1
        changeWhenCantBuyAll(clientsWithOrdersBuffer(buyerId),buyerOrder,candidate,candidateOrder, amountToBuy)
        return
      }
    }
    if(buyerOrder.amountStock == candidateOrder.amountStock){
      buyWhenStockEqual(clientsWithOrdersBuffer(buyerId),buyerOrder,candidate,candidateOrder)
    }else if(buyerOrder.amountStock < candidateOrder.amountStock){
      changeIfWantBuyLess(clientsWithOrdersBuffer(buyerId),buyerOrder,candidate,candidateOrder)
    }else{
      changeIfWantBuyMore(clientsWithOrdersBuffer(buyerId),buyerOrder,candidate,candidateOrder)
    }
  }

  def changeIfWantBuyMore(buyer:ClientWithOrdersBuffer, buyerOrder:Order,
                           candidate:ClientWithOrdersBuffer, candidateOrder:Order): Unit = {

    buyer.money -= candidateOrder.amountStock * candidateOrder.priceStock
    candidate.money += candidateOrder.amountStock * candidateOrder.priceStock

    if(buyer.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

    if(buyerOrder.stockName == 'A') {
      buyer.A += candidateOrder.amountStock
      candidate.A -= candidateOrder.amountStock
    }else if(buyerOrder.stockName == 'B') {
      buyer.B += candidateOrder.amountStock
      candidate.B -= candidateOrder.amountStock
    }else if(buyerOrder.stockName == 'C') {
      buyer.C += candidateOrder.amountStock
      candidate.C -= candidateOrder.amountStock
    }else {
      buyer.D += candidateOrder.amountStock
      candidate.D -= candidateOrder.amountStock
    }
    buyerOrder.amountStock -= candidateOrder.amountStock
    for(i <- buySellOrdersBuffer.indices) {
      if(buySellOrdersBuffer(i) == candidateOrder){
        buySellOrdersBuffer.remove(i)
        return
      }
    }
  }

  def changeIfWantBuyLess(buyer:ClientWithOrdersBuffer, buyerOrder:Order,
                           candidate:ClientWithOrdersBuffer, candidateOrder:Order): Unit = {

    buyer.money -= buyerOrder.amountStock * candidateOrder.priceStock
    candidate.money += buyerOrder.amountStock * candidateOrder.priceStock

    if(buyer.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

    if(buyerOrder.stockName == 'A') {
      buyer.A += buyerOrder.amountStock
      candidate.A -= buyerOrder.amountStock
    }else if(buyerOrder.stockName == 'B') {
      buyer.B += buyerOrder.amountStock
      candidate.B -= buyerOrder.amountStock
    }else if(buyerOrder.stockName == 'C') {
      buyer.C += buyerOrder.amountStock
      candidate.C -= buyerOrder.amountStock
    }else {
      buyer.D += buyerOrder.amountStock
      candidate.D -= buyerOrder.amountStock
    }
    candidateOrder.amountStock -= buyerOrder.amountStock
    for(i <- buySellOrdersBuffer.indices) {
      if(buySellOrdersBuffer(i) == buyerOrder){
        buySellOrdersBuffer.remove(i)
        return
      }
    }
  }

  def buyWhenStockEqual(buyer:ClientWithOrdersBuffer, buyerOrder:Order,
                           candidate:ClientWithOrdersBuffer, candidateOrder:Order): Unit = {

    buyer.money -= candidateOrder.amountStock * candidateOrder.priceStock
    candidate.money += candidateOrder.amountStock * candidateOrder.priceStock

    if(buyer.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

    if(buyerOrder.stockName == 'A') {
      buyer.A += candidateOrder.amountStock
      candidate.A -= candidateOrder.amountStock
    }else if(buyerOrder.stockName == 'B') {
      buyer.B += candidateOrder.amountStock
      candidate.B -= candidateOrder.amountStock
    }else if(buyerOrder.stockName == 'C') {
      buyer.C += candidateOrder.amountStock
      candidate.C -= candidateOrder.amountStock
    }else {
      buyer.D += candidateOrder.amountStock
      candidate.D -= candidateOrder.amountStock
    }
    var returnCounter = 0
    var oneId = 0
    var twoId = 0
    for(i <- buySellOrdersBuffer.indices) {
      if(buySellOrdersBuffer(i) == buyerOrder){
//        buySellOrdersBuffer.remove(i)
        returnCounter += 1
        oneId = i
      }
      if(returnCounter == 2) {
        if(oneId > twoId){
          buySellOrdersBuffer.remove(twoId)
          buySellOrdersBuffer.remove(oneId - 1)
        }else{
          buySellOrdersBuffer.remove(oneId)
          buySellOrdersBuffer.remove(twoId - 1)
        }
        return
      }

      if(buySellOrdersBuffer(i) == candidateOrder) {
        returnCounter += 1
        twoId = i
      }
      if(returnCounter == 2) {
        if(oneId > twoId){
          buySellOrdersBuffer.remove(twoId)
          buySellOrdersBuffer.remove(oneId - 1)
        }else{
          buySellOrdersBuffer.remove(oneId)
          buySellOrdersBuffer.remove(twoId - 1)
        }
        return
      }
    }
  }

  def changeWhenCantBuyAll(buyer:ClientWithOrdersBuffer, buyerOrder:Order,
                           candidate:ClientWithOrdersBuffer, candidateOrder:Order, amountToBuy:Int): Unit = {

    buyer.money -= amountToBuy * candidateOrder.priceStock
    candidate.money += amountToBuy * candidateOrder.priceStock

    if(buyer.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

    if(buyerOrder.stockName == 'A') {
      buyer.A += amountToBuy
      candidate.A -= amountToBuy
    }else if(buyerOrder.stockName == 'B') {
      buyer.B += amountToBuy
      candidate.B -= amountToBuy
    }else if(buyerOrder.stockName == 'C') {
      buyer.C += amountToBuy
      candidate.C -= amountToBuy
    }else {
      buyer.D += amountToBuy
      candidate.D -= amountToBuy
    }

    buyerOrder.amountStock -= amountToBuy
    candidateOrder.amountStock -= amountToBuy

  }

  def sellOperation(order: Order, buySellOrdersBuffer:mutable.Buffer[Order], sellerId:Int): Unit = {
    for(i <- buySellOrdersBuffer.indices) {

      if(i >= buySellOrdersBuffer.size)
        return

      var candidateOrder = buySellOrdersBuffer(i)
      if(!order.name.equals(candidateOrder.name) && !order.operation.equals(candidateOrder.operation) &&
          order.stockName.equals(candidateOrder.stockName) &&
          order.priceStock.compareTo(candidateOrder.priceStock) != 1) {

        var candidateId = checkMoneyCandidate(clientsWithOrdersBuffer, candidateOrder.name, candidateOrder, order)
        if(candidateId != 0) {
          sellStock(clientsWithOrdersBuffer, order, sellerId, candidateOrder,candidateId)
        }
      }
    }
  }

  def checkMoneyCandidate(clientsWithOrdersBuffer:mutable.Buffer[ClientWithOrdersBuffer], candidateName:String,
                          candidateOrder:Order, order: Order):Int = {
    var result = 0
    for(i <- clientsWithOrdersBuffer.indices) {
      if(clientsWithOrdersBuffer(i).name.equals(candidateName)) {
        if(clientsWithOrdersBuffer(i).money.compareTo(candidateOrder.priceStock) != -1) {
          return i
        }else{
          return 0
        }
      }
    }
    result
  }

  def checkOwnMoney(clientsWithOrdersBuffer:mutable.Buffer[ClientWithOrdersBuffer], clientId:Int, order: Order,
                    candidateOrder:Order):Int = {
    var result = 0
        if(clientsWithOrdersBuffer(clientId).money.compareTo(candidateOrder.priceStock) != -1) {
          result = clientId
        }else{
          result = 0
        }
    result
  }

  def sellStock(clientsWithOrdersBuffer:mutable.Buffer[ClientWithOrdersBuffer], sellerOrder:Order, sellerId:Int,
                candidateOrder:Order, candidateId:Int): Unit = {
    //Test
    var candidate = clientsWithOrdersBuffer(candidateId)
//    for(i <- 0 until sellerOrder.amountStock) {
    var numFrom = 1
    var numTo = sellerOrder.amountStock
    var numMediator = 1
      for(i <- numFrom to (numTo, numMediator)) {
      var amountToSell = 0
      if(candidate.money < (candidateOrder.priceStock * i)) {
        if(candidate.money < (candidateOrder.priceStock * 1)) {
          return
        }

        amountToSell = i - 1
        changeWhenCantSellAll(clientsWithOrdersBuffer(sellerId),sellerOrder,candidate,candidateOrder, amountToSell)
        return
      }
    }
    //Test
//    var candidate = clientsWithOrdersBuffer(candidateId)
//    for(i <- 1 until sellerOrder.amountStock) {
//      var amountToSell = 0
//      if(candidate.money < (candidateOrder.priceStock * i)) {
//        if(candidate.money < (candidateOrder.priceStock * 1)){
//          return
//        }
//          amountToSell = i - 1
//          changeWhenCantSellAll(clientsWithOrdersBuffer(sellerId),sellerOrder,candidate,candidateOrder, amountToSell)
//        return
//      }
//    }
    //Test
    if(candidate.money < (candidateOrder.priceStock * 1)) {
      println("<MF")
    }
    //Test
    if(sellerOrder.amountStock == candidateOrder.amountStock){
      sellWhenStockEqual(clientsWithOrdersBuffer(sellerId),sellerOrder,candidate,candidateOrder)
    }else if(sellerOrder.amountStock < candidateOrder.amountStock){
      changeIfWantSellLess(clientsWithOrdersBuffer(sellerId),sellerOrder,candidate,candidateOrder)
    }else{
      changeIfWantSellMore(clientsWithOrdersBuffer(sellerId),sellerOrder,candidate,candidateOrder)
    }
  }

  def changeIfWantSellMore(seller:ClientWithOrdersBuffer, sellerOrder:Order,
                           candidate:ClientWithOrdersBuffer, candidateOrder:Order): Unit = {

    seller.money += candidateOrder.amountStock * candidateOrder.priceStock
    candidate.money -= candidateOrder.amountStock * candidateOrder.priceStock

    if(candidate.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

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
    sellerOrder.amountStock -= candidateOrder.amountStock
    for(i <- buySellOrdersBuffer.indices) {
      if(buySellOrdersBuffer(i) == candidateOrder){
        buySellOrdersBuffer.remove(i)
        returnCounter += 1
      }
      if(returnCounter == 1)
        return
    }
  }

  def changeIfWantSellLess(seller:ClientWithOrdersBuffer, sellerOrder:Order,
                           candidate:ClientWithOrdersBuffer, candidateOrder:Order): Unit = {

    seller.money += sellerOrder.amountStock * candidateOrder.priceStock
    candidate.money -= sellerOrder.amountStock * candidateOrder.priceStock

    if(candidate.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

    if(sellerOrder.stockName == 'A') {
      seller.A -= sellerOrder.amountStock
      candidate.A += sellerOrder.amountStock
    }else if(sellerOrder.stockName == 'B') {
      seller.B -= sellerOrder.amountStock
      candidate.B += sellerOrder.amountStock
    }else if(sellerOrder.stockName == 'C') {
      seller.C -= sellerOrder.amountStock
      candidate.C += sellerOrder.amountStock
    }else {
      seller.D -= sellerOrder.amountStock
      candidate.D += sellerOrder.amountStock
    }
    var returnCounter = 0
    candidateOrder.amountStock -= sellerOrder.amountStock
    for(i <- buySellOrdersBuffer.indices) {
      if(buySellOrdersBuffer(i) == sellerOrder){
        buySellOrdersBuffer.remove(i)
        returnCounter += 1
      }
      if(returnCounter == 1)
        return
    }
  }

  def changeWhenCantSellAll(seller:ClientWithOrdersBuffer, sellerOrder:Order,
                           candidate:ClientWithOrdersBuffer, candidateOrder:Order, amountToSell:Int): Unit = {

    seller.money += amountToSell * candidateOrder.priceStock
    candidate.money -= amountToSell * candidateOrder.priceStock

    if(candidate.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

    if(sellerOrder.stockName == 'A') {
      seller.A -= amountToSell
      candidate.A += amountToSell
    }else if(sellerOrder.stockName == 'B') {
      seller.B -= amountToSell
      candidate.B += amountToSell
    }else if(sellerOrder.stockName == 'C') {
      seller.C -= amountToSell
      candidate.C += amountToSell
    }else {
      seller.D -= amountToSell
      candidate.D += amountToSell
    }

    sellerOrder.amountStock -= amountToSell
    candidateOrder.amountStock -= amountToSell
  }

  def sellWhenStockEqual(seller:ClientWithOrdersBuffer, sellerOrder:Order,
                           candidate:ClientWithOrdersBuffer, candidateOrder:Order): Unit = {

    seller.money += candidateOrder.amountStock * candidateOrder.priceStock
    candidate.money -= candidateOrder.amountStock * candidateOrder.priceStock

    if(candidate.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

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
    var oneId = 0
    var twoId = 0
    for(i <- buySellOrdersBuffer.indices) {

      if(buySellOrdersBuffer(i) == sellerOrder){
        returnCounter += 1
        oneId = i
      }
      if(returnCounter == 2) {
        if(oneId > twoId){
          buySellOrdersBuffer.remove(twoId)
          buySellOrdersBuffer.remove(oneId - 1)
        }else{
          buySellOrdersBuffer.remove(oneId)
          buySellOrdersBuffer.remove(twoId - 1)
        }
        return
      }

      if(buySellOrdersBuffer(i) == candidateOrder) {
        returnCounter += 1
        twoId = i
      }
      if(returnCounter == 2) {
        if(oneId > twoId){
          buySellOrdersBuffer.remove(twoId)
          buySellOrdersBuffer.remove(oneId - 1)
        }else{
          buySellOrdersBuffer.remove(oneId)
          buySellOrdersBuffer.remove(twoId - 1)
        }
        return
      }
    }
  }

  println("End\n")
}
