package main.classes

import scala.io.Source
import collection.mutable
object Application extends App{

  val clientsAmount = Source.fromFile("d:ExchangeTransactions/clients.txt").getLines().size
  val seqClients = Source.fromFile("d:ExchangeTransactions/clients.txt").getLines().toSeq
  val clientsBuffer = Client.createClients(seqClients, clientsAmount)

//  Source.fromFile("d:ExchangeTransactions/orders.txt").getLines().foreach(println)

  val ordersAmount = Source.fromFile("d:ExchangeTransactions/orders.txt").getLines().size
  val seqOrder = Source.fromFile("d:ExchangeTransactions/orders.txt").getLines().toSeq
  val ordersBuffer = Order.createOrdersList(seqOrder, ordersAmount)

  val buySellOrdersBuffer:mutable.Buffer[Order] = mutable.Buffer()

  val clientsThreadBuffer = ClientThread.createClientThread(clientsBuffer, clientsAmount, ordersBuffer, buySellOrdersBuffer)

//  println(buySellOrdersBuffer)

  clientsThreadBuffer(1).join()
  clientsThreadBuffer(2).join()
  clientsThreadBuffer(3).join()
  clientsThreadBuffer(4).join()
  clientsThreadBuffer(5).join()
  clientsThreadBuffer(6).join()
  clientsThreadBuffer(7).join()
  clientsThreadBuffer(8).join()

//  for(i <- buySellOrdersBuffer.indices) {
//    println(buySellOrdersBuffer(i).name)
//  }



  println("End\n")
//  Source.fromFile("d:ExchangeTransactions/orders.txt").getLines().foreach(println)

}
