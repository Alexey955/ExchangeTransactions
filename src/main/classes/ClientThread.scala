package main.classes
import collection.mutable

class ClientThread(val client: Client) extends Thread(new Runnable {
  override def run(): Unit = {
//    println("My name is " + client.name + ", and I have " + client.money + " dollars.")
    def findOrder(client: Client, ordersBuffer:mutable.Buffer[Order]): Unit = {
      for(i <- ordersBuffer.indices) {
        if(client.name.equals(ordersBuffer(i).name)) {
          var order = ordersBuffer(i)
          println("My name is " + client.name + " and my order is " + order)
          return
        }
      }
    }
  }
}){

}
object ClientThread{
  val clientsThreadList:mutable.Buffer[ClientThread] = mutable.Buffer()
  def createClientThread(clientsList:mutable.Buffer[Client], amount:Int): mutable.Buffer[ClientThread]= {
    for(i <- 0 until amount) {
      var clientThread = new ClientThread(clientsList(i))
      clientsThreadList.append(clientThread)
      clientThread.start()
    }
    clientsThreadList
  }
}