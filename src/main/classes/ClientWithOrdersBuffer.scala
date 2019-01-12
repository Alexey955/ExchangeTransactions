package main.classes

import scala.collection.mutable

class ClientWithOrdersBuffer(var client: Client, var ownOrdersBuffer:mutable.Buffer[Order]) extends Client(client.name,
  client.money, client.A, client.B, client.C, client.D) {

}

object ClientWithOrdersBuffer{

  def createClientsWithOrdersBuffer(ordersBuffer:mutable.Buffer[Order],
                                    clientsBuffer:mutable.Buffer[Client]):mutable.Buffer[ClientWithOrdersBuffer] = {

    val clientsWithOrdersBuffer:mutable.Buffer[ClientWithOrdersBuffer] = mutable.Buffer()

    for(i <- clientsBuffer.indices) {
      var ownOrdersBuffer = createOwnOrderBuffer(clientsBuffer(i), ordersBuffer)
      var clientsWithOrders = new ClientWithOrdersBuffer(clientsBuffer(i), ownOrdersBuffer)

      clientsWithOrdersBuffer.append(clientsWithOrders)
    }
    clientsWithOrdersBuffer
  }

  def createOwnOrderBuffer(client: Client, ordersBuffer:mutable.Buffer[Order]):mutable.Buffer[Order] = {

    val ownOrdersBuffer:mutable.Buffer[Order] = mutable.Buffer()

    for(i <- ordersBuffer.indices) {
      if(client.name.equals(ordersBuffer(i).name))
        ownOrdersBuffer.append(ordersBuffer(i))
    }
    ownOrdersBuffer
  }
}