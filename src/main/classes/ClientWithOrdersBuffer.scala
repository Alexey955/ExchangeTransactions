package classes

import scala.collection.mutable

class ClientWithOrdersBuffer(client: Client, var ownOrdersBuffer:mutable.Buffer[Order]) extends Client(client.getName,
  client.getMoney, client.getA, client.getB, client.getC, client.getD) {


}

object ClientWithOrdersBuffer{

  def createClientsWithOrdersBuffer(ordersBuffer:mutable.Buffer[Order],
                                    clientsBuffer:mutable.Buffer[Client]):mutable.Buffer[ClientWithOrdersBuffer] = {

    val clientsWithOrdersBuffer:mutable.Buffer[ClientWithOrdersBuffer] = mutable.Buffer()

    for(i <- clientsBuffer.indices) {
      val ownOrdersBuffer = createOwnOrderBuffer(clientsBuffer(i), ordersBuffer)
      val clientsWithOrders = new ClientWithOrdersBuffer(clientsBuffer(i), ownOrdersBuffer)

      clientsWithOrdersBuffer.append(clientsWithOrders)
    }
    clientsWithOrdersBuffer
  }

  def createOwnOrderBuffer(client: Client, ordersBuffer:mutable.Buffer[Order]):mutable.Buffer[Order] = {

    val ownOrdersBuffer:mutable.Buffer[Order] = mutable.Buffer()

    for(i <- ordersBuffer.indices) {
      if(client.getName.equals(ordersBuffer(i).getName))
        ownOrdersBuffer.append(ordersBuffer(i))
    }
    ownOrdersBuffer
  }
}