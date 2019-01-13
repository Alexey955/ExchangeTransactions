package classes

object Application extends App{

  private val ordersPath = "src/main/resources/orders.txt"
  private val clientsPath = "src/main/resources/clients.txt"

  val stockExchange = new StockExchange(clientsPath, ordersPath)
  stockExchange.calculateClientsBalance()
}
