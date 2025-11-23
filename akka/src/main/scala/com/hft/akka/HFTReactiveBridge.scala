package com.hft.akka

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}

/**
 * The Reactive Bridge - Akka Actor System for HFT
 * 
 * This provides the reactive, message-driven foundation for the HFT system
 * using the Actor model for concurrent, distributed trading operations.
 */
object HFTReactiveBridge {

  // Order domain model
  sealed trait OrderSide
  case object Buy extends OrderSide
  case object Sell extends OrderSide

  case class Order(
    orderId: Long,
    symbol: String,
    price: BigDecimal,
    quantity: Long,
    side: OrderSide
  )

  case class Trade(
    tradeId: Long,
    buyOrder: Order,
    sellOrder: Order,
    executionPrice: BigDecimal,
    quantity: Long
  )

  // Actor Messages
  sealed trait OrderMessage
  case class SubmitOrder(order: Order, replyTo: ActorRef[OrderResponse]) extends OrderMessage
  case class MatchOrders(orders: Seq[Order]) extends OrderMessage
  
  sealed trait OrderResponse
  case class OrderAccepted(orderId: Long) extends OrderResponse
  case class OrderRejected(orderId: Long, reason: String) extends OrderResponse
  case class OrderMatched(trade: Trade) extends OrderResponse

  // Order Book Actor
  object OrderBookActor {
    def apply(): Behavior[OrderMessage] = Behaviors.setup { context =>
      new OrderBookActor(context)
    }
  }

  class OrderBookActor(context: ActorContext[OrderMessage]) 
    extends AbstractBehavior[OrderMessage](context) {

    private var buyOrders: List[Order] = List.empty
    private var sellOrders: List[Order] = List.empty
    private var tradeIdCounter: Long = 1

    override def onMessage(msg: OrderMessage): Behavior[OrderMessage] = {
      msg match {
        case SubmitOrder(order, replyTo) =>
          if (validateOrder(order)) {
            order.side match {
              case Buy =>
                buyOrders = (order :: buyOrders).sortBy(-_.price.toDouble)
              case Sell =>
                sellOrders = (order :: sellOrders).sortBy(_.price.toDouble)
            }
            replyTo ! OrderAccepted(order.orderId)
            context.log.info(s"Order accepted: ${order.orderId} ${order.side} ${order.symbol} @ ${order.price}")
            
            // Try to match orders
            matchOrders()
          } else {
            replyTo ! OrderRejected(order.orderId, "Invalid order parameters")
          }
          this

        case MatchOrders(orders) =>
          orders.foreach { order =>
            order.side match {
              case Buy => buyOrders = order :: buyOrders
              case Sell => sellOrders = order :: sellOrders
            }
          }
          matchOrders()
          this
      }
    }

    private def validateOrder(order: Order): Boolean = {
      order.price > 0 && order.quantity > 0 && order.symbol.nonEmpty
    }

    private def matchOrders(): Unit = {
      buyOrders.headOption.foreach { buyOrder =>
        sellOrders.headOption.foreach { sellOrder =>
          if (buyOrder.symbol == sellOrder.symbol && 
              buyOrder.price >= sellOrder.price) {
            
            val executionPrice = sellOrder.price
            val executionQty = Math.min(buyOrder.quantity, sellOrder.quantity)
            
            val trade = Trade(
              tradeIdCounter,
              buyOrder,
              sellOrder,
              executionPrice,
              executionQty
            )
            
            context.log.info(s"Trade executed: ${trade.tradeId} ${buyOrder.symbol} " +
              s"${executionQty}@${executionPrice}")
            
            tradeIdCounter += 1
            
            // Remove matched orders or update quantities
            buyOrders = buyOrders.tail
            sellOrders = sellOrders.tail
            
            // Continue matching if more orders exist
            if (buyOrders.nonEmpty && sellOrders.nonEmpty) {
              matchOrders()
            }
          }
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val system: ActorSystem[OrderMessage] = 
      ActorSystem(OrderBookActor(), "HFT-Reactive-System")

    println("=== Akka Reactive Bridge Started ===")
    println("🌉 Actor system initialized")
    println("📊 Order book ready for trading")
    println("⚡ High-frequency processing enabled")
    
    // Keep the system running
    Thread.sleep(2000)
    
    println("=== Reactive Bridge Running ===")
    
    // Graceful shutdown hook
    sys.addShutdownHook {
      system.terminate()
      println("=== Reactive Bridge Shutdown ===")
    }
  }
}
