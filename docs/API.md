# HFT System API Reference

## Ada API

### HFT_Engine Package

```ada
package HFT_Engine is
   type Price is delta 0.01 digits 15;
   type Quantity is range 0 .. 1_000_000_000;
   type Side is (Buy, Sell);
   
   type Order is record
      Order_ID   : Positive;
      Symbol     : String (1 .. 10);
      Price_Val  : Price;
      Qty        : Quantity;
      Order_Side : Side;
      Timestamp  : Ada.Real_Time.Time;
   end record;
   
   function Is_Valid_Order (O : Order) return Boolean;
   function Calculate_Value (O : Order) return Price;
   function Can_Match (Buy_Order : Order; Sell_Order : Order) return Boolean;
end HFT_Engine;
```

#### Functions

**`Is_Valid_Order(O : Order) : Boolean`**
- Validates that an order meets all constraints
- Postcondition: If true, then Qty > 0 and Price_Val > 0

**`Calculate_Value(O : Order) : Price`**
- Calculates the total value of an order
- Precondition: Order must be valid
- Postcondition: Result >= 0.0

**`Can_Match(Buy_Order : Order, Sell_Order : Order) : Boolean`**
- Determines if two orders can be matched
- Precondition: Both orders must be valid
- Returns true if buy price >= sell price and symbols match

## Lean API

### HFT Module

```lean
namespace HFT
  def Price := { x : ℝ // x > 0 }
  def Quantity := ℕ
  inductive Side | buy | sell
  
  structure Order where
    orderId : ℕ
    symbol : String
    price : Price
    quantity : Quantity
    side : Side
  
  def orderValue (o : Order) : ℝ
  def canMatch (buy : Order) (sell : Order) : Prop
end HFT
```

#### Theorems

**`orderValue_nonneg`**
- Proves order value is always non-negative

**`match_implies_positive_value`**
- Proves matched trades have positive value

**`price_improvement`**
- Formalizes price improvement property

**`system_maintains_correctness`**
- Proves system maintains correctness over time

## Akka API

### HFTReactiveBridge

```scala
object HFTReactiveBridge {
  sealed trait OrderMessage
  case class SubmitOrder(order: Order, replyTo: ActorRef[OrderResponse]) extends OrderMessage
  case class MatchOrders(orders: Seq[Order]) extends OrderMessage
  
  sealed trait OrderResponse
  case class OrderAccepted(orderId: Long) extends OrderResponse
  case class OrderRejected(orderId: Long, reason: String) extends OrderResponse
  case class OrderMatched(trade: Trade) extends OrderResponse
  
  case class Order(
    orderId: Long,
    symbol: String,
    price: BigDecimal,
    quantity: Long,
    side: OrderSide
  )
}
```

#### Messages

**`SubmitOrder`**
- Submits a new order to the order book
- Receives OrderAccepted or OrderRejected response

**`MatchOrders`**
- Triggers order matching process
- Returns OrderMatched for successful matches

## Java API

### Order Record

```java
public record Order(
    long orderId,
    String symbol,
    BigDecimal price,
    long quantity,
    OrderSide side
)
```

#### Methods

**`calculateValue() : BigDecimal`**
- Returns the total value of the order

**`canMatchWith(Order other) : boolean`**
- Checks if this order can match with another

### NettyHFTServer

```java
public class NettyHFTServer {
    public NettyHFTServer(int port);
    public void start() throws Exception;
}
```

#### Usage

```java
NettyHFTServer server = new NettyHFTServer(8080);
server.start(); // Binds to port and starts accepting connections
```

## Erlang API

### Order Processor

```erlang
-module(hft_order_processor).
-export([submit_order/1, get_stats/0]).
```

**`submit_order(Order) -> {ok, accepted} | {error, invalid}`**
- Submits an order for processing
- Order is a map with keys: order_id, symbol, price, quantity, side

**`get_stats() -> #{processed => integer(), rejected => integer()}`**
- Returns processing statistics

### Match Engine

```erlang
-module(hft_match_engine).
-export([match_orders/2, get_trades/0]).
```

**`match_orders(BuyOrder, SellOrder) -> {ok, Trade} | {error, no_match}`**
- Attempts to match two orders
- Returns trade details on success

**`get_trades() -> [Trade]`**
- Returns list of all executed trades

### Risk Manager

```erlang
-module(hft_risk_manager).
-export([check_risk/1, get_exposure/0]).
```

**`check_risk(Order) -> {ok, approved} | {error, risk_limit}`**
- Checks if order passes risk limits
- Validates against maximum exposure

**`get_exposure() -> #{current => float(), max => float(), alerts => integer()}`**
- Returns current risk exposure metrics

## Integration APIs

### Java-Ada Bridge (JNI)

```java
public class AdaBridge {
    public native boolean validateOrder(long orderId, String symbol, 
                                       double price, long quantity, boolean isBuy);
    public native double calculateValue(double price, long quantity);
    public native boolean canMatch(double buyPrice, double sellPrice, 
                                   String buySymbol, String sellSymbol);
}
```

### Erlang-Java Integration

```erlang
-module(java_integration).
-export([call_java_order_service/1]).

call_java_order_service(Order) -> {ok, Result} | {error, Reason}.
```

## Message Formats

### Order Message (JSON)

```json
{
  "orderId": 12345,
  "symbol": "AAPL",
  "price": 150.50,
  "quantity": 100,
  "side": "BUY",
  "timestamp": 1234567890000
}
```

### Trade Message (JSON)

```json
{
  "tradeId": 1,
  "buyOrderId": 12345,
  "sellOrderId": 67890,
  "symbol": "AAPL",
  "price": 150.50,
  "quantity": 100,
  "timestamp": 1234567890000
}
```

## Error Codes

| Code | Description |
|------|-------------|
| `INVALID_ORDER` | Order parameters do not meet validation criteria |
| `INSUFFICIENT_RISK` | Order exceeds risk limits |
| `NO_MATCH` | Orders cannot be matched |
| `SYSTEM_ERROR` | Internal system error |
| `TIMEOUT` | Operation timed out |

## Performance Characteristics

| Operation | Latency | Throughput |
|-----------|---------|------------|
| Order Validation (Ada) | < 1 μs | 1M+ ops/sec |
| Order Matching (Erlang) | < 10 μs | 100K+ matches/sec |
| Network I/O (Netty) | < 100 μs | 10K+ msgs/sec |
| Actor Message (Akka) | < 1 ms | 1M+ msgs/sec |
