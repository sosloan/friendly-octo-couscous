#if canImport(SwiftUI)
import SwiftUI

/// Order Entry View for submitting new orders
/// Follows HIG with proper form design and validation
public struct OrderEntryView: View {
    @ObservedObject var engine: ReactiveTradeEngine
    
    @State private var symbol: String = "AAPL"
    @State private var priceString: String = ""
    @State private var quantityString: String = ""
    @State private var selectedSide: OrderSide = .buy
    @State private var showingAlert = false
    @State private var alertMessage = ""
    @State private var orderIdCounter: Int64 = 1000
    
    public init(engine: ReactiveTradeEngine) {
        self.engine = engine
    }
    
    public var body: some View {
        ScrollView {
            VStack(spacing: 24) {
                // Header
                header
                
                // Order form
                orderForm
                
                // Submit button
                submitButton
                
                Spacer()
            }
            .padding()
        }
        .alert("Order Status", isPresented: $showingAlert) {
            Button("OK", role: .cancel) { }
        } message: {
            Text(alertMessage)
        }
    }
    
    // MARK: - Header
    
    private var header: some View {
        VStack(alignment: .leading, spacing: 8) {
            Text("NEW ORDER")
                .font(.headline)
            
            Text("Enter order details below")
                .font(.caption)
                .foregroundColor(.secondary)
        }
        .frame(maxWidth: .infinity, alignment: .leading)
    }
    
    // MARK: - Order Form
    
    private var orderForm: some View {
        VStack(spacing: 20) {
            // Symbol field
            FormField(
                label: "Symbol",
                icon: "chart.line.uptrend.xyaxis",
                content: {
                    TextField("AAPL", text: $symbol)
                        .textFieldStyle(.roundedBorder)
                        .autocapitalization(.allCharacters)
                }
            )
            
            // Price field
            FormField(
                label: "Price",
                icon: "dollarsign.circle",
                content: {
                    TextField("0.00", text: $priceString)
                        .textFieldStyle(.roundedBorder)
                        .keyboardType(.decimalPad)
                }
            )
            
            // Quantity field
            FormField(
                label: "Quantity",
                icon: "number.circle",
                content: {
                    TextField("100", text: $quantityString)
                        .textFieldStyle(.roundedBorder)
                        .keyboardType(.numberPad)
                }
            )
            
            // Side picker
            FormField(
                label: "Side",
                icon: "arrow.left.arrow.right.circle",
                content: {
                    Picker("Order Side", selection: $selectedSide) {
                        Text("Buy").tag(OrderSide.buy)
                        Text("Sell").tag(OrderSide.sell)
                    }
                    .pickerStyle(.segmented)
                }
            )
            
            // Order preview
            if let order = previewOrder {
                orderPreview(order)
            }
        }
        .padding()
        .background(
            RoundedRectangle(cornerRadius: 16)
                .fill(Color(.secondarySystemBackground))
        )
    }
    
    // MARK: - Submit Button
    
    private var submitButton: some View {
        Button(action: submitOrder) {
            Label("Submit Order", systemImage: "paperplane.fill")
                .frame(maxWidth: .infinity)
                .padding()
                .background(
                    RoundedRectangle(cornerRadius: 12)
                        .fill(selectedSide == .buy ? Color.green : Color.red)
                )
                .foregroundColor(.white)
                .font(.headline)
        }
        .disabled(!isValidOrder)
        .opacity(isValidOrder ? 1.0 : 0.5)
    }
    
    // MARK: - Order Preview
    
    private func orderPreview(_ order: Order) -> some View {
        VStack(spacing: 12) {
            Text("ORDER PREVIEW")
                .font(.caption.weight(.semibold))
                .foregroundColor(.secondary)
            
            Divider()
            
            VStack(spacing: 8) {
                PreviewRow(label: "Symbol", value: order.symbol)
                PreviewRow(label: "Side", value: order.side.rawValue)
                PreviewRow(label: "Price", value: formatPrice(order.price))
                PreviewRow(label: "Quantity", value: "\(order.quantity)")
                PreviewRow(label: "Total Value", value: formatPrice(order.value), emphasized: true)
            }
        }
        .padding()
        .background(
            RoundedRectangle(cornerRadius: 12)
                .fill(Color(.tertiarySystemBackground))
        )
    }
    
    // MARK: - Helper Views
    
    private var previewOrder: Order? {
        guard let price = Decimal(string: priceString),
              let quantity = Int64(quantityString),
              !symbol.isEmpty else {
            return nil
        }
        
        return Order(
            id: orderIdCounter,
            symbol: symbol.uppercased(),
            price: price,
            quantity: quantity,
            side: selectedSide
        )
    }
    
    private var isValidOrder: Bool {
        guard let price = Decimal(string: priceString),
              let quantity = Int64(quantityString),
              !symbol.isEmpty else {
            return false
        }
        
        return price > 0 && quantity > 0
    }
    
    // MARK: - Actions
    
    private func submitOrder() {
        guard let order = previewOrder else { return }
        
        engine.submitOrder(order)
        
        alertMessage = "Order #\(order.id) submitted successfully!\n\n\(order.symbol) \(order.side.rawValue) \(order.quantity) @ \(formatPrice(order.price))"
        showingAlert = true
        
        // Increment order ID and reset form
        orderIdCounter += 1
        priceString = ""
        quantityString = ""
    }
    
    // MARK: - Formatting
    
    private func formatPrice(_ price: Decimal) -> String {
        let formatter = NumberFormatter()
        formatter.numberStyle = .currency
        formatter.currencySymbol = "$"
        formatter.minimumFractionDigits = 2
        formatter.maximumFractionDigits = 2
        return formatter.string(from: price as NSDecimalNumber) ?? "$0.00"
    }
}

// MARK: - Form Field

struct FormField<Content: View>: View {
    let label: String
    let icon: String
    let content: () -> Content
    
    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            Label {
                Text(label)
                    .font(.caption.weight(.semibold))
            } icon: {
                Image(systemName: icon)
            }
            .foregroundColor(.secondary)
            
            content()
        }
    }
}

// MARK: - Preview Row

struct PreviewRow: View {
    let label: String
    let value: String
    var emphasized: Bool = false
    
    var body: some View {
        HStack {
            Text(label)
                .font(.caption)
                .foregroundColor(.secondary)
            
            Spacer()
            
            Text(value)
                .font(emphasized ? .callout.weight(.bold) : .caption.weight(.medium))
                .foregroundColor(emphasized ? .orange : .primary)
        }
    }
}

// MARK: - Preview

#Preview {
    let engine = ReactiveTradeEngine()
    return OrderEntryView(engine: engine)
}
#endif
