# 🌾 Farm Supply Chain Tracker

A blockchain-based solution for transparent agricultural supply chain management.

## 🎯 Features

- 🌱 Create and track farm produce batches from seed to store
- 📝 Record multiple stages in the supply chain
- 🔄 Transfer batch ownership
- 📊 View complete batch history and details

## 🚀 Usage

### Creating a Batch
```clarity
(contract-call? .farm-supply-chain-tracker create-batch "FARM123" "Tomatoes" u1000 u1234)
```

### Recording a Stage
```clarity
(contract-call? .farm-supply-chain-tracker record-stage u1 "harvested" tx-sender "WAREHOUSE1" "Quality check passed")
```

### Viewing Batch Details
```clarity
(contract-call? .farm-supply-chain-tracker get-batch-details u1)
```

### Transferring Ownership
```clarity
(contract-call? .farm-supply-chain-tracker transfer-batch u1 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

## 🔒 Security

- Only batch owners can transfer ownership
- Immutable stage history
- Verified handler tracking

## 🤝 Contributing

Pull requests are welcome! For major changes, please open an issue first.
```
