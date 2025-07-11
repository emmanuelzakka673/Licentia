# 📜 Licentia - NFT Licensing Contract

> 🎨 Attach license metadata to NFTs and monetize digital assets through flexible licensing agreements

## 🌟 Overview

Licentia is a Clarity smart contract that enables NFT creators to attach licensing terms to their digital assets. Users can purchase licenses to use NFTs under specific conditions, creating new revenue streams for creators while protecting intellectual property rights.

## ✨ Features

- 🏷️ **License Creation** - Attach custom licenses to any NFT
- 💰 **Monetization** - Set prices and earn from license sales
- ⏰ **Time-based Licensing** - Set expiration dates for licenses
- 📊 **Usage Tracking** - Monitor license usage and limits
- 🎯 **License Templates** - Pre-defined licensing terms
- 🔧 **Flexible Terms** - Custom licensing conditions
- 💸 **Revenue Sharing** - Platform fees and creator earnings

## 🚀 Quick Start

### Creating a License

```clarity
(contract-call? .licentia create-license 
  'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7.my-nft  ;; NFT contract
  u1                                                      ;; Token ID
  "commercial"                                           ;; License type
  u"Can be used for commercial purposes"                 ;; Terms
  u1000000                                              ;; Price (1 STX)
  u144                                                  ;; Duration (blocks)
  u100)                                                 ;; Max usage
```

### Purchasing a License

```clarity
(contract-call? .licentia purchase-license u1)
```

### Using a License

```clarity
(contract-call? .licentia use-license u1)
```

## 📋 Core Functions

### 🔨 Public Functions

| Function | Description |
|----------|-------------|
| `create-license` | Create a new license for an NFT |
| `purchase-license` | Buy a license to use an NFT |
| `use-license` | Record usage of a purchased license |
| `revoke-license` | Deactivate a license (creator only) |
| `update-license-price` | Change license pricing |
| `create-license-template` | Create reusable license templates |

### 👀 Read-Only Functions

| Function | Description |
|----------|-------------|
| `get-license` | Get license details |
| `get-license-holder` | Get holder information |
| `get-nft-licenses` | List all licenses for an NFT |
| `is-license-valid` | Check if license is still valid |
| `can-use-license` | Check if license can be used |
| `get-creator-earnings` | View creator earnings |

## 💡 Usage Examples

### 🎨 For NFT Creators

1. **Create a Commercial License**
```clarity
(contract-call? .licentia create-license 
  'SP...my-nft u1 "commercial" 
  u"Full commercial rights for 30 days" 
  u5000000 u4320 u50)
```

2. **Update Pricing**
```clarity
(contract-call? .licentia update-license-price u1 u3000000)
```

### 🛒 For License Buyers

1. **Purchase License**
```clarity
(contract-call? .licentia purchase-license u1)
```

2. **Check License Status**
```clarity
(contract-call? .licentia is-license-valid u1 tx-sender)
```

3. **Use License**
```clarity
(contract-call? .licentia use-license u1)
```

## 🏗️ License Types

- 📝 **Personal** - Non-commercial use only
- 💼 **Commercial** - Business and commercial use
- 🎓 **Educational** - Academic and educational purposes
- 🔄 **Resale** - Rights to resell or redistribute
- 🎨 **Derivative** - Create derivative works

## 💰 Economics

- **Platform Fee**: 5% (50/1000) by default
- **Creator Earnings**: 95% of license sales
- **Pricing**: Set by license creators
- **Duration**: Measured in Stacks blocks

## 🔒 Security Features

- ✅ Owner verification for license creation
- ✅ Payment validation before license issuance
- ✅ Expiration checking for license usage
- ✅ Usage limits enforcement
- ✅ Authorization checks for modifications

## 🛠️ Development

### Prerequisites
- Clarinet CLI
- Stacks blockchain knowledge
- Clarity smart contract experience

### Testing
```bash
clarinet test
```

### Deployment
```bash
clarinet deploy
```

## 📊 Error Codes

| Code | Description |
|------|-------------|
| `u100` | Not authorized |
| `u101` | License not found |
| `u102` | Already exists |
| `u103` | Invalid license |
| `u104` | License expired |
| `u105` | Insufficient payment |

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License.

---


