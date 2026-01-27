# ORE XSD Schema Gaps Analysis

This document lists features present in ORE sample XML files that are missing or
incorrectly mapped in the XSD-generated domain code (`ores.ore/domain/domain.hpp`).

## Summary

| Category | Issue Count | Severity |
|----------|-------------|----------|
| Missing Elements | 5+ | High |
| Cardinality Mismatches | 2 | High |
| Missing Trade Types | 80+ | High |

---

## 1. Missing Elements

### 1.1 `envelope.NettingSetId` - MISSING

**Location in XSD:** `instruments.xsd`
```xml
<xs:element type="xs:string" name="NettingSetId" substitutionGroup="nettingSetGroup"/>
```

**Current generated struct:**
```cpp
struct envelope {
    xsd::optional<domain::envelope_CounterParty_t> CounterParty;
    xsd::optional<domain::envelope_PortfolioIds_t> PortfolioIds;
    xsd::optional<domain::envelope_AdditionalFields_t> AdditionalFields;
    // MISSING: NettingSetId
};
```

**Sample file using it:** `examples/ORE-API/Input/portfolio.xml`
```xml
<Envelope>
  <CounterParty>CPTY_A</CounterParty>
  <NettingSetId>CPTY_A</NettingSetId>
  <AdditionalFields/>
</Envelope>
```

**Impact:** All portfolio XML files that use NettingSetId fail to parse.

---

## 2. Cardinality Mismatches

### 2.1 `crossAssetModel_InterestRateModels_t.LGM` - Should be `xsd::vector`

**Location in XSD:** `simulation.xsd`
```xml
<xs:element type="lgm" name="LGM" maxOccurs="unbounded" minOccurs="1"/>
```

**Current generated struct:**
```cpp
struct crossAssetModel_InterestRateModels_t {
    xsd::optional<domain::lgm> LGM;  // WRONG: should be xsd::vector
    xsd::optional<domain::hw> HWModel;
};
```

**Sample file using multiple LGMs:** `examples/ORE-API/Input/simulation.xml`
```xml
<InterestRateModels>
  <LGM ccy="default">...</LGM>
  <LGM ccy="EUR">...</LGM>
  <LGM ccy="CHF">...</LGM>
  <LGM ccy="USD">...</LGM>
  ...
</InterestRateModels>
```

**Error message:** `Maximum occurrence of element 'LGM' is 1`

**Impact:** All simulation XML files with multiple currency LGM models fail to parse.

### 2.2 `lgm` attribute `ccy` - MISSING

The `LGM` element has a `ccy` attribute in XML samples but this attribute is not in the generated struct.

**Sample XML:**
```xml
<LGM ccy="EUR">
```

---

## 3. Missing Trade Data Types

The `instruments.xsd` defines a group `oreTradeData` with 80+ trade types, but the generated
`trade` struct only has basic fields:

**Current generated struct:**
```cpp
struct trade {
    xsd::string id;
    domain::oreTradeType TradeType;
    xsd::optional<domain::envelope> Envelope;
    xsd::optional<domain::tradeActions> TradeActions;
    // MISSING: All trade-specific data (SwapData, FxForwardData, FxOptionData, etc.)
};
```

**Missing trade data types from `instruments.xsd`:**

| Category | Trade Data Types |
|----------|-----------------|
| **Swaps** | SwapData, CrossCurrencySwapData, InflationSwapData, EquitySwapData, CallableSwapData, VarianceSwapData, CommoditySwapData, FlexiSwapData, BalanceGuaranteedSwapData |
| **FX** | FxForwardData, FxAverageForwardData, FxOptionData, FxBarrierOptionData, FxDoubleBarrierOptionData, FxDigitalOptionData, FxEuropeanBarrierOptionData, FxKIKOBarrierOptionData, FxDigitalBarrierOptionData, FxTouchOptionData, FxDoubleTouchOptionData, FxSwapData, FxAsianOptionData, FxAccumulatorData, FxWindowBarrierOptionData, FxBasketOptionData, FxGenericBarrierOptionData, FxRainbowOptionData, FxTaRFData, FxWorstOfBasketSwapData |
| **Equity** | EquityForwardData, EquityOptionData, EquityFutureOptionData, EquityBarrierOptionData, EquityDoubleBarrierOptionData, EquityEuropeanBarrierOptionData, EquityDigitalOptionData, EquityTouchOptionData, EquityDoubleTouchOptionData, EquityCliquetOptionData, EquitySwapData, EquityVarianceSwapData, EquityPositionData, EquityOptionPositionData, EquityAsianOptionData, EquityAccumulatorData, EquityWindowBarrierOptionData, EquityBasketOptionData, EquityGenericBarrierOptionData, EquityRainbowOptionData, EquityTaRFData, EquityWorstOfBasketSwapData, EquityBestEntryOptionData, EquityBasketVarianceSwapData, EquityOutperformanceOptionData, EquityStrikeResettableOptionData |
| **Commodities** | CommodityForwardData, CommodityOptionData, CommodityDigitalOptionData, CommodityDigitalAveragePriceOptionData, CommoditySpreadOptionData, CommoditySwapData, CommoditySwaptionData, CommodityAveragePriceOptionData, CommodityOptionStripData, CommodityPositionData, CommodityAsianOptionData, CommodityAccumulatorData, CommodityWindowBarrierOptionData, CommodityBasketOptionData, CommodityGenericBarrierOptionData, CommodityRainbowOptionData, CommodityVarianceSwapData, CommodityRevenueOptionData, CommodityWorstOfBasketSwapData, CommodityTaRFData, CommodityBasketVarianceSwapData, CommodityStrikeResettableOptionData |
| **Bonds** | BondData, ForwardBondData, BondFutureData, BondOptionData, BondRepoData, BondTRSData, CallableBondData, ConvertibleBondData, BondBasketData |
| **Credit** | CreditDefaultSwapData, CreditDefaultSwapOptionData, IndexCreditDefaultSwapData, IndexCreditDefaultSwapOptionData, CdoData, CreditLinkedSwapData, CBOData |
| **Interest Rate** | ForwardRateAgreementData, SwaptionData, CapFloorData, ArcOptionData, MultiLegOptionData, TreasuryLockData, RiskParticipationAgreementData |
| **Other** | TotalReturnSwapData, ContractForDifferenceData, CompositeTradeData, AscotData, ScriptedTradeData, CashPositionData |

---

## 4. Root Cause Analysis

### Code Generation Entry Point

The code is generated using `xsdcpp_generate_ore.sh` which uses `external/ore/xsd/input.xsd`:

```bash
exec "${SCRIPT_DIR}/xsdcpp_generate.sh" \
    --xsd external/ore/xsd/input.xsd \
    ...
```

### What `input.xsd` Includes

```xml
<xs:include schemaLocation="instruments.xsd"/>
<xs:include schemaLocation="simulation.xsd"/>
<!-- ... all other XSD files are included -->
```

### The Trade Type Definition in `input.xsd`

```xml
<xs:complexType name="trade">
  <xs:sequence>
    <xs:element type="oreTradeType" name="TradeType"/>
    <xs:element type="envelope" name="Envelope" minOccurs="0"/>
    <xs:element type="tradeActions" name="TradeActions" minOccurs="0"/>
    <xs:group ref="oreTradeData" minOccurs="0"/>  <!-- THIS IS NOT EXPANDED -->
  </xs:sequence>
  <xs:attribute type="xs:string" name="id" use="required"/>
</xs:complexType>
```

### Code Generator Limitations

1. **`xs:group ref` not expanded** - The `oreTradeData` group reference in `trade` type is not expanded to include all 80+ trade data types (SwapData, FxForwardData, etc.)

2. **`xs:element ref` with substitutionGroup not handled** - The `nettingSetGroup` substitution group pattern is not resolved:
   ```xml
   <!-- In instruments.xsd -->
   <xs:element ref="nettingSetGroup" minOccurs="0"/>
   <xs:element name="nettingSetGroup" abstract="true"/>
   <xs:element type="xs:string" name="NettingSetId" substitutionGroup="nettingSetGroup"/>
   ```

3. **`maxOccurs="unbounded"` inside xs:choice not handled** - In simulation.xsd:
   ```xml
   <xs:choice>
     <xs:sequence>
       <xs:element type="lgm" name="LGM" maxOccurs="unbounded" minOccurs="1"/>
     </xs:sequence>
     ...
   </xs:choice>
   ```
   This generates `xsd::optional<lgm>` instead of `xsd::vector<lgm>`

---

## 5. XSD Files in ORE

| File | Status | Notes |
|------|--------|-------|
| `ore.xsd` | Partially used | Main entry point |
| `ore_types.xsd` | Used | Basic types |
| `simulation.xsd` | Used (with issues) | LGM cardinality wrong |
| `conventions.xsd` | Used | Works |
| `currencyconfig.xsd` | Used | Works |
| `todaysmarket.xsd` | Used | Works |
| `pricingengines.xsd` | Used | Works |
| `curveconfig.xsd` | Used | Works |
| `instruments.xsd` | **NOT fully used** | Missing trade types, envelope extensions |
| `nettingsetdefinitions.xsd` | Unknown | |
| `collateralbalance.xsd` | Unknown | |
| `sensitivity.xsd` | Unknown | |
| `stress.xsd` | Unknown | |
| `creditsimulation.xsd` | Partially used | |
| `referencedata.xsd` | Unknown | |
| `scriptlibrary.xsd` | Unknown | |
| `calendaradjustment.xsd` | Used | Works |
| `counterparty.xsd` | Unknown | |
| `iborfallbackconfig.xsd` | Unknown | |
| `historicalreturnconfig.xsd` | Unknown | |
| `simmcalibration.xsd` | Unknown | |
| `baselTrafficLightconfig.xsd` | Unknown | |
| `input.xsd` | Unknown | |

---

## 6. Recommendations

1. **Verify code generator configuration** - Ensure all XSD files are included, especially `instruments.xsd`

2. **Fix cardinality mapping** - `maxOccurs="unbounded"` should generate `xsd::vector<T>` not `xsd::optional<T>`

3. **Support XSD substitution groups** - Handle `substitutionGroup` patterns for `nettingSetGroup`

4. **Add XML attributes support** - Handle `ccy` attribute on `LGM` element

5. **Verify XSD includes/imports** - Check that all `xs:include` and `xs:import` statements are followed

---

## 7. Test Files Affected

| Test | Status | Blocking Issue |
|------|--------|----------------|
| Portfolio roundtrip | Fails | Missing `NettingSetId` in envelope |
| Simulation roundtrip | Fails | LGM cardinality (should be vector) |
| Conventions roundtrip | Works | - |
| CurrencyConfig roundtrip | Works | - |
| TodaysMarket roundtrip | Works | - |
| PricingEngines roundtrip | Works | - |
| CurveConfig roundtrip | Works | - |
