# Cryptocurrency Reference Data

This directory contains cryptocurrency reference data including symbol/name
mappings and SVG icons.

## Quick Start

```bash
# Generate all crypto SQL files
python3 projects/ores.codegen/src/crypto_generate_metadata_sql.py
```

## Data Files

### Source Data

| Directory | Description | Count |
|-----------|-------------|-------|
| `cryptocurrencies/` | JSON mapping of symbol to name | ~12,243 |
| `cryptocurrency-icons/` | SVG icons keyed by symbol | 483 |

### Generated SQL (in projects/ores.sql/populate/crypto/)

| File | Description |
|------|-------------|
| `crypto.sql` | Master include file |
| `crypto_catalog_populate.sql` | Cryptocurrency catalog |
| `crypto_methodology_populate.sql` | Data sourcing methodology |
| `crypto_dataset_populate.sql` | Dataset definitions |
| `crypto_dataset_tag_populate.sql` | Dataset tags |
| `crypto_dataset_dependency_populate.sql` | Dataset dependencies |
| `crypto_images_artefact_populate.sql` | Icon images (~483) |
| `crypto_currencies_small_artefact_populate.sql` | Top 100 coins |
| `crypto_currencies_large_artefact_populate.sql` | All ~12K coins |

## Dataset Sizes

### Large (~12,243 cryptocurrencies)
- All cryptocurrencies from the source data
- Sorted alphabetically by symbol
- Best for: Comprehensive reference data

### Small (Top 100)
- Top 100 cryptocurrencies by market cap
- Sorted by market cap rank
- Best for: Quick testing, common use cases

## Classification

Cryptocurrencies are classified as:
- `crypto.major`: Top 20 by market cap (BTC, ETH, USDT, etc.)
- `crypto.minor`: All others

## Data Sources

- **Cryptocurrencies**: CoinMarketCap export
- **Icons**: [cryptocurrency-icons](https://github.com/spothq/cryptocurrency-icons) (MIT License)

## Related Files

- Generator script: `projects/ores.codegen/src/crypto_generate_metadata_sql.py`
- Manifest: `external/crypto/manifest.json`
- Methodology: `external/crypto/methodology.txt`
