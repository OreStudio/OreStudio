# LEI Dataset

This directory contains GLEIF (Global Legal Entity Identifier Foundation) LEI
data files and generated subsets for testing and development.

## Quick Start

```bash
# Generate both small and large subsets from existing data
./projects/ores.codegen/scripts/generate_lei_subsets.sh

# Generate small subset only
./projects/ores.codegen/scripts/generate_lei_subsets.sh --small

# Download latest data and generate subsets
./projects/ores.codegen/scripts/generate_lei_subsets.sh --download
```

Or using the Python script directly:

```bash
# Extract small subset
python3 projects/ores.codegen/src/lei_extract_subset.py --size small

# Extract large subset
python3 projects/ores.codegen/src/lei_extract_subset.py --size large

# Download latest data first
python3 projects/ores.codegen/src/lei_extract_subset.py --download --size small
```

## Data Files

### Source Files (from GLEIF)

| File | Description | Size |
|------|-------------|------|
| `*-lei2-golden-copy.csv` | Main LEI entity data (~3.2M entities) | ~4.3 GB |
| `*-rr-golden-copy.csv` | Relationship records (~463K relationships) | ~221 MB |

### Output Files

| File | Description | Size |
|------|-------------|------|
| `*-subset-small.csv` | Small subset (~10K entities) | ~7 MB |
| `*-subset-large.csv` | Large subset (~50K entities) | ~35 MB |

## Size Presets

### Small Subset (~10K entities)
Best for: Quick testing, CI/CD pipelines, local development
```
per_country=20, per_depth=10, per_sector=15
per_category=20, per_fund_type=10, per_legal_form=5
```

### Large Subset (~50K entities)
Best for: Comprehensive testing, performance benchmarks
```
per_country=60, per_depth=30, per_sector=40
per_category=60, per_fund_type=30, per_legal_form=10
```

## Subset Diversity

The script ensures the subset is representative across multiple dimensions:

### Geographic Diversity
- Samples from all ~235 countries in the dataset
- Configurable number of entities per country

### Entity Categories
- GENERAL (corporations, LLCs, etc.)
- FUND (investment funds)
- SOLE_PROPRIETOR
- RESIDENT_GOVERNMENT_ENTITY
- BRANCH
- INTERNATIONAL_ORGANIZATION

### Sector Diversity (detected from entity names)
- Financial: BANK, INSURANCE, BROKER_DEALER, ASSET_MANAGEMENT
- Funds: INVESTMENT_FUND, ETF, HEDGE_FUND, PRIVATE_EQUITY, PENSION
- Industrial: ENERGY, MANUFACTURING, MINING, CONSTRUCTION
- Services: TECHNOLOGY, HEALTHCARE, RETAIL, TELECOM
- Other: REAL_ESTATE, AEROSPACE, SHIPPING, AGRICULTURE, MEDIA

### Fund Types (for FUND entities)
- ETF, BOND, EQUITY, INDEX
- MONEY_MARKET, BALANCED, COMMODITY
- REAL_ESTATE, GLOBAL, EMERGING_MARKETS

### Relationship Depth
- 0 children (standalone entities)
- 1-4 children (small corporate groups)
- 5+ children (large corporate groups)

## Data Sources

- **GLEIF Golden Copy**: https://www.gleif.org/en/lei-data/gleif-golden-copy/download-the-golden-copy
- **Download Base URL**: `https://leidata-preview.gleif.org/storage/golden-copy-files/`
- **URL Pattern**: `{base}/{YYYY}/{MM}/{DD}/{ID}/{YYYYMMDD}-0800-gleif-goldencopy-{type}-golden-copy.csv.zip`

## Related Files

- Extraction script: `projects/ores.codegen/src/lei_extract_subset.py`
- Shell script: `projects/ores.codegen/scripts/generate_lei_subsets.sh`
- Manifest: `external/lei/manifest.txt`
- Methodology: `external/lei/methodology.txt`
