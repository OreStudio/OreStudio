# LEI Dataset Subset Extractor

This directory contains tools for working with the GLEIF (Global Legal Entity
Identifier Foundation) LEI dataset. The main script extracts a diverse,
representative subset from the full dataset for testing and development.

## Quick Start

```bash
# Extract subset from existing local files
python extract_subset.py

# Download latest data and extract subset
python extract_subset.py --download

# Download only (for data refresh)
python extract_subset.py --download-only
```

## Data Files

### Source Files (from GLEIF)

| File | Description | Size |
|------|-------------|------|
| `*-lei2-golden-copy.csv` | Main LEI entity data (3.2M entities) | ~4.3 GB |
| `*-rr-golden-copy.csv` | Relationship records (463K relationships) | ~221 MB |

### Output Files

| File | Description | Size |
|------|-------------|------|
| `*-lei2-golden-copy-subset.csv` | Subset of LEI entities (~8.6K) | ~6.5 MB |
| `*-rr-golden-copy-subset.csv` | Subset of relationships (~23K) | ~9.4 MB |

## Usage

### Basic Usage

```bash
# Use existing local data files
python extract_subset.py

# Customize subset size
python extract_subset.py --per-country 50 --per-sector 30 --per-depth 20
```

### Download Options

```bash
# Auto-discover and download latest data
python extract_subset.py --download

# Download without extracting subset
python extract_subset.py --download-only

# Use explicit URLs (when auto-discovery fails)
python extract_subset.py --download \
  --lei-url "https://leidata-preview.gleif.org/storage/golden-copy-files/YYYY/MM/DD/ID/filename-lei2-golden-copy.csv.zip" \
  --rr-url "https://leidata-preview.gleif.org/storage/golden-copy-files/YYYY/MM/DD/ID/filename-rr-golden-copy.csv.zip"
```

### All Options

```
Download options:
  --download            Download latest GLEIF data before processing
  --download-only       Download data without extracting subset
  --lei-url URL         Explicit URL for LEI data file
  --rr-url URL          Explicit URL for relationship records file

Subset configuration:
  --per-country N       Target entities per country (default: 30)
  --per-depth N         Target entities per relationship depth (default: 15)
  --per-sector N        Target entities per sector (default: 20)
  --per-category N      Target entities per category (default: 30)
  --per-fund-type N     Target entities per fund type (default: 15)
```

## Subset Diversity

The script ensures the subset is representative across multiple dimensions:

### Geographic Diversity
- Samples from all 235 countries in the dataset
- Configurable number of entities per country

### Entity Categories
- GENERAL (corporations, LLCs, etc.)
- FUND (investment funds)
- SOLE_PROPRIETOR
- RESIDENT_GOVERNMENT_ENTITY
- BRANCH
- INTERNATIONAL_ORGANIZATION

### Sector Diversity (detected from entity names)
- BANK, INSURANCE, BROKER_DEALER
- INVESTMENT_FUND, ETF, HEDGE_FUND, PRIVATE_EQUITY
- PENSION, ASSET_MANAGEMENT
- ENERGY, TECHNOLOGY, HEALTHCARE
- MANUFACTURING, RETAIL, TELECOM
- REAL_ESTATE, CONSTRUCTION, MINING
- AEROSPACE, SHIPPING, AGRICULTURE, MEDIA
- PROFESSIONAL_SERVICES
- OTHER (unclassified)

### Fund Types (for FUND entities)
- ETF, BOND, EQUITY, INDEX
- MONEY_MARKET, BALANCED, COMMODITY
- REAL_ESTATE, GLOBAL, EMERGING_MARKETS

### Relationship Depth
- 0 children (standalone entities)
- 1 child
- 2 children
- 3 children
- 4 children
- 5+ children (large corporate groups)

### Legal Forms
- Samples from top 100 legal form codes
- Covers diverse corporate structures across jurisdictions

## Data Sources

- **GLEIF Golden Copy**: https://www.gleif.org/en/lei-data/gleif-golden-copy/download-the-golden-copy
- **Download Base URL**: `https://leidata-preview.gleif.org/storage/golden-copy-files/`
- **URL Pattern**: `{base}/{YYYY}/{MM}/{DD}/{ID}/{YYYYMMDD}-0800-gleif-goldencopy-{type}-golden-copy.csv.zip`

---

# Code Structure

## File: `extract_subset.py`

### Overview

Single-file Python script (~620 lines) with no external dependencies beyond the
standard library. Processes multi-GB CSV files efficiently using streaming.

### Key Constants

```python
GLEIF_API_ENDPOINTS    # API URLs for discovering download links
GLEIF_DOWNLOAD_BASE    # Base URL for direct file downloads
SECTOR_KEYWORDS        # Dict mapping sector names to detection keywords
FUND_TYPE_KEYWORDS     # Dict mapping fund types to detection keywords
```

### Main Classes

#### `Config` (dataclass)
Configuration holder for subset extraction parameters.

```python
@dataclass
class Config:
    per_country: int = 30
    per_depth: int = 15
    per_sector: int = 20
    per_category: int = 30
    per_fund_type: int = 15
    per_legal_form: int = 10
```

#### `EntityInfo` (dataclass)
Holds classification information for a single LEI entity.

```python
@dataclass
class EntityInfo:
    lei: str              # LEI identifier
    name: str             # Legal name
    country: str          # Country code (column 43)
    category: str         # Entity category (column 191)
    subcategory: str      # Entity subcategory (column 192)
    legal_form_code: str  # Legal form code (column 193)
    other_legal_form: str # Other legal form text (column 194)
    sector: str           # Detected sector from name
    fund_type: str        # Detected fund type (if FUND)
    n_children: int       # Number of direct children
    is_child: bool        # Whether entity has a parent
```

#### `ProgressReporter`
Displays progress percentage during long operations.

#### `DownloadProgressReporter`
Displays download progress with MB counts.

### Key Functions

#### Download Functions

| Function | Purpose |
|----------|---------|
| `discover_gleif_urls()` | Auto-discover download URLs via API or probing |
| `try_gleif_api()` | Query a GLEIF API endpoint for file URLs |
| `try_direct_url()` | Test if a direct URL exists (HEAD request) |
| `download_and_extract()` | Download ZIP and extract CSV |
| `download_gleif_data()` | Main download orchestration |

#### Data Processing Functions

| Function | Purpose |
|----------|---------|
| `find_data_files()` | Locate LEI and RR CSV files in directory |
| `get_subset_filename()` | Generate `-subset.csv` filename |
| `build_relationship_map()` | Parse RR file into parent->children dict |
| `analyze_entities()` | Read LEI file, classify all entities |
| `select_subset()` | Select diverse subset using multi-dimensional sampling |
| `write_subset()` | Write filtered LEI and RR CSV files |

#### Classification Functions

| Function | Purpose |
|----------|---------|
| `detect_sector()` | Classify entity by sector using name keywords |
| `detect_fund_type()` | Classify fund by type using name keywords |

#### Utility Functions

| Function | Purpose |
|----------|---------|
| `count_lines()` | Efficiently count lines in large file |

### Processing Pipeline

```
main()
  │
  ├─► download_gleif_data()        # If --download
  │     ├─► discover_gleif_urls()
  │     └─► download_and_extract()
  │
  ├─► find_data_files()            # Locate CSV files
  │
  ├─► build_relationship_map()     # Phase 1: Parse relationships
  │     └─► Returns: parent_to_children, child_to_parent
  │
  ├─► analyze_entities()           # Phase 2: Read & classify entities
  │     ├─► detect_sector()
  │     ├─► detect_fund_type()
  │     └─► Returns: Dict[lei, EntityInfo]
  │
  ├─► select_subset()              # Phase 3: Multi-dimensional sampling
  │     ├─► Build indices by dimension
  │     ├─► Sample from each dimension
  │     ├─► Add children of selected parents
  │     ├─► Add parents of selected children
  │     └─► Returns: Set[lei]
  │
  └─► write_subset()               # Phase 4: Write output files
```

### CSV Column Indices (LEI File)

| Index | Column Name | Used For |
|-------|-------------|----------|
| 0 | LEI | Primary identifier |
| 1 | Entity.LegalName | Name, sector detection |
| 43 | Entity.LegalAddress.Country | Geographic diversity |
| 191 | Entity.EntityCategory | Category diversity |
| 192 | Entity.EntitySubCategory | Subcategory diversity |
| 193 | Entity.LegalForm.EntityLegalFormCode | Legal form diversity |
| 194 | Entity.LegalForm.OtherLegalForm | Additional form info |
| 199 | Entity.EntityStatus | Filter inactive entities |

### CSV Column Indices (RR File)

| Index | Column Name | Used For |
|-------|-------------|----------|
| 0 | Relationship.StartNode.NodeID | Child LEI |
| 2 | Relationship.EndNode.NodeID | Parent LEI |
| 4 | Relationship.RelationshipType | Filter for IS_DIRECTLY_CONSOLIDATED_BY |

### Relationship Types in Dataset

| Type | Count | Description |
|------|-------|-------------|
| IS_FUND-MANAGED_BY | 141K | Fund managed by entity |
| IS_ULTIMATELY_CONSOLIDATED_BY | 128K | Ultimate parent |
| IS_DIRECTLY_CONSOLIDATED_BY | 122K | Direct parent (used for hierarchy) |
| IS_SUBFUND_OF | 69K | Subfund relationship |
| IS_INTERNATIONAL_BRANCH_OF | 2K | Branch relationship |
| IS_FEEDER_TO | 1K | Feeder fund |

### Memory Considerations

- Relationship map: ~50K parents, ~122K children (small dicts)
- Entity info: ~3M EntityInfo objects during analysis
- Selected LEIs: ~8-10K string set
- Files processed via streaming (no full file load)

### Adding New Sectors

To add a new sector for detection, add to `SECTOR_KEYWORDS`:

```python
SECTOR_KEYWORDS = {
    # ... existing sectors ...
    'NEW_SECTOR': ['KEYWORD1', 'KEYWORD2', 'KEYWORD 3'],
}
```

Keywords are matched against uppercase entity names with space padding.
