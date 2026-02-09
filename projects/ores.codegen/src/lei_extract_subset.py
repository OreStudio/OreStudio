#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Extract a diverse subset from the GLEIF LEI dataset.

This script creates a smaller, representative subset of the LEI data that includes:
- Entities from as many countries as possible
- All entity categories (GENERAL, FUND, SOLE_PROPRIETOR, etc.)
- Entities with different relationship depths (0, 1, 2, 3, 4, 5+ children)
- Diversity across detected sectors (banks, insurance, funds, energy, etc.)
- Various legal forms and fund types

Usage:
    python lei_extract_subset.py --size small   # Small subset for quick testing
    python lei_extract_subset.py --size large   # Larger subset for comprehensive testing
    python lei_extract_subset.py --download     # Download latest data first

Output files are written to external/lei/:
    - <original_lei_file>-subset-<size>.csv
    - <original_rr_file>-subset-<size>.csv
"""

import argparse
import csv
import json
import os
import re
import sys
import urllib.request
import zipfile
from collections import defaultdict, Counter
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from pathlib import Path
from typing import Set, Dict, List, Tuple, Optional
import glob as glob_module
import random

# Directory where LEI data files are stored (external/lei relative to repo root)
def get_lei_data_dir() -> Path:
    """Get the path to external/lei directory."""
    # This script lives in projects/ores.codegen/src/
    # We need to go up to repo root, then into external/lei
    script_dir = Path(__file__).parent
    repo_root = script_dir.parent.parent.parent  # src -> ores.codegen -> projects -> repo_root
    return repo_root / "external" / "lei"


# Subset size configurations
SUBSET_SIZES = {
    'small': {
        'per_country': 20,
        'per_depth': 10,
        'per_sector': 15,
        'per_category': 20,
        'per_fund_type': 10,
        'per_legal_form': 5,
    },
    'large': {
        'per_country': 60,
        'per_depth': 30,
        'per_sector': 40,
        'per_category': 60,
        'per_fund_type': 30,
        'per_legal_form': 10,
    },
}

# GLEIF data endpoints
# Primary: Try the golden copy API
# Fallback: Direct file listing endpoint
GLEIF_API_ENDPOINTS = [
    "https://api.gleif.org/api/v1/golden-copies/publishes",
    "https://goldencopy.gleif.org/api/v2/golden-copies/publishes",
]

# Base URL pattern for direct downloads
GLEIF_DOWNLOAD_BASE = "https://leidata-preview.gleif.org/storage/golden-copy-files"

# LEI file column indices
LEI_COL_ID = 0
LEI_COL_NAME = 1
LEI_COL_COUNTRY = 43
LEI_COL_CATEGORY = 191
LEI_COL_SUBCATEGORY = 192
LEI_COL_LEGAL_FORM_CODE = 193
LEI_COL_OTHER_LEGAL_FORM = 194
LEI_COL_STATUS = 199

# Relationship file column indices
RR_COL_START_NODE = 0
RR_COL_END_NODE = 2
RR_COL_RELATIONSHIP_TYPE = 4


class DownloadProgressReporter:
    """Progress reporter for file downloads."""

    def __init__(self, filename: str, total_size: int):
        self.filename = filename
        self.total_size = total_size
        self.downloaded = 0
        self.last_percent = -1

    def update(self, block_count: int, block_size: int, total_size: int):
        if total_size > 0:
            self.total_size = total_size
        self.downloaded = block_count * block_size
        if self.total_size > 0:
            percent = int(100 * self.downloaded / self.total_size)
            if percent != self.last_percent and percent % 5 == 0:
                self.last_percent = percent
                mb_downloaded = self.downloaded / (1024 * 1024)
                mb_total = self.total_size / (1024 * 1024)
                sys.stdout.write(f"\r  Downloading {self.filename}: {percent}% ({mb_downloaded:.1f}/{mb_total:.1f} MB)")
                sys.stdout.flush()

    def finish(self):
        mb_total = self.total_size / (1024 * 1024)
        sys.stdout.write(f"\r  Downloading {self.filename}: 100% ({mb_total:.1f} MB)          \n")
        sys.stdout.flush()


def try_gleif_api(api_base: str, date_str: str) -> Tuple[Optional[str], Optional[str]]:
    """Try to get URLs from a GLEIF API endpoint."""
    api_url = f"{api_base}?filter[publish_date]={date_str}"

    try:
        req = urllib.request.Request(api_url, headers={"Accept": "application/json"})
        with urllib.request.urlopen(req, timeout=30) as response:
            data = json.loads(response.read().decode('utf-8'))

        lei_url = None
        rr_url = None

        for item in data.get('data', []):
            attrs = item.get('attributes', {})
            download_url = attrs.get('download_url', '')

            if 'lei2-golden-copy' in download_url:
                lei_url = download_url
            elif 'rr-golden-copy' in download_url:
                rr_url = download_url

        return lei_url, rr_url
    except (urllib.error.URLError, json.JSONDecodeError, KeyError, TypeError):
        return None, None


def try_direct_url(base_url: str, file_id: int, date: datetime, file_type: str) -> Optional[str]:
    """Try to access a direct URL with a specific file ID."""
    date_path = date.strftime("%Y/%m/%d")
    date_str = date.strftime("%Y%m%d")
    filename = f"{date_str}-0800-gleif-goldencopy-{file_type}-golden-copy.csv.zip"
    url = f"{base_url}/{date_path}/{file_id}/{filename}"

    try:
        req = urllib.request.Request(url, method='HEAD')
        with urllib.request.urlopen(req, timeout=10) as response:
            if response.status == 200:
                return url
    except (urllib.error.URLError, urllib.error.HTTPError, TimeoutError, OSError):
        pass
    return None


def discover_gleif_urls(target_date: Optional[datetime] = None) -> Tuple[str, str]:
    """
    Discover the download URLs for LEI and RR files from GLEIF.

    Tries multiple strategies:
    1. Query GLEIF API endpoints
    2. Try direct URL construction with ID probing

    URL pattern: https://leidata-preview.gleif.org/storage/golden-copy-files/YYYY/MM/DD/ID/filename.csv.zip

    Args:
        target_date: Date to download (defaults to today)

    Returns:
        Tuple of (lei_url, rr_url)
    """
    if target_date is None:
        target_date = datetime.now()

    date_str = target_date.strftime("%Y-%m-%d")
    print(f"Discovering GLEIF download URLs for {date_str}...")

    # Strategy 1: Try all API endpoints
    for api_base in GLEIF_API_ENDPOINTS:
        print(f"  Trying API: {api_base}...")
        lei_url, rr_url = try_gleif_api(api_base, date_str)
        if lei_url and rr_url:
            print(f"  Found URLs via API")
            return lei_url, rr_url

    # Strategy 2: Try previous days with API
    print("  Trying previous days...")
    for days_back in range(1, 8):
        try_date = target_date - timedelta(days=days_back)
        try_date_str = try_date.strftime("%Y-%m-%d")

        for api_base in GLEIF_API_ENDPOINTS:
            lei_url, rr_url = try_gleif_api(api_base, try_date_str)
            if lei_url and rr_url:
                print(f"  Found files from {try_date_str}")
                return lei_url, rr_url

    # Strategy 3: Try direct URL probing with common ID ranges
    # Based on observed IDs (1181452, 1181497), they seem to be in the 1M+ range
    print("  Probing direct URLs (this may take a moment)...")
    for days_back in range(0, 8):
        try_date = target_date - timedelta(days=days_back)

        # Try a range of IDs around recently observed values
        # IDs seem to increment over time
        base_id = 1181400 + (days_back * -50)  # Rough estimate

        for id_offset in range(-100, 200, 10):
            test_id = base_id + id_offset
            if test_id <= 0:
                continue

            lei_url = try_direct_url(GLEIF_DOWNLOAD_BASE, test_id, try_date, "lei2")
            if lei_url:
                # Found LEI, now search nearby for RR (usually ~45 IDs apart based on examples)
                for rr_offset in range(40, 60):
                    rr_url = try_direct_url(GLEIF_DOWNLOAD_BASE, test_id + rr_offset, try_date, "rr")
                    if rr_url:
                        print(f"  Found files via direct probing (date: {try_date.strftime('%Y-%m-%d')})")
                        return lei_url, rr_url

    raise RuntimeError(
        "Could not discover GLEIF download URLs automatically.\n"
        "Please provide URLs manually with --lei-url and --rr-url\n"
        "URLs can be found at: https://www.gleif.org/en/lei-data/gleif-golden-copy/download-the-golden-copy"
    )


def download_and_extract(url: str, output_dir: Path) -> str:
    """
    Download a zip file and extract the CSV.

    Args:
        url: URL to download
        output_dir: Directory to save the extracted file

    Returns:
        Path to the extracted CSV file
    """
    # Extract filename from URL
    zip_filename = url.split('/')[-1]
    csv_filename = zip_filename.replace('.zip', '')
    zip_path = output_dir / zip_filename
    csv_path = output_dir / csv_filename

    # Check if CSV already exists
    if csv_path.exists():
        print(f"  {csv_filename} already exists, skipping download")
        return str(csv_path)

    # Download the zip file
    print(f"  Downloading {zip_filename}...")
    progress = DownloadProgressReporter(zip_filename, 0)

    try:
        urllib.request.urlretrieve(url, zip_path, reporthook=progress.update)
        progress.finish()
    except (urllib.error.URLError, urllib.error.HTTPError, TimeoutError, OSError) as e:
        if zip_path.exists():
            zip_path.unlink()
        raise RuntimeError(f"Download failed: {e}")

    # Extract the CSV
    print(f"  Extracting {csv_filename}...")
    try:
        with zipfile.ZipFile(zip_path, 'r') as zf:
            # Find the CSV file in the archive
            csv_names = [n for n in zf.namelist() if n.endswith('.csv')]
            if not csv_names:
                raise RuntimeError(f"No CSV file found in {zip_filename}")

            # Extract the CSV
            zf.extract(csv_names[0], output_dir)
            extracted_path = output_dir / csv_names[0]

            # Rename if needed
            if extracted_path != csv_path:
                if csv_path.exists():
                    csv_path.unlink()
                extracted_path.rename(csv_path)

    finally:
        # Clean up zip file
        if zip_path.exists():
            zip_path.unlink()

    print(f"  Extracted: {csv_filename}")
    return str(csv_path)


def download_gleif_data(
    output_dir: Path,
    lei_url: Optional[str] = None,
    rr_url: Optional[str] = None,
    target_date: Optional[datetime] = None
) -> Tuple[str, str]:
    """
    Download the latest GLEIF golden copy files.

    Args:
        output_dir: Directory to save files
        lei_url: Optional explicit URL for LEI file
        rr_url: Optional explicit URL for RR file
        target_date: Target date for downloads (defaults to today)

    Returns:
        Tuple of (lei_file_path, rr_file_path)
    """
    print("\n=== Downloading GLEIF Data ===")

    # Discover URLs if not provided
    if not lei_url or not rr_url:
        discovered_lei, discovered_rr = discover_gleif_urls(target_date)
        lei_url = lei_url or discovered_lei
        rr_url = rr_url or discovered_rr

    print(f"\nLEI URL: {lei_url}")
    print(f"RR URL: {rr_url}")

    # Download and extract
    lei_path = download_and_extract(lei_url, output_dir)
    rr_path = download_and_extract(rr_url, output_dir)

    return lei_path, rr_path


# Sector detection keywords - used to classify entities by industry
SECTOR_KEYWORDS = {
    'BANK': ['BANK', 'BANKING', 'BANQUE', 'BANCA', 'BANCO', 'SPARKASSE', 'CREDIT UNION', 'SAVINGS BANK'],
    'INSURANCE': ['INSURANCE', 'ASSURANCE', 'VERSICHERUNG', 'SEGUROS', 'ASSICURAZION', 'REINSURANCE', 'UNDERWRITER'],
    'INVESTMENT_FUND': ['MUTUAL FUND', 'UCITS', 'SICAV', 'FONDS COMMUN'],
    'ETF': ['ETF', 'EXCHANGE TRADED', 'EXCHANGE-TRADED'],
    'HEDGE_FUND': ['HEDGE FUND', 'HEDGE-FUND', 'CAPITAL PARTNERS LP', 'CAPITAL OFFSHORE', 'OFFSHORE FUND'],
    'PRIVATE_EQUITY': ['PRIVATE EQUITY', 'BUYOUT', 'VENTURE CAPITAL', 'PE FUND', 'GROWTH EQUITY'],
    'PENSION': ['PENSION', 'RETIREMENT', 'SUPERANNUATION', '401K', 'PROVIDENT FUND'],
    'ASSET_MANAGEMENT': ['ASSET MANAGEMENT', 'WEALTH MANAGEMENT', 'INVESTMENT MANAGEMENT', 'PORTFOLIO MANAGEMENT'],
    'BROKER_DEALER': ['BROKER', 'DEALER', 'SECURITIES', 'BROKERAGE', 'TRADING CO'],
    'REAL_ESTATE': ['REAL ESTATE', 'REIT', 'PROPERTY FUND', 'REALTY', 'IMMOBILIEN', 'IMMOBILIER'],
    'ENERGY': ['ENERGY', ' OIL ', 'PETROLEUM', 'NATURAL GAS', 'UTILITIES', 'ELECTRIC', 'POWER GENERATION'],
    'TECHNOLOGY': ['TECHNOLOGY', 'SOFTWARE', 'DIGITAL', ' TECH ', 'COMPUTING', 'CYBER', 'FINTECH'],
    'HEALTHCARE': ['HEALTHCARE', 'HEALTH CARE', 'MEDICAL', 'PHARMA', 'BIOTECH', 'HOSPITAL', 'THERAPEUTICS'],
    'MANUFACTURING': ['MANUFACTURING', 'INDUSTRIAL', 'FACTORY', 'PRODUCTION CO'],
    'RETAIL': ['RETAIL', 'STORES', 'SHOPPING', 'E-COMMERCE', 'CONSUMER GOODS'],
    'TELECOM': ['TELECOM', 'COMMUNICATIONS', 'MOBILE', 'WIRELESS', 'NETWORK'],
    'AUTOMOTIVE': ['AUTOMOTIVE', 'AUTOMOBILE', 'VEHICLE', 'MOTOR CO', 'CAR '],
    'AEROSPACE': ['AEROSPACE', 'AVIATION', 'AIRCRAFT', 'AIRLINE', 'DEFENCE', 'DEFENSE'],
    'MINING': ['MINING', 'METALS', 'MINERAL', 'RESOURCES', 'EXPLORATION'],
    'CONSTRUCTION': ['CONSTRUCTION', 'BUILDING', 'INFRASTRUCTURE', 'CIVIL ENGINEERING'],
    'PROFESSIONAL_SERVICES': ['CONSULTING', 'ADVISORY', 'LEGAL', 'ACCOUNTING', 'AUDIT', 'LAW FIRM'],
    'SHIPPING': ['SHIPPING', 'MARITIME', 'VESSEL', 'CARGO', 'LOGISTICS', 'FREIGHT'],
    'AGRICULTURE': ['AGRICULTURE', 'FARMING', 'AGRI', 'FOOD', 'BEVERAGE'],
    'MEDIA': ['MEDIA', 'ENTERTAINMENT', 'BROADCAST', 'PUBLISHING', 'FILM', 'MUSIC'],
}

# Fund type keywords - for more detailed classification of FUND entities
FUND_TYPE_KEYWORDS = {
    'ETF': ['ETF', 'EXCHANGE TRADED', 'EXCHANGE-TRADED'],
    'MONEY_MARKET': ['MONEY MARKET', 'LIQUIDITY', 'CASH FUND'],
    'BOND': ['BOND', 'FIXED INCOME', 'DEBT', 'CREDIT', 'HIGH YIELD', 'TREASURY'],
    'EQUITY': ['EQUITY', 'STOCK', 'SHARES', 'GROWTH FUND', 'VALUE FUND'],
    'INDEX': ['INDEX', 'TRACKER', 'PASSIVE'],
    'BALANCED': ['BALANCED', 'MIXED', 'MULTI-ASSET'],
    'REAL_ESTATE': ['REAL ESTATE', 'REIT', 'PROPERTY'],
    'COMMODITY': ['COMMODITY', 'GOLD', 'PRECIOUS METALS'],
    'EMERGING_MARKETS': ['EMERGING', 'FRONTIER', 'DEVELOPING'],
    'GLOBAL': ['GLOBAL', 'WORLD', 'INTERNATIONAL'],
}


def load_anchor_leis(data_dir: Path) -> Set[str]:
    """
    Load anchor LEIs from anchor_leis.json.

    The JSON file is generated by lei_extract_anchor_leis.py from regulatory
    lists (ECB SSM, UK PRA) plus hand-curated G-SIB LEIs.
    """
    anchor_file = data_dir / "anchor_leis.json"
    if not anchor_file.exists():
        print(f"Warning: anchor LEI file not found: {anchor_file}")
        print("  Run lei_extract_anchor_leis.py to generate it.")
        return set()

    with open(anchor_file, 'r', encoding='utf-8') as f:
        data = json.load(f)

    leis = {entry["lei"] for entry in data.get("leis", [])}
    print(f"Loaded {len(leis)} anchor LEIs from {anchor_file.name}")
    return leis


@dataclass
class Config:
    lei_file: str = ""
    rr_file: str = ""
    size: str = "small"
    per_country: int = 60
    per_depth: int = 30
    per_sector: int = 40
    per_category: int = 60
    per_fund_type: int = 30
    per_legal_form: int = 10


class ProgressReporter:
    """Progress reporter for long-running operations."""

    def __init__(self, total: int = 0, desc: str = "Processing"):
        self.total = total
        self.desc = desc
        self.current = 0
        self.last_percent = -1

    def update(self, n: int = 1):
        self.current += n
        if self.total > 0:
            percent = int(100 * self.current / self.total)
            if percent != self.last_percent and percent % 5 == 0:
                self.last_percent = percent
                sys.stdout.write(f"\r{self.desc}: {percent}% ({self.current:,}/{self.total:,})")
                sys.stdout.flush()

    def finish(self):
        if self.total > 0:
            sys.stdout.write(f"\r{self.desc}: 100% ({self.total:,}/{self.total:,})\n")
        else:
            sys.stdout.write(f"\r{self.desc}: {self.current:,} records processed\n")
        sys.stdout.flush()


def count_lines(filepath: str) -> int:
    """Count lines in a file efficiently."""
    print(f"Counting lines in {os.path.basename(filepath)}...")
    count = 0
    with open(filepath, 'rb') as f:
        for _ in f:
            count += 1
    return count - 1  # Subtract header


def detect_sector(name: str) -> str:
    """Detect sector from entity name using keywords."""
    name_upper = ' ' + name.upper() + ' '
    for sector, keywords in SECTOR_KEYWORDS.items():
        for kw in keywords:
            if kw in name_upper:
                return sector
    return 'OTHER'


def detect_fund_type(name: str) -> str:
    """Detect fund type from fund entity name."""
    name_upper = ' ' + name.upper() + ' '
    for fund_type, keywords in FUND_TYPE_KEYWORDS.items():
        for kw in keywords:
            if kw in name_upper:
                return fund_type
    return 'OTHER_FUND'


def find_data_files(directory: Path) -> Tuple[str, str]:
    """Find the LEI and RR data files in the directory."""
    lei_pattern = str(directory / "*-gleif-goldencopy-lei2-golden-copy.csv")
    rr_pattern = str(directory / "*-gleif-goldencopy-rr-golden-copy.csv")

    lei_files = sorted(glob_module.glob(lei_pattern))
    rr_files = sorted(glob_module.glob(rr_pattern))

    if not lei_files:
        raise FileNotFoundError(f"No LEI file found matching: {lei_pattern}")
    if not rr_files:
        raise FileNotFoundError(f"No RR file found matching: {rr_pattern}")

    # Return latest files (filenames start with YYYYMMDD, so sorted order gives chronological)
    return lei_files[-1], rr_files[-1]


def get_subset_filename(original: str, size: str) -> str:
    """Generate subset filename by inserting '-subset-<size>' before .csv extension."""
    base = original.rsplit('.csv', 1)[0]
    # Remove any existing -subset suffix if present
    base = re.sub(r'-subset(-\w+)?$', '', base)
    return f"{base}-subset-{size}.csv"


def build_relationship_map(rr_file: str) -> Tuple[Dict[str, List[str]], Dict[str, str]]:
    """
    Build a map of parent LEI -> list of child LEIs.

    Uses IS_DIRECTLY_CONSOLIDATED_BY relationships where:
    - StartNode (column 0) is the child
    - EndNode (column 2) is the parent
    """
    print("\n=== Phase 1: Building relationship map ===")
    total_lines = count_lines(rr_file)

    parent_to_children: Dict[str, List[str]] = defaultdict(list)
    child_to_parent: Dict[str, str] = {}

    progress = ProgressReporter(total_lines, "Reading relationships")

    with open(rr_file, 'r', encoding='utf-8') as f:
        reader = csv.reader(f)
        next(reader)  # Skip header

        for row in reader:
            progress.update()
            if len(row) < 5:
                continue

            rel_type = row[RR_COL_RELATIONSHIP_TYPE]
            if rel_type == 'IS_DIRECTLY_CONSOLIDATED_BY':
                child_lei = row[RR_COL_START_NODE]
                parent_lei = row[RR_COL_END_NODE]
                parent_to_children[parent_lei].append(child_lei)
                child_to_parent[child_lei] = parent_lei

    progress.finish()

    # Calculate statistics
    child_counts = Counter(len(children) for children in parent_to_children.values())

    print(f"\nRelationship statistics:")
    print(f"  Total parents with children: {len(parent_to_children):,}")
    print(f"  Total children with parents: {len(child_to_parent):,}")
    print(f"  Child count distribution:")
    for n_children in sorted(child_counts.keys())[:10]:
        print(f"    {n_children} children: {child_counts[n_children]:,} parents")
    if child_counts and max(child_counts.keys()) > 10:
        print(f"    ... (max: {max(child_counts.keys())} children)")

    return parent_to_children, child_to_parent


@dataclass
class EntityInfo:
    """Information about an entity for selection."""
    lei: str
    name: str
    country: str
    category: str
    subcategory: str
    legal_form_code: str
    other_legal_form: str
    sector: str
    fund_type: str
    n_children: int
    is_child: bool


def analyze_entities(
    lei_file: str,
    parent_to_children: Dict[str, List[str]],
    child_to_parent: Dict[str, str],
) -> Dict[str, EntityInfo]:
    """
    Read and analyze all entities, collecting classification info.
    """
    print("\n=== Phase 2: Analyzing entities ===")
    total_lines = count_lines(lei_file)

    entities: Dict[str, EntityInfo] = {}
    progress = ProgressReporter(total_lines, "Reading LEI data")

    with open(lei_file, 'r', encoding='utf-8') as f:
        reader = csv.reader(f)
        next(reader)  # Skip header

        for row in reader:
            progress.update()
            if len(row) < 200:
                continue

            lei = row[LEI_COL_ID]
            name = row[LEI_COL_NAME]
            country = row[LEI_COL_COUNTRY] if row[LEI_COL_COUNTRY] else "UNKNOWN"
            category = row[LEI_COL_CATEGORY] if row[LEI_COL_CATEGORY] else "GENERAL"
            subcategory = row[LEI_COL_SUBCATEGORY] if row[LEI_COL_SUBCATEGORY] else ""
            legal_form_code = row[LEI_COL_LEGAL_FORM_CODE] if row[LEI_COL_LEGAL_FORM_CODE] else ""
            other_legal_form = row[LEI_COL_OTHER_LEGAL_FORM] if row[LEI_COL_OTHER_LEGAL_FORM] else ""
            status = row[LEI_COL_STATUS] if len(row) > LEI_COL_STATUS else ""

            # Skip inactive entities
            if status == 'INACTIVE':
                continue

            # Detect sector from name
            sector = detect_sector(name)

            # Detect fund type if it's a fund
            fund_type = ""
            if category == 'FUND':
                fund_type = detect_fund_type(name)

            # Relationship info
            n_children = len(parent_to_children.get(lei, []))
            is_child = lei in child_to_parent

            entities[lei] = EntityInfo(
                lei=lei,
                name=name,
                country=country,
                category=category,
                subcategory=subcategory,
                legal_form_code=legal_form_code,
                other_legal_form=other_legal_form,
                sector=sector,
                fund_type=fund_type,
                n_children=n_children,
                is_child=is_child,
            )

    progress.finish()
    return entities


def select_subset(
    entities: Dict[str, EntityInfo],
    parent_to_children: Dict[str, List[str]],
    child_to_parent: Dict[str, str],
    config: Config,
    anchor_leis: Set[str] = None,
) -> Set[str]:
    """
    Select a diverse subset of LEIs based on multiple dimensions.
    """
    print("\n=== Phase 3: Selecting diverse subset ===")

    random.seed(42)  # Reproducible selection

    selected_leis: Set[str] = set()

    # Force-include anchor LEIs (G-SIBs and major financial institutions)
    if anchor_leis:
        entity_leis = set(entities.keys())
        anchor_found = anchor_leis & entity_leis
        anchor_missing = anchor_leis - entity_leis
        selected_leis.update(anchor_found)
        print(f"\nAnchor entities: {len(anchor_found)} included, {len(anchor_missing)} not found in source data")
        if anchor_missing:
            print(f"  Missing LEIs (may be inactive): {', '.join(sorted(anchor_missing)[:5])}...")

    # Build indices for each dimension
    by_country: Dict[str, List[str]] = defaultdict(list)
    by_category: Dict[str, List[str]] = defaultdict(list)
    by_subcategory: Dict[str, List[str]] = defaultdict(list)
    by_sector: Dict[str, List[str]] = defaultdict(list)
    by_fund_type: Dict[str, List[str]] = defaultdict(list)
    by_depth: Dict[int, List[str]] = defaultdict(list)
    by_legal_form: Dict[str, List[str]] = defaultdict(list)

    for lei, info in entities.items():
        by_country[info.country].append(lei)
        by_category[info.category].append(lei)
        if info.subcategory:
            by_subcategory[info.subcategory].append(lei)
        by_sector[info.sector].append(lei)
        if info.fund_type:
            by_fund_type[info.fund_type].append(lei)
        depth_bucket = min(info.n_children, 5)
        by_depth[depth_bucket].append(lei)
        if info.legal_form_code:
            by_legal_form[info.legal_form_code].append(lei)

    # Print dimension statistics
    print(f"\nDimension statistics:")
    print(f"  Countries: {len(by_country)}")
    print(f"  Categories: {len(by_category)}")
    print(f"  Subcategories: {len(by_subcategory)}")
    print(f"  Sectors: {len(by_sector)}")
    print(f"  Fund types: {len(by_fund_type)}")
    print(f"  Legal forms: {len(by_legal_form)}")

    def sample_from(source: Dict[str, List[str]], target: int, name: str) -> int:
        """Sample from each bucket in source, avoiding already selected."""
        added = 0
        for key, candidates in sorted(source.items()):
            available = [lei for lei in candidates if lei not in selected_leis]
            if available:
                sample_size = min(target, len(available))
                selected = random.sample(available, sample_size)
                selected_leis.update(selected)
                added += len(selected)
        print(f"  {name}: added {added:,} entities")
        return added

    # 1. Sample by relationship depth (priority - ensures hierarchy diversity)
    print("\nSelecting by dimensions...")
    sample_from(by_depth, config.per_depth, "Relationship depth")

    # 2. Sample by sector (important for industry diversity)
    sample_from(by_sector, config.per_sector, "Sector")

    # 3. Sample by fund type (for fund diversity)
    sample_from(by_fund_type, config.per_fund_type, "Fund type")

    # 4. Sample by entity category
    sample_from(by_category, config.per_category, "Category")

    # 5. Sample by subcategory
    sample_from(by_subcategory, config.per_category, "Subcategory")

    # 6. Sample by country (ensures geographic diversity)
    sample_from(by_country, config.per_country, "Country")

    # 7. Sample by legal form (top 100 legal forms)
    top_legal_forms = {k: v for k, v in sorted(by_legal_form.items(), key=lambda x: -len(x[1]))[:100]}
    sample_from(top_legal_forms, config.per_legal_form, "Legal form (top 100)")

    # 8. Add children of selected parents (for complete hierarchies)
    print("\n  Adding related entities...")
    children_to_add: Set[str] = set()
    for lei in list(selected_leis):
        if lei in parent_to_children:
            for child in parent_to_children[lei]:
                if child in entities:
                    children_to_add.add(child)

    selected_leis.update(children_to_add)
    print(f"    Added {len(children_to_add):,} children")

    # 9. Add parents of selected children
    parents_to_add: Set[str] = set()
    for lei in list(selected_leis):
        if lei in child_to_parent:
            parent = child_to_parent[lei]
            if parent in entities:
                parents_to_add.add(parent)

    selected_leis.update(parents_to_add)
    print(f"    Added {len(parents_to_add):,} parents")

    # Print final statistics
    print(f"\n=== Subset Statistics ===")
    print(f"Total selected: {len(selected_leis):,}")

    # Country distribution
    country_dist = Counter(entities[lei].country for lei in selected_leis if lei in entities)
    print(f"\nCountries represented: {len(country_dist)}")
    print("Top 15 countries:")
    for country, count in country_dist.most_common(15):
        print(f"  {country}: {count}")

    # Category distribution
    cat_dist = Counter(entities[lei].category for lei in selected_leis if lei in entities)
    print(f"\nCategories:")
    for cat, count in cat_dist.most_common():
        print(f"  {cat}: {count}")

    # Sector distribution
    sector_dist = Counter(entities[lei].sector for lei in selected_leis if lei in entities)
    print(f"\nSectors:")
    for sector, count in sector_dist.most_common():
        print(f"  {sector}: {count}")

    # Fund type distribution (for FUND entities)
    fund_type_dist = Counter(
        entities[lei].fund_type for lei in selected_leis
        if lei in entities and entities[lei].fund_type
    )
    if fund_type_dist:
        print(f"\nFund types:")
        for ftype, count in fund_type_dist.most_common():
            print(f"  {ftype}: {count}")

    # Depth distribution
    depth_dist = Counter(min(entities[lei].n_children, 5) for lei in selected_leis if lei in entities)
    print(f"\nRelationship depth:")
    for depth, count in sorted(depth_dist.items()):
        label = f"{depth}+ children" if depth == 5 else f"{depth} children"
        print(f"  {label}: {count}")

    return selected_leis


def write_subset(
    lei_file: str,
    rr_file: str,
    selected_leis: Set[str],
    output_lei: str,
    output_rr: str
):
    """Write the subset files."""
    print("\n=== Phase 4: Writing subset files ===")

    # Write LEI subset
    print(f"Writing {os.path.basename(output_lei)}...")
    total_lines = count_lines(lei_file)
    progress = ProgressReporter(total_lines, "Writing LEI subset")
    written = 0

    with open(lei_file, 'r', encoding='utf-8') as infile:
        with open(output_lei, 'w', encoding='utf-8', newline='') as outfile:
            reader = csv.reader(infile)
            writer = csv.writer(outfile)

            # Write header
            header = next(reader)
            writer.writerow(header)

            for row in reader:
                progress.update()
                if row[LEI_COL_ID] in selected_leis:
                    writer.writerow(row)
                    written += 1

    progress.finish()
    print(f"  Written {written:,} LEI records")

    # Write relationship subset
    print(f"Writing {os.path.basename(output_rr)}...")
    total_lines = count_lines(rr_file)
    progress = ProgressReporter(total_lines, "Writing RR subset")
    written = 0

    with open(rr_file, 'r', encoding='utf-8') as infile:
        with open(output_rr, 'w', encoding='utf-8', newline='') as outfile:
            reader = csv.reader(infile)
            writer = csv.writer(outfile)

            # Write header
            header = next(reader)
            writer.writerow(header)

            for row in reader:
                progress.update()
                if len(row) >= 3:
                    start_lei = row[RR_COL_START_NODE]
                    end_lei = row[RR_COL_END_NODE]
                    # Include relationship only if both parties are in our subset
                    if start_lei in selected_leis and end_lei in selected_leis:
                        writer.writerow(row)
                        written += 1

    progress.finish()
    print(f"  Written {written:,} relationship records")


def main():
    parser = argparse.ArgumentParser(
        description="Extract a diverse subset from the GLEIF LEI dataset",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Extract small subset from existing local files
  python lei_extract_subset.py --size small

  # Extract large subset
  python lei_extract_subset.py --size large

  # Download latest data and extract subset
  python lei_extract_subset.py --download --size small

  # Download only (no subset extraction)
  python lei_extract_subset.py --download-only

  # Customize subset size beyond presets
  python lei_extract_subset.py --size small --per-country 50 --per-sector 30
"""
    )

    # Size presets
    parser.add_argument(
        "--size", "-z",
        type=str,
        choices=['small', 'large'],
        default='small',
        help="Subset size preset: 'small' (~10k entities) or 'large' (~50k entities). Default: small"
    )

    # Download options
    download_group = parser.add_argument_group('Download options')
    download_group.add_argument(
        "--download",
        action="store_true",
        help="Download latest GLEIF data before processing"
    )
    download_group.add_argument(
        "--download-only",
        action="store_true",
        help="Download data without extracting subset"
    )
    download_group.add_argument(
        "--lei-url",
        type=str,
        help="Explicit URL for LEI data file (optional)"
    )
    download_group.add_argument(
        "--rr-url",
        type=str,
        help="Explicit URL for relationship records file (optional)"
    )

    # Subset configuration options (override size presets)
    subset_group = parser.add_argument_group('Subset configuration (overrides size presets)')
    subset_group.add_argument(
        "--per-country", "-c",
        type=int,
        help="Target entities per country"
    )
    subset_group.add_argument(
        "--per-depth", "-d",
        type=int,
        help="Target entities per relationship depth bucket"
    )
    subset_group.add_argument(
        "--per-sector", "-s",
        type=int,
        help="Target entities per sector"
    )
    subset_group.add_argument(
        "--per-category", "-t",
        type=int,
        help="Target entities per category"
    )
    subset_group.add_argument(
        "--per-fund-type", "-f",
        type=int,
        help="Target entities per fund type"
    )

    args = parser.parse_args()

    # Get size preset
    size_config = SUBSET_SIZES[args.size]

    # Configuration - start with preset, allow overrides
    config = Config(
        size=args.size,
        per_country=args.per_country if args.per_country else size_config['per_country'],
        per_depth=args.per_depth if args.per_depth else size_config['per_depth'],
        per_sector=args.per_sector if args.per_sector else size_config['per_sector'],
        per_category=args.per_category if args.per_category else size_config['per_category'],
        per_fund_type=args.per_fund_type if args.per_fund_type else size_config['per_fund_type'],
        per_legal_form=size_config['per_legal_form'],
    )

    # Get the data directory
    data_dir = get_lei_data_dir()

    if not data_dir.exists():
        print(f"Error: LEI data directory does not exist: {data_dir}")
        print("Please create it and place the GLEIF golden copy files there,")
        print("or use --download to fetch them automatically.")
        sys.exit(1)

    # Handle download if requested
    if args.download or args.download_only:
        try:
            lei_file, rr_file = download_gleif_data(
                output_dir=data_dir,
                lei_url=args.lei_url,
                rr_url=args.rr_url,
            )
        except Exception as e:
            print(f"Error downloading data: {e}")
            sys.exit(1)

        if args.download_only:
            print("\nDownload complete. Use without --download-only to extract subset.")
            sys.exit(0)
    else:
        # Find existing input files
        try:
            lei_file, rr_file = find_data_files(data_dir)
        except FileNotFoundError as e:
            print(f"Error: {e}")
            print("\nTip: Use --download to fetch the latest GLEIF data")
            sys.exit(1)

    # Generate output filenames (in the data directory)
    output_lei = get_subset_filename(lei_file, config.size)
    output_rr = get_subset_filename(rr_file, config.size)

    print("=" * 70)
    print(f"LEI Dataset Subset Extractor ({config.size} preset)")
    print("=" * 70)
    print(f"\nInput files:")
    print(f"  LEI data: {os.path.basename(lei_file)}")
    print(f"  Relationships: {os.path.basename(rr_file)}")
    print(f"\nOutput files:")
    print(f"  LEI subset: {os.path.basename(output_lei)}")
    print(f"  RR subset: {os.path.basename(output_rr)}")
    print(f"\nConfiguration ({config.size}):")
    print(f"  Target per country: {config.per_country}")
    print(f"  Target per depth bucket: {config.per_depth}")
    print(f"  Target per sector: {config.per_sector}")
    print(f"  Target per category: {config.per_category}")
    print(f"  Target per fund type: {config.per_fund_type}")

    # Load anchor LEIs
    anchor_leis = load_anchor_leis(data_dir)

    # Build relationship map
    parent_to_children, child_to_parent = build_relationship_map(rr_file)

    # Analyze all entities
    entities = analyze_entities(lei_file, parent_to_children, child_to_parent)
    print(f"\nTotal active entities: {len(entities):,}")

    # Select diverse subset
    selected_leis = select_subset(
        entities,
        parent_to_children,
        child_to_parent,
        config,
        anchor_leis,
    )

    # Write subset files
    write_subset(
        lei_file,
        rr_file,
        selected_leis,
        output_lei,
        output_rr
    )

    # Final summary
    lei_size = os.path.getsize(output_lei) / (1024 * 1024)
    rr_size = os.path.getsize(output_rr) / (1024 * 1024)

    print("\n" + "=" * 70)
    print("Done!")
    print("=" * 70)
    print(f"\nOutput files:")
    print(f"  {os.path.basename(output_lei)} ({lei_size:.1f} MB)")
    print(f"  {os.path.basename(output_rr)} ({rr_size:.1f} MB)")


if __name__ == "__main__":
    main()
