#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Extract anchor LEIs from regulatory lists into a single JSON file.

Parses:
  - ECB SSM Significant Institutions list (xlsx)
  - Bank of England PRA Banks list (csv)
  - Hand-curated G-SIB / major institution LEIs (US, Asia-Pacific, Canada)

Output: external/lei/anchor_leis.json

Usage:
    python lei_extract_anchor_leis.py
"""

import csv
import json
import os
import sys
from pathlib import Path

try:
    import openpyxl
except ImportError:
    print("Error: openpyxl is required. Install with: pip3 install openpyxl")
    sys.exit(1)


def get_lei_data_dir() -> Path:
    """Get the path to external/lei directory."""
    script_dir = Path(__file__).parent
    repo_root = script_dir.parent.parent.parent
    return repo_root / "external" / "lei"


def extract_ecb_sis(filepath: str) -> list:
    """
    Extract LEIs from the ECB SSM Significant Institutions list.

    Parses the 'SIs' sheet. Top-level groups have a sequential number in
    column 2; sub-entities have column 2 blank. We extract all entities
    with valid LEI codes.
    """
    entries = []
    wb = openpyxl.load_workbook(filepath, data_only=True)
    ws = wb['SIs']

    for row in ws.iter_rows(min_row=16, values_only=False):
        lei = row[2].value  # Column C (index 2)
        name = row[11].value  # Column L (index 11)
        country = row[17].value  # Column R (index 17)

        if not lei or not isinstance(lei, str):
            continue

        # LEIs are 20 chars alphanumeric; skip MFI codes for branches
        lei = lei.strip()
        if len(lei) != 20:
            continue

        entry = {
            "lei": lei,
            "name": (name or "").strip(),
            "source": "ecb_ssm",
        }
        if country:
            entry["country"] = country.strip()

        entries.append(entry)

    wb.close()
    return entries


def extract_uk_pra(filepath: str) -> list:
    """
    Extract LEIs from the Bank of England PRA Banks list.

    CSV with headers at row 10 (0-indexed row 9): Firm Name, FRN, LEI.
    Multiple sections (UK-incorporated, overseas branches, Gibraltar, EEA).
    """
    entries = []

    with open(filepath, 'r', encoding='utf-8-sig') as f:
        reader = csv.reader(f)
        in_data = False

        for row in reader:
            if len(row) < 3:
                continue

            if row[0] == 'Firm Name':
                in_data = True
                continue

            if not in_data:
                continue

            firm_name = row[0].strip()
            lei = row[2].strip() if row[2] else ""

            if not firm_name or not lei:
                continue

            # Validate LEI format (20 chars)
            if len(lei) != 20:
                continue

            entries.append({
                "lei": lei,
                "name": firm_name,
                "source": "uk_pra",
                "country": "GB",
            })

    return entries


# Hand-curated LEIs for major institutions not covered by ECB/PRA lists.
# US holding companies, Asia-Pacific G-SIBs, and Canadian banks.
MANUAL_ANCHORS = [
    # --- United States (holding companies) ---
    {"lei": "8I5DZWZKVSZI1NUHU748", "name": "JPMorgan Chase & Co.", "source": "manual", "country": "US"},
    {"lei": "9DJT3UXIJIZJI4WXO774", "name": "Bank of America Corporation", "source": "manual", "country": "US"},
    {"lei": "6SHGI4ZSSLCXXQSBB395", "name": "Citigroup Inc.", "source": "manual", "country": "US"},
    {"lei": "784F5XWPLTWKTBV3E584", "name": "The Goldman Sachs Group, Inc.", "source": "manual", "country": "US"},
    {"lei": "IGJSJL3JD5P30I6NJZ34", "name": "Morgan Stanley", "source": "manual", "country": "US"},
    {"lei": "PBLD0EJDB5FWOLXP3B76", "name": "Wells Fargo & Company", "source": "manual", "country": "US"},
    {"lei": "WFLLPEPC7FZXENRZV188", "name": "The Bank of New York Mellon Corporation", "source": "manual", "country": "US"},
    {"lei": "549300ZFEEJ2IP5VME73", "name": "State Street Corporation", "source": "manual", "country": "US"},
    # --- Asia-Pacific ---
    {"lei": "353800V2V8PUY9TK3E06", "name": "Mitsubishi UFJ Financial Group (MUFG)", "source": "manual", "country": "JP"},
    {"lei": "549300HS3WTRS6D88H32", "name": "Mizuho Financial Group", "source": "manual", "country": "JP"},
    {"lei": "35380028MYWPB6AUO129", "name": "Sumitomo Mitsui Financial Group (SMFG)", "source": "manual", "country": "JP"},
    {"lei": "5493002ERZU2K9PZDL40", "name": "Industrial and Commercial Bank of China (ICBC)", "source": "manual", "country": "CN"},
    {"lei": "54930053HGCFWVHYZX42", "name": "Bank of China Limited", "source": "manual", "country": "CN"},
    {"lei": "5493001KQW6DM7KEDR62", "name": "China Construction Bank Corporation", "source": "manual", "country": "CN"},
    {"lei": "549300E7TSGLCOVSY746", "name": "Agricultural Bank of China Limited", "source": "manual", "country": "CN"},
    # --- Canada ---
    {"lei": "ES7IP3U3RHIGC71XBU11", "name": "Royal Bank of Canada", "source": "manual", "country": "CA"},
    {"lei": "PT3QB789TSUIDF371261", "name": "The Toronto-Dominion Bank", "source": "manual", "country": "CA"},
]


def main():
    data_dir = get_lei_data_dir()

    # Find source files
    ecb_file = data_dir / "ssm.listofsupervisedentities202512.en.xlsx"
    pra_file = data_dir / "Banks List 2602.csv"

    all_entries = []
    seen_leis = set()

    # 1. ECB Significant Institutions
    if ecb_file.exists():
        print(f"Parsing ECB SSM list: {ecb_file.name}")
        ecb_entries = extract_ecb_sis(str(ecb_file))
        for e in ecb_entries:
            if e["lei"] not in seen_leis:
                seen_leis.add(e["lei"])
                all_entries.append(e)
        print(f"  Extracted {len(ecb_entries)} LEIs ({len(seen_leis)} unique)")
    else:
        print(f"Warning: ECB file not found: {ecb_file}")

    # 2. UK PRA Banks
    if pra_file.exists():
        print(f"Parsing UK PRA list: {pra_file.name}")
        pra_entries = extract_uk_pra(str(pra_file))
        added = 0
        for e in pra_entries:
            if e["lei"] not in seen_leis:
                seen_leis.add(e["lei"])
                all_entries.append(e)
                added += 1
        print(f"  Extracted {len(pra_entries)} LEIs ({added} new, {len(pra_entries) - added} duplicates)")
    else:
        print(f"Warning: UK PRA file not found: {pra_file}")

    # 3. Manual anchors (US, Asia-Pacific, Canada)
    print("Adding manual anchor LEIs (US, Asia-Pacific, Canada)")
    added = 0
    for e in MANUAL_ANCHORS:
        if e["lei"] not in seen_leis:
            seen_leis.add(e["lei"])
            all_entries.append(e)
            added += 1
    print(f"  Added {added} new ({len(MANUAL_ANCHORS) - added} already present)")

    # Write output
    output = {
        "description": "Anchor LEIs from regulatory lists for GLEIF subset extraction",
        "sources": [
            {
                "id": "ecb_ssm",
                "name": "ECB SSM Significant Institutions",
                "file": ecb_file.name if ecb_file.exists() else None,
                "url": "https://www.bankingsupervision.europa.eu/framework/supervised-banks/html/index.en.html",
            },
            {
                "id": "uk_pra",
                "name": "Bank of England PRA Regulated Banks",
                "file": pra_file.name if pra_file.exists() else None,
                "url": "https://www.bankofengland.co.uk/prudential-regulation/authorisations/which-firms-does-the-pra-regulate",
            },
            {
                "id": "manual",
                "name": "Hand-curated G-SIB and major institution LEIs",
                "url": "https://www.fsb.org/2024/11/2024-list-of-global-systemically-important-banks-g-sibs/",
            },
        ],
        "total_leis": len(all_entries),
        "leis": sorted(all_entries, key=lambda e: (e.get("country", ""), e["name"])),
    }

    output_path = data_dir / "anchor_leis.json"
    with open(output_path, 'w', encoding='utf-8', newline='\n') as f:
        json.dump(output, f, indent=2, ensure_ascii=False)
        f.write('\n')

    print(f"\nWritten {len(all_entries)} anchor LEIs to {output_path.name}")

    # Summary by source
    by_source = {}
    for e in all_entries:
        src = e["source"]
        by_source[src] = by_source.get(src, 0) + 1
    for src, count in sorted(by_source.items()):
        print(f"  {src}: {count}")


if __name__ == "__main__":
    main()
