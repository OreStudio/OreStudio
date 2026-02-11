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


# Map full country names (as found in the ECB SSM spreadsheet) to ISO 3166-1
# alpha-2 codes. This ensures all entries in anchor_leis.json use consistent
# two-letter country codes.
COUNTRY_NAME_TO_ISO = {
    "Austria": "AT", "Belgium": "BE", "Bulgaria": "BG", "Croatia": "HR",
    "Cyprus": "CY", "Czech Republic": "CZ", "Czechia": "CZ",
    "Denmark": "DK", "Estonia": "EE", "Finland": "FI", "France": "FR",
    "Germany": "DE", "Greece": "GR", "Hungary": "HU", "Iceland": "IS",
    "Ireland": "IE", "Italy": "IT", "Latvia": "LV", "Lithuania": "LT",
    "Luxembourg": "LU", "Malta": "MT", "Netherlands": "NL", "Norway": "NO",
    "Poland": "PL", "Portugal": "PT", "Romania": "RO", "Slovakia": "SK",
    "Slovenia": "SI", "Spain": "ES", "Sweden": "SE",
}


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
            country = country.strip()
            entry["country"] = COUNTRY_NAME_TO_ISO.get(country, country)

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
            lei = row[2].strip().strip("'\"") if row[2] else ""

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

# Sovereign issuers: government treasuries, ministries of finance, and
# republics/kingdoms that issue government bonds. LEIs verified against the
# GLEIF API (2025).
SOVEREIGN_ANCHORS = [
    # --- Americas ---
    {"lei": "254900HROIFWPRGM1V77", "name": "United States Department of the Treasury", "source": "sovereign", "country": "US"},
    {"lei": "4BFD7AQU0A75QLAHK410", "name": "His Majesty the King in right of Canada", "source": "sovereign", "country": "CA"},
    {"lei": "254900EGTWEU67VP6075", "name": "United Mexican States", "source": "sovereign", "country": "MX"},
    {"lei": "254900ZFY40OYEADAP90", "name": "Federative Republic of Brazil", "source": "sovereign", "country": "BR"},
    {"lei": "549300FLZTJM5YJF8D34", "name": "Republic of Chile", "source": "sovereign", "country": "CL"},
    {"lei": "549300MHDRBVRF6B9117", "name": "Republic of Colombia", "source": "sovereign", "country": "CO"},
    # --- Europe ---
    {"lei": "ECTRVYYCEF89VWYS6K36", "name": "His Majesty's Treasury", "source": "sovereign", "country": "GB"},
    {"lei": "529900AQBND3S6YJLY83", "name": "Bundesrepublik Deutschland", "source": "sovereign", "country": "DE"},
    {"lei": "9695006J0AWHMYNZAL19", "name": "Republique Francaise", "source": "sovereign", "country": "FR"},
    {"lei": "815600DE60799F5A9309", "name": "Repubblica Italiana", "source": "sovereign", "country": "IT"},
    {"lei": "PJQDPSI1D8J2Q1IM3G17", "name": "Instituto de Credito Oficial, E.P.E.", "source": "sovereign", "country": "ES"},
    {"lei": "549300SZ25JZFHRHWD76", "name": "The Kingdom of Belgium", "source": "sovereign", "country": "BE"},
    {"lei": "529900QWWUI4XRVR7I03", "name": "Republik Osterreich", "source": "sovereign", "country": "AT"},
    {"lei": "743700M6Y2OQRVSBRD14", "name": "Republic of Finland", "source": "sovereign", "country": "FI"},
    {"lei": "549300P6U1FJ3IMP7K42", "name": "Republic of Portugal", "source": "sovereign", "country": "PT"},
    {"lei": "2138003EKTMKZ5598902", "name": "Hellenic Republic", "source": "sovereign", "country": "GR"},
    {"lei": "254900G14ALGVKORFN62", "name": "Ministerie van Financien", "source": "sovereign", "country": "NL"},
    {"lei": "549300KXBEJAOJ9OVF93", "name": "National Treasury Management Agency", "source": "sovereign", "country": "IE"},
    {"lei": "549300PTO6LS1PTM6607", "name": "Ministry of Finance on behalf of the Kingdom of Denmark", "source": "sovereign", "country": "DK"},
    {"lei": "ERE94C0BSULG2RM19605", "name": "Riksgaldskontoret", "source": "sovereign", "country": "SE"},
    {"lei": "5493003HN3EVPYH50Y28", "name": "Republic of Turkey", "source": "sovereign", "country": "TR"},
    {"lei": "529900FWX0GRR7WG5W79", "name": "Kingdom of Morocco", "source": "sovereign", "country": "MA"},
    {"lei": "213800L6VDKUM3TCM927", "name": "The Republic of Uzbekistan", "source": "sovereign", "country": "UZ"},
    # --- Asia-Pacific ---
    {"lei": "353800WZS8AXZXFUC241", "name": "Ministry of Finance Japan", "source": "sovereign", "country": "JP"},
    {"lei": "300300CHN201808MOF68", "name": "Ministry of Finance of PRC", "source": "sovereign", "country": "CN"},
    {"lei": "549300O7R7FJSJFX7786", "name": "Republic of Korea", "source": "sovereign", "country": "KR"},
    {"lei": "213800J6B7JSBDETCB42", "name": "Australian Office of Financial Management", "source": "sovereign", "country": "AU"},
    {"lei": "549300ZPF4443CZCWY12", "name": "Government of Singapore", "source": "sovereign", "country": "SG"},
    {"lei": "549300DSMAD69T7GGN13", "name": "Government of HKSAR", "source": "sovereign", "country": "HK"},
    {"lei": "529900RAHBALMYIJ3T08", "name": "Republic of the Philippines", "source": "sovereign", "country": "PH"},
    # --- Middle East / Africa ---
    {"lei": "213800T8ZHTFZIBYPE21", "name": "Ministry of Finance on behalf of the State of Israel", "source": "sovereign", "country": "IL"},
    {"lei": "378900AAFB4F17004C49", "name": "National Treasury of South Africa", "source": "sovereign", "country": "ZA"},
    {"lei": "98450060C643D82DF808", "name": "Federal Government of the UAE", "source": "sovereign", "country": "AE"},
]

# Central banks and monetary authorities. LEIs verified against the GLEIF
# API (2025).
CENTRAL_BANK_ANCHORS = [
    # --- Americas ---
    {"lei": "254900Y8NKGV541U8Q32", "name": "Federal Reserve Bank of New York", "source": "central_bank", "country": "US"},
    {"lei": "549300PN6MKI0CLP4T28", "name": "Bank of Canada", "source": "central_bank", "country": "CA"},
    {"lei": "AALGVYK6RWPEUC46UZ70", "name": "Banco Central do Brasil", "source": "central_bank", "country": "BR"},
    {"lei": "5493000PCHOG3B6S3Q85", "name": "Banco de Mexico", "source": "central_bank", "country": "MX"},
    {"lei": "549300UM1EVTYXVS1166", "name": "Banco Central de Reserva del Peru", "source": "central_bank", "country": "PE"},
    # --- Europe (ECB + Eurosystem national central banks) ---
    {"lei": "549300DTUYXVMJXZNY75", "name": "European Central Bank", "source": "central_bank", "country": "DE"},
    {"lei": "YUEDD7W89PH0FV8Q2S28", "name": "Bank of England", "source": "central_bank", "country": "GB"},
    {"lei": "529900SEOICVR2VM6Y05", "name": "Deutsche Bundesbank", "source": "central_bank", "country": "DE"},
    {"lei": "9W4ONDYI7MRRJYXY8R34", "name": "Banque de France", "source": "central_bank", "country": "FR"},
    {"lei": "549300TE4WZOT0R7UU56", "name": "Banca d'Italia", "source": "central_bank", "country": "IT"},
    {"lei": "95980020140006022422", "name": "Banco de Espana", "source": "central_bank", "country": "ES"},
    {"lei": "5493001O36CVKPVL2D48", "name": "De Nederlandsche Bank N.V.", "source": "central_bank", "country": "NL"},
    {"lei": "635400OAUSKT6BT5UZ19", "name": "Central Bank of Ireland", "source": "central_bank", "country": "IE"},
    {"lei": "54930037NWG1CCVQHF93", "name": "Banco de Portugal", "source": "central_bank", "country": "PT"},
    {"lei": "529900SL3K21WAN6X611", "name": "Bank of Greece", "source": "central_bank", "country": "GR"},
    {"lei": "7437007ALEZNCJGTGA67", "name": "Bank of Finland", "source": "central_bank", "country": "FI"},
    {"lei": "5299001Z8JOKGWV5O949", "name": "Oesterreichische Nationalbank", "source": "central_bank", "country": "AT"},
    {"lei": "222100OWMIOR5WN9MF69", "name": "Banque Centrale du Luxembourg", "source": "central_bank", "country": "LU"},
    {"lei": "5299002QI7G5XEIYAO60", "name": "Bank of Lithuania", "source": "central_bank", "country": "LT"},
    {"lei": "H0HWWEVOF2OSX76KYU55", "name": "Bank of Latvia", "source": "central_bank", "country": "LV"},
    {"lei": "6YJ6OTG0PYO45FO7Q425", "name": "Bank of Estonia", "source": "central_bank", "country": "EE"},
    {"lei": "5493002F1U5CO1UMKD70", "name": "Central Bank of Malta", "source": "central_bank", "country": "MT"},
    {"lei": "213800OEIJJPAMOFV795", "name": "Central Bank of Cyprus", "source": "central_bank", "country": "CY"},
    # --- Europe (non-Eurozone) ---
    {"lei": "549300VLYM2XZE4FJF95", "name": "Sveriges Riksbank", "source": "central_bank", "country": "SE"},
    {"lei": "549300AHYJ0OMBLIL335", "name": "Danmarks Nationalbank", "source": "central_bank", "country": "DK"},
    {"lei": "549300O6E2WAK3IAXE34", "name": "Norges Bank", "source": "central_bank", "country": "NO"},
    {"lei": "549300K5GD3JPA2LLG98", "name": "Central Bank of Iceland", "source": "central_bank", "country": "IS"},
    {"lei": "549300K7Z2KT76WQJD18", "name": "Narodowy Bank Polski", "source": "central_bank", "country": "PL"},
    {"lei": "549300DS86PEHLIYB473", "name": "Ceska Narodni Banka", "source": "central_bank", "country": "CZ"},
    {"lei": "SS58IXU92ZSUMORZFG95", "name": "Magyar Nemzeti Bank", "source": "central_bank", "country": "HU"},
    {"lei": "259400VHILGGKLBNL383", "name": "Banca Nationala a Romaniei", "source": "central_bank", "country": "RO"},
    {"lei": "74780000X0J87XKFIL77", "name": "Hrvatska Narodna Banka", "source": "central_bank", "country": "HR"},
    {"lei": "549300OY1IT4IB353V97", "name": "Narodna Banka Slovenska", "source": "central_bank", "country": "SK"},
    {"lei": "529900OID1Z3U5WRAN31", "name": "Banka Slovenije", "source": "central_bank", "country": "SI"},
    {"lei": "789000B8K6KNUMHC1Y64", "name": "Central Bank of Turkey", "source": "central_bank", "country": "TR"},
    {"lei": "M8LYVLF5DOB9X0LZOW72", "name": "Bank of Israel", "source": "central_bank", "country": "IL"},
    # --- Asia-Pacific ---
    {"lei": "353800717BLAHZMEXR15", "name": "Bank of Japan", "source": "central_bank", "country": "JP"},
    {"lei": "300300CHNPBC19481202", "name": "People's Bank of China", "source": "central_bank", "country": "CN"},
    {"lei": "RVHOHKPBCJ2GSJ37YH94", "name": "Bank of Korea", "source": "central_bank", "country": "KR"},
    {"lei": "335800IV7VCDQGIOI391", "name": "Reserve Bank of India", "source": "central_bank", "country": "IN"},
    {"lei": "5493003GYPR7VI37GG77", "name": "Reserve Bank of Australia", "source": "central_bank", "country": "AU"},
    {"lei": "54930066LV3G32L0HB48", "name": "Reserve Bank of New Zealand", "source": "central_bank", "country": "NZ"},
    {"lei": "LKAJX4ZOK2R6D9THVO59", "name": "Swiss National Bank", "source": "central_bank", "country": "CH"},
    {"lei": "54930035WQZLGC45RZ35", "name": "Monetary Authority of Singapore", "source": "central_bank", "country": "SG"},
    {"lei": "549300NROGNBV2T1GS07", "name": "Bank Negara Malaysia", "source": "central_bank", "country": "MY"},
    {"lei": "OUB893BYM4R6CVFWWL56", "name": "Bank Indonesia", "source": "central_bank", "country": "ID"},
    {"lei": "4IDQTQC5NXVM3WSFHR30", "name": "Bangko Sentral ng Pilipinas", "source": "central_bank", "country": "PH"},
    {"lei": "549300HJD00HOXYCA262", "name": "Bank of Thailand", "source": "central_bank", "country": "TH"},
    # --- Africa ---
    {"lei": "549300KEVMSOSY8T8R35", "name": "South African Reserve Bank", "source": "central_bank", "country": "ZA"},
    {"lei": "25490039BSO2ZF8JZ448", "name": "Central Bank of Kenya", "source": "central_bank", "country": "KE"},
    {"lei": "549300G1JG004Y3K8S97", "name": "Central Bank of Nigeria", "source": "central_bank", "country": "NG"},
]

# Supranational financial institutions (development banks, multilateral
# organisations). LEIs verified against the GLEIF API (2025).
SUPRANATIONAL_ANCHORS = [
    {"lei": "5493006YXS1U5GIHE750", "name": "European Investment Bank", "source": "supranational", "country": "LU"},
    {"lei": "222100W4EEAQ77386N50", "name": "European Stability Mechanism", "source": "supranational", "country": "LU"},
    {"lei": "222100OW6UHQXNHKQ143", "name": "European Financial Stability Facility", "source": "supranational", "country": "LU"},
    {"lei": "ZTMSNXROF84AHWJNKQ93", "name": "International Bank for Reconstruction and Development", "source": "supranational", "country": "US"},
    {"lei": "549300X0MVH42CY8Q105", "name": "Asian Development Bank", "source": "supranational", "country": "PH"},
    {"lei": "VKU1UKDS9E7LYLMACP54", "name": "Inter-American Development Bank", "source": "supranational", "country": "US"},
    {"lei": "549300LNCLMO3ITVCU07", "name": "African Development Bank", "source": "supranational", "country": "CI"},
    {"lei": "549300UYNXMI821WYG82", "name": "Council of Europe Development Bank", "source": "supranational", "country": "FR"},
    {"lei": "213800HYL1S7VAXG6Z48", "name": "Nordic Investment Bank", "source": "supranational", "country": "FI"},
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

    # 4. Sovereign issuers (treasuries, republics, ministries of finance)
    def add_anchor_list(anchors: list):
        nonlocal seen_leis, all_entries
        count = 0
        for e in anchors:
            if e["lei"] not in seen_leis:
                seen_leis.add(e["lei"])
                all_entries.append(e)
                count += 1
        print(f"  Added {count} new ({len(anchors) - count} already present)")
        return count

    print(f"Adding sovereign issuer LEIs ({len(SOVEREIGN_ANCHORS)} entries)")
    add_anchor_list(SOVEREIGN_ANCHORS)

    # 5. Central banks
    print(f"Adding central bank LEIs ({len(CENTRAL_BANK_ANCHORS)} entries)")
    add_anchor_list(CENTRAL_BANK_ANCHORS)

    # 6. Supranational organisations
    print(f"Adding supranational LEIs ({len(SUPRANATIONAL_ANCHORS)} entries)")
    add_anchor_list(SUPRANATIONAL_ANCHORS)

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
            {
                "id": "sovereign",
                "name": "Sovereign bond issuers (treasuries, republics)",
                "url": "https://api.gleif.org/api/v1/lei-records",
            },
            {
                "id": "central_bank",
                "name": "Central banks and monetary authorities",
                "url": "https://api.gleif.org/api/v1/lei-records",
            },
            {
                "id": "supranational",
                "name": "Supranational financial institutions",
                "url": "https://api.gleif.org/api/v1/lei-records",
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
