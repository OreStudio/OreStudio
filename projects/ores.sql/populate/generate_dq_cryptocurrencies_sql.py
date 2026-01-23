#!/usr/bin/env python3
"""
Generates SQL populate script for DQ cryptocurrency artefacts.

Reads cryptocurrency data from a JSON file (symbol -> name mapping) and generates
a SQL script to populate the dq_currencies_artefact_tbl with cryptocurrency data.

The script links cryptocurrencies to their icons from the cryptocurrency-icons dataset
where available, falling back to a placeholder for unmatched currencies.

Usage:
    python3 generate_dq_cryptocurrencies_sql.py
    python3 generate_dq_cryptocurrencies_sql.py --source-file path/to/cryptocurrencies.json
    python3 generate_dq_cryptocurrencies_sql.py --output-file path/to/output.sql
"""

import argparse
import json
import os
import sys

# Default configuration
DEFAULT_CONFIG = {
    'dataset_name': 'Cryptocurrencies Top 12243 Coins',
    'subject_area_name': 'Cryptocurrencies',
    'domain_name': 'Reference Data',
    'icons_dataset_name': 'Cryptocurrency Icon Images',
    'icons_subject_area_name': 'Cryptocurrencies',
    'icons_domain_name': 'Reference Data',
    'source_file': 'projects/ores.sql/populate/data/cryptocurrencies/cryptocurrencies.json',
    'output_file': 'projects/ores.sql/populate/dq_cryptocurrencies_artefact_populate.sql',
}

# Default values for cryptocurrency fields
CRYPTO_DEFAULTS = {
    'numeric_code': '',           # No ISO numeric codes for crypto
    'fraction_symbol': '',        # No standard fraction symbol
    'fractions_per_unit': 100000000,  # 8 decimal places (satoshi-like)
    'rounding_type': 'standard',
    'rounding_precision': 8,
    'format': '#,##0.00000000',
}

# Top 100 cryptocurrencies by market cap (January 2026)
# Source: CoinMarketCap, CoinGecko
# First 20 are classified as 'crypto.major', rest as 'crypto.minor'
TOP_100_CRYPTOS = [
    # Top 20 (major)
    'BTC',   # 1. Bitcoin
    'ETH',   # 2. Ethereum
    'USDT',  # 3. Tether
    'XRP',   # 4. Ripple
    'BNB',   # 5. Binance Coin
    'SOL',   # 6. Solana
    'USDC',  # 7. USD Coin
    'DOGE',  # 8. Dogecoin
    'ADA',   # 9. Cardano
    'TRX',   # 10. TRON
    'LINK',  # 11. Chainlink
    'XLM',   # 12. Stellar
    'HBAR',  # 13. Hedera
    'SUI',   # 14. Sui
    'HYPE',  # 15. Hyperliquid
    'BCH',   # 16. Bitcoin Cash
    'XMR',   # 17. Monero
    'ZEC',   # 18. Zcash
    'PAXG',  # 19. PAX Gold
    'AVAX',  # 20. Avalanche
    # 21-50
    'TON',   # 21. Toncoin
    'SHIB',  # 22. Shiba Inu
    'DOT',   # 23. Polkadot
    'LTC',   # 24. Litecoin
    'UNI',   # 25. Uniswap
    'LEO',   # 26. UNUS SED LEO
    'DAI',   # 27. Dai
    'NEAR',  # 28. NEAR Protocol
    'APT',   # 29. Aptos
    'ICP',   # 30. Internet Computer
    'RNDR',  # 31. Render
    'POL',   # 32. Polygon (formerly MATIC)
    'ETC',   # 33. Ethereum Classic
    'TAO',   # 34. Bittensor
    'AAVE',  # 35. Aave
    'VET',   # 36. VeChain
    'FET',   # 37. Fetch.ai
    'MNT',   # 38. Mantle
    'FIL',   # 39. Filecoin
    'ARB',   # 40. Arbitrum
    'ATOM',  # 41. Cosmos
    'OP',    # 42. Optimism
    'KAS',   # 43. Kaspa
    'INJ',   # 44. Injective
    'IMX',   # 45. Immutable
    'STX',   # 46. Stacks
    'THETA', # 47. Theta Network
    'RUNE',  # 48. THORChain
    'GRT',   # 49. The Graph
    'SEI',   # 50. Sei
    # 51-100
    'FTM',   # 51. Fantom
    'SAND',  # 52. The Sandbox
    'MANA',  # 53. Decentraland
    'ALGO',  # 54. Algorand
    'AXS',   # 55. Axie Infinity
    'FLOW',  # 56. Flow
    'CFX',   # 57. Conflux
    'GALA',  # 58. Gala
    'NEO',   # 59. NEO
    'KAVA',  # 60. Kava
    'XTZ',   # 61. Tezos
    'EGLD',  # 62. MultiversX
    'ROSE',  # 63. Oasis Network
    'LUNC',  # 64. Terra Classic
    'MIOTA', # 65. IOTA
    'CRO',   # 66. Cronos
    'QNT',   # 67. Quant
    'FLOKI', # 68. Floki
    'SNX',   # 69. Synthetix
    'CAKE',  # 70. PancakeSwap
    'ORDI',  # 71. ORDI
    'CHZ',   # 72. Chiliz
    'PEPE',  # 73. Pepe
    'WIF',   # 74. dogwifhat
    'BONK',  # 75. Bonk
    'ENS',   # 76. Ethereum Name Service
    'TUSD',  # 77. TrueUSD
    'OSMO',  # 78. Osmosis
    'XEC',   # 79. eCash
    'MINA',  # 80. Mina Protocol
    'DASH',  # 81. Dash
    'APE',   # 82. ApeCoin
    'BLUR',  # 83. Blur
    'CRV',   # 84. Curve DAO Token
    'LDO',   # 85. Lido DAO
    'MKR',   # 86. Maker
    'COMP',  # 87. Compound
    'ZIL',   # 88. Zilliqa
    'ENJ',   # 89. Enjin Coin
    'BAT',   # 90. Basic Attention Token
    'NEXO',  # 91. Nexo
    '1INCH', # 92. 1inch
    'WOO',   # 93. WOO Network
    'GMT',   # 94. STEPN
    'DYDX',  # 95. dYdX
    'AR',    # 96. Arweave
    'ASTR',  # 97. Astar
    'CELO',  # 98. Celo
    'AGIX',  # 99. SingularityNET
    'OCEAN', # 100. Ocean Protocol
]

MAJOR_CRYPTOS = set(TOP_100_CRYPTOS[:20])

# Special symbols for well-known cryptocurrencies
SPECIAL_SYMBOLS = {
    'BTC': '₿',
    'ETH': 'Ξ',
    'LTC': 'Ł',
    'DOGE': 'Ð',
    'XRP': '✕',
}


def get_header(dataset_name: str, subject_area_name: str, domain_name: str,
               icons_dataset_name: str, icons_subject_area_name: str,
               icons_domain_name: str, source_file: str, script_name: str) -> str:
    return f"""/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * Data Quality Cryptocurrencies Artefact Population Script
 *
 * Populates the dq_currencies_artefact_tbl with cryptocurrency data.
 * Links cryptocurrencies to their icons from the cryptocurrency-icons dataset.
 * This script is idempotent.
 *
 * Dataset: {dataset_name}
 * Subject Area: {subject_area_name}
 * Domain: {domain_name}
 *
 * This file was auto-generated from {source_file}
 * by {script_name}
 *
 * To regenerate, run:
 *   python3 {script_name}
 */

set schema 'ores';

DO $$
declare
    v_dataset_id uuid;
    v_icons_dataset_id uuid;
    v_placeholder_image_id uuid;
    v_count integer := 0;
begin
    -- Get the cryptocurrencies dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where name = '{dataset_name}'
      and subject_area_name = '{subject_area_name}'
      and domain_name = '{domain_name}'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: {dataset_name}';
    end if;

    -- Get the cryptocurrency icons dataset ID (for linking images)
    select id into v_icons_dataset_id
    from ores.dq_datasets_tbl
    where name = '{icons_dataset_name}'
      and subject_area_name = '{icons_subject_area_name}'
      and domain_name = '{icons_domain_name}'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_icons_dataset_id is null then
        raise exception 'Dataset not found: {icons_dataset_name}';
    end if;

    -- Get a placeholder image (use 'xx' flag from the flags dataset)
    select image_id into v_placeholder_image_id
    from ores.dq_images_artefact_tbl
    where key = 'xx'
    limit 1;

    if v_placeholder_image_id is null then
        raise warning 'Placeholder image (xx) not found - cryptocurrencies without icons will have NULL image_id';
    end if;

    -- Clear existing cryptocurrencies for this dataset (idempotency)
    delete from ores.dq_currencies_artefact_tbl
    where dataset_id = v_dataset_id;

    raise notice 'Populating cryptocurrencies for dataset: {dataset_name}';

    -- Insert cryptocurrencies with icon links
    -- Icons are keyed by lowercase symbol (e.g., 'btc', 'eth')
    insert into ores.dq_currencies_artefact_tbl (
        dataset_id, iso_code, version, name, numeric_code, symbol, fraction_symbol,
        fractions_per_unit, rounding_type, rounding_precision, format, currency_type, image_id
    )
    select
        v_dataset_id,
        c.iso_code,
        0,
        c.name,
        c.numeric_code,
        c.symbol,
        c.fraction_symbol,
        c.fractions_per_unit,
        c.rounding_type,
        c.rounding_precision,
        c.format,
        c.currency_type,
        coalesce(i.image_id, v_placeholder_image_id)
    from (values
"""


def get_footer(dataset_name: str, count: int) -> str:
    return f"""    ) as c(iso_code, name, numeric_code, symbol, fraction_symbol, fractions_per_unit, rounding_type, rounding_precision, format, currency_type)
    left join ores.dq_images_artefact_tbl i
        on i.dataset_id = v_icons_dataset_id
        and i.key = lower(c.iso_code);

    get diagnostics v_count = row_count;

    raise notice 'Successfully populated % cryptocurrencies for dataset: {dataset_name}', v_count;

    -- Report count of cryptocurrencies with icons
    raise notice 'Cryptocurrencies with matching icons: %', (
        select count(*)
        from ores.dq_currencies_artefact_tbl
        where dataset_id = v_dataset_id
          and image_id is not null
    );
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\\echo ''
\\echo '--- DQ Cryptocurrencies Summary ---'

select 'Total DQ Cryptocurrencies' as metric, count(*) as count
from ores.dq_currencies_artefact_tbl c
join ores.dq_datasets_tbl d on c.dataset_id = d.id
where d.name = '{dataset_name}'
union all
select 'Major Cryptocurrencies (crypto.major)', count(*)
from ores.dq_currencies_artefact_tbl c
join ores.dq_datasets_tbl d on c.dataset_id = d.id
where d.name = '{dataset_name}'
  and c.currency_type = 'crypto.major'
union all
select 'Minor Cryptocurrencies (crypto.minor)', count(*)
from ores.dq_currencies_artefact_tbl c
join ores.dq_datasets_tbl d on c.dataset_id = d.id
where d.name = '{dataset_name}'
  and c.currency_type = 'crypto.minor'
union all
select 'Cryptocurrencies with Icons', count(*)
from ores.dq_currencies_artefact_tbl c
join ores.dq_datasets_tbl d on c.dataset_id = d.id
where d.name = '{dataset_name}'
  and c.image_id is not null
union all
select 'Cryptocurrencies without Icons', count(*)
from ores.dq_currencies_artefact_tbl c
join ores.dq_datasets_tbl d on c.dataset_id = d.id
where d.name = '{dataset_name}'
  and c.image_id is null;
"""


def escape_sql_string(s: str) -> str:
    """Escape single quotes for SQL."""
    return s.replace("'", "''")


def get_symbol(iso_code: str) -> str:
    """Get display symbol for a cryptocurrency."""
    return SPECIAL_SYMBOLS.get(iso_code, iso_code)


def get_currency_type(iso_code: str) -> str:
    """Get currency type classification for a cryptocurrency."""
    return 'crypto.major' if iso_code in MAJOR_CRYPTOS else 'crypto.minor'


def generate_value_row(iso_code: str, name: str, is_last: bool) -> str:
    """Generate a single VALUES row for a cryptocurrency."""
    safe_name = escape_sql_string(name)
    symbol = escape_sql_string(get_symbol(iso_code))
    safe_iso_code = escape_sql_string(iso_code)
    currency_type = get_currency_type(iso_code)

    defaults = CRYPTO_DEFAULTS
    comma = '' if is_last else ','

    return f"        ('{safe_iso_code}', '{safe_name}', '{defaults['numeric_code']}', '{symbol}', '{defaults['fraction_symbol']}', {defaults['fractions_per_unit']}, '{defaults['rounding_type']}', {defaults['rounding_precision']}, '{defaults['format']}', '{currency_type}'){comma}\n"


def main():
    parser = argparse.ArgumentParser(
        description='Generate SQL populate script for DQ cryptocurrency artefacts.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s
  %(prog)s --source-file path/to/cryptocurrencies.json
  %(prog)s --output-file path/to/output.sql
  %(prog)s --top-only --dataset-name 'Cryptocurrencies Top 100 Coins'
        """
    )

    parser.add_argument('--source-file', '-s',
                        default=DEFAULT_CONFIG['source_file'],
                        help='Path to cryptocurrencies.json file')
    parser.add_argument('--output-file', '-o',
                        default=DEFAULT_CONFIG['output_file'],
                        help='Output SQL file path')
    parser.add_argument('--dataset-name', '-n',
                        default=DEFAULT_CONFIG['dataset_name'],
                        help='Name of the dataset in dq_datasets_tbl')
    parser.add_argument('--subject-area',
                        default=DEFAULT_CONFIG['subject_area_name'],
                        help='Subject area name')
    parser.add_argument('--domain',
                        default=DEFAULT_CONFIG['domain_name'],
                        help='Domain name')
    parser.add_argument('--top-only', '-t',
                        action='store_true',
                        help='Generate only top 100 cryptocurrencies from TOP_100_CRYPTOS list')

    args = parser.parse_args()

    # Validate source file
    if not os.path.exists(args.source_file):
        print(f"Error: Source file '{args.source_file}' does not exist.", file=sys.stderr)
        sys.exit(1)

    # Load JSON data
    print(f"Loading cryptocurrencies from {args.source_file}...")
    with open(args.source_file, 'r', encoding='utf-8') as f:
        data = json.load(f)

    if not isinstance(data, dict):
        print(f"Error: Expected JSON object with symbol -> name mappings.", file=sys.stderr)
        sys.exit(1)

    # Filter to top 100 if requested
    if args.top_only:
        top_100_set = set(TOP_100_CRYPTOS)
        filtered_data = {k: v for k, v in data.items() if k in top_100_set}
        # Sort by rank in TOP_100_CRYPTOS list
        rank_map = {symbol: i for i, symbol in enumerate(TOP_100_CRYPTOS)}
        sorted_items = sorted(filtered_data.items(), key=lambda x: rank_map.get(x[0], 999))
        print(f"  Filtered to top {len(sorted_items)} cryptocurrencies.")
    else:
        # Sort by symbol for consistent output
        sorted_items = sorted(data.items(), key=lambda x: x[0])

    print(f"Configuration:")
    print(f"  Dataset:      {args.dataset_name}")
    print(f"  Subject Area: {args.subject_area}")
    print(f"  Domain:       {args.domain}")
    print(f"  Source:       {args.source_file}")
    print(f"  Output:       {args.output_file}")
    print(f"  Found {len(sorted_items)} cryptocurrencies.")
    print()

    # Generate SQL
    script_name = os.path.basename(__file__)
    with open(args.output_file, 'w', encoding='utf-8') as f:
        f.write(get_header(
            args.dataset_name,
            args.subject_area,
            args.domain,
            DEFAULT_CONFIG['icons_dataset_name'],
            DEFAULT_CONFIG['icons_subject_area_name'],
            DEFAULT_CONFIG['icons_domain_name'],
            args.source_file,
            script_name
        ))

        for i, (iso_code, name) in enumerate(sorted_items):
            is_last = (i == len(sorted_items) - 1)
            f.write(generate_value_row(iso_code, name, is_last))

        f.write(get_footer(args.dataset_name, len(sorted_items)))

    # Count majors and minors
    major_count = sum(1 for iso_code, _ in sorted_items if iso_code in MAJOR_CRYPTOS)
    minor_count = len(sorted_items) - major_count

    print(f"Successfully generated {args.output_file}")
    print(f"  Total cryptocurrencies: {len(sorted_items)}")
    print(f"  Major (crypto.major):   {major_count}")
    print(f"  Minor (crypto.minor):   {minor_count}")


if __name__ == '__main__':
    main()
