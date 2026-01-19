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
    'dataset_name': 'Cryptocurrency Reference Data',
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

# Top 20 cryptocurrencies by market cap (January 2026)
# Source: https://staxpayments.com/blog/most-popular-cryptocurrencies/
# These are classified as 'crypto.major', all others as 'crypto.minor'
MAJOR_CRYPTOS = {
    'BTC',   # Bitcoin
    'ETH',   # Ethereum
    'USDT',  # Tether
    'XRP',   # Ripple
    'BNB',   # Binance Coin
    'SOL',   # Solana
    'USDC',  # USD Coin
    'DOGE',  # Dogecoin
    'ADA',   # Cardano
    'TRX',   # TRON
    'LINK',  # Chainlink
    'XLM',   # Stellar
    'HBAR',  # Hedera
    'SUI',   # Sui
    'HYPE',  # Hyperliquid
    'BCH',   # Bitcoin Cash
    'XMR',   # Monero
    'ZEC',   # Zcash
    'PAXG',  # PAX Gold
    'AVAX',  # Avalanche
}

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

    -- Report cryptocurrencies with icons
    raise notice 'Cryptocurrencies with matching icons:';
    perform iso_code
    from ores.dq_currencies_artefact_tbl
    where dataset_id = v_dataset_id
      and image_id is not null;
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
