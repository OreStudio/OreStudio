/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
 * Cryptocurrency Methodology Population Script
 *
 * Auto-generated from external/crypto/manifest.json
 * This script is idempotent.
 */


-- =============================================================================
-- Cryptocurrency Data Sourcing Methodologies
-- =============================================================================

\echo '--- Cryptocurrency Methodologies ---'

select ores_dq_methodologies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'GitHub Cryptocurrency Icons Download',
    'SVG images downloaded from spothq/cryptocurrency-icons GitHub repository',
    'https://github.com/spothq/cryptocurrency-icons',
    'Last Download: 2026-01-20

Data Sourcing and Generation Steps:

1. SOURCE DATA DOWNLOAD (Cryptocurrencies)
   Source: https://github.com/crypti/cryptocurrencies
   Format: JSON mapping symbol -> name
   Command: curl -o cryptocurrencies.json https://raw.githubusercontent.com/crypti/cryptocurrencies/master/cryptocurrencies.json
   Target: external/crypto/cryptocurrencies/cryptocurrencies.json

2. SOURCE DATA DOWNLOAD (Icons)
   Source: https://github.com/spothq/cryptocurrency-icons
   License: MIT
   Manual: Download SVG icons to external/crypto/cryptocurrency-icons/

3. GENERATE ICON IMAGES SQL
   Script: projects/ores.codegen/src/images_generate_sql.py
   Command: python3 images_generate_sql.py --config crypto
   Output: projects/ores.sql/populate/crypto/crypto_images_artefact_populate.sql

4. CURRENCY POPULATE FILES
   The currency artefact files were manually created:
     - crypto_currencies_large_artefact_populate.sql (~12K coins)
     - crypto_currencies_small_artefact_populate.sql (top 100 coins)

   These files read from the cryptocurrencies.json and link to icons.
   To update, edit the files directly or regenerate from source.

5. COMMIT GENERATED SQL
   git add projects/ores.sql/populate/crypto/
   git commit -m "[sql] Regenerate cryptocurrency reference data"

DATASET STRUCTURE
-----------------
Output files (in projects/ores.sql/populate/crypto/):
  - crypto.sql (master include)
  - crypto_methodology_populate.sql (methodology definitions)
  - crypto_dataset_populate.sql (dataset definitions)
  - crypto_dataset_tag_populate.sql (dataset tags)
  - crypto_images_artefact_populate.sql (icon images)
  - crypto_currencies_large_artefact_populate.sql (all ~12K coins)
  - crypto_currencies_small_artefact_populate.sql (top 100 coins)

DATASET METHODOLOGY
-------------------
The data is organized into two size variants:

Large Dataset (~12K coins):
  - All cryptocurrencies from the source JSON
  - Sorted alphabetically by symbol
  - Classified as crypto.major (top 20) or crypto.minor (rest)

Small Dataset (Top 100):
  - Top 100 cryptocurrencies by market capitalization
  - Sorted by market cap rank
  - Same classification: crypto.major (top 20) or crypto.minor (21-100)

CURRENCY CLASSIFICATION
-----------------------
crypto.major: Top 20 cryptocurrencies by market cap
  BTC, ETH, USDT, XRP, BNB, SOL, USDC, DOGE, ADA, TRX,
  LINK, XLM, HBAR, SUI, HYPE, BCH, XMR, ZEC, PAXG, AVAX

crypto.minor: All other cryptocurrencies

ICON LINKING
------------
Icons are keyed by lowercase symbol (e.g., ''btc'', ''eth'').
When populating currencies, the script joins to the icons dataset
to link each cryptocurrency to its icon. Cryptocurrencies without
matching icons use a placeholder image (flag ''xx'').

SPECIAL SYMBOLS
---------------
Some cryptocurrencies have special Unicode symbols:
  BTC: ₿ (Bitcoin)
  ETH: Ξ (Ethereum)
  LTC: Ł (Litecoin)
  DOGE: Ð (Dogecoin)
  XRP: ✕ (Ripple)'
);

select ores_dq_methodologies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'GitHub Cryptocurrencies JSON Download',
    'Cryptocurrency symbol-to-name mappings from crypti/cryptocurrencies GitHub repository',
    'https://github.com/crypti/cryptocurrencies',
    'Last Download: 2026-01-20

Data Sourcing and Generation Steps:

1. SOURCE DATA DOWNLOAD (Cryptocurrencies)
   Source: https://github.com/crypti/cryptocurrencies
   Format: JSON mapping symbol -> name
   Command: curl -o cryptocurrencies.json https://raw.githubusercontent.com/crypti/cryptocurrencies/master/cryptocurrencies.json
   Target: external/crypto/cryptocurrencies/cryptocurrencies.json

2. SOURCE DATA DOWNLOAD (Icons)
   Source: https://github.com/spothq/cryptocurrency-icons
   License: MIT
   Manual: Download SVG icons to external/crypto/cryptocurrency-icons/

3. GENERATE ICON IMAGES SQL
   Script: projects/ores.codegen/src/images_generate_sql.py
   Command: python3 images_generate_sql.py --config crypto
   Output: projects/ores.sql/populate/crypto/crypto_images_artefact_populate.sql

4. CURRENCY POPULATE FILES
   The currency artefact files were manually created:
     - crypto_currencies_large_artefact_populate.sql (~12K coins)
     - crypto_currencies_small_artefact_populate.sql (top 100 coins)

   These files read from the cryptocurrencies.json and link to icons.
   To update, edit the files directly or regenerate from source.

5. COMMIT GENERATED SQL
   git add projects/ores.sql/populate/crypto/
   git commit -m "[sql] Regenerate cryptocurrency reference data"

DATASET STRUCTURE
-----------------
Output files (in projects/ores.sql/populate/crypto/):
  - crypto.sql (master include)
  - crypto_methodology_populate.sql (methodology definitions)
  - crypto_dataset_populate.sql (dataset definitions)
  - crypto_dataset_tag_populate.sql (dataset tags)
  - crypto_images_artefact_populate.sql (icon images)
  - crypto_currencies_large_artefact_populate.sql (all ~12K coins)
  - crypto_currencies_small_artefact_populate.sql (top 100 coins)

DATASET METHODOLOGY
-------------------
The data is organized into two size variants:

Large Dataset (~12K coins):
  - All cryptocurrencies from the source JSON
  - Sorted alphabetically by symbol
  - Classified as crypto.major (top 20) or crypto.minor (rest)

Small Dataset (Top 100):
  - Top 100 cryptocurrencies by market capitalization
  - Sorted by market cap rank
  - Same classification: crypto.major (top 20) or crypto.minor (21-100)

CURRENCY CLASSIFICATION
-----------------------
crypto.major: Top 20 cryptocurrencies by market cap
  BTC, ETH, USDT, XRP, BNB, SOL, USDC, DOGE, ADA, TRX,
  LINK, XLM, HBAR, SUI, HYPE, BCH, XMR, ZEC, PAXG, AVAX

crypto.minor: All other cryptocurrencies

ICON LINKING
------------
Icons are keyed by lowercase symbol (e.g., ''btc'', ''eth'').
When populating currencies, the script joins to the icons dataset
to link each cryptocurrency to its icon. Cryptocurrencies without
matching icons use a placeholder image (flag ''xx'').

SPECIAL SYMBOLS
---------------
Some cryptocurrencies have special Unicode symbols:
  BTC: ₿ (Bitcoin)
  ETH: Ξ (Ethereum)
  LTC: Ł (Litecoin)
  DOGE: Ð (Dogecoin)
  XRP: ✕ (Ripple)'
);

