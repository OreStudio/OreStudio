/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
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

-- Books, portfolios, and business units (drop first, depend on parties and lookup tables)
\ir ./refdata_books_notify_trigger_drop.sql
\ir ./refdata_books_drop.sql
\ir ./refdata_portfolios_notify_trigger_drop.sql
\ir ./refdata_portfolios_drop.sql
\ir ./refdata_business_units_notify_trigger_drop.sql
\ir ./refdata_business_units_drop.sql
\ir ./refdata_business_unit_types_notify_trigger_drop.sql
\ir ./refdata_business_unit_types_drop.sql

-- Party and counterparty contact information (drop first, depends on parties/counterparties)
\ir ./refdata_counterparty_contact_informations_notify_trigger_drop.sql
\ir ./refdata_counterparty_contact_informations_drop.sql
\ir ./refdata_party_contact_informations_notify_trigger_drop.sql
\ir ./refdata_party_contact_informations_drop.sql

-- Party and counterparty identifiers
\ir ./refdata_counterparty_identifiers_notify_trigger_drop.sql
\ir ./refdata_counterparty_identifiers_drop.sql
\ir ./refdata_party_identifiers_notify_trigger_drop.sql
\ir ./refdata_party_identifiers_drop.sql

-- Party-currency and party-country visibility junctions (drop before parties)
\ir ./refdata_party_currencies_drop.sql
\ir ./refdata_party_countries_drop.sql

-- Party-counterparty junction (drop before parties and counterparties)
\ir ./refdata_party_counterparties_drop.sql

-- Party and counterparty tables
\ir ./refdata_counterparties_notify_trigger_drop.sql
\ir ./refdata_counterparties_drop.sql
\ir ./refdata_parties_notify_trigger_drop.sql
\ir ./refdata_parties_drop.sql

-- Party reference data tables
\ir ./refdata_contact_types_notify_trigger_drop.sql
\ir ./refdata_contact_types_drop.sql
\ir ./refdata_book_statuses_notify_trigger_drop.sql
\ir ./refdata_book_statuses_drop.sql
\ir ./refdata_purpose_types_notify_trigger_drop.sql
\ir ./refdata_purpose_types_drop.sql
\ir ./refdata_party_id_schemes_notify_trigger_drop.sql
\ir ./refdata_party_id_schemes_drop.sql
\ir ./refdata_party_statuses_notify_trigger_drop.sql
\ir ./refdata_party_statuses_drop.sql
\ir ./refdata_party_types_notify_trigger_drop.sql
\ir ./refdata_party_types_drop.sql
\ir ./refdata_party_categories_notify_trigger_drop.sql
\ir ./refdata_party_categories_drop.sql

-- Currencies
\ir ./refdata_currencies_notify_trigger_drop.sql
\ir ./refdata_currencies_drop.sql

-- Lookup tables (must drop after tables that reference them)
\ir ./refdata_currency_market_tiers_notify_trigger_drop.sql
\ir ./refdata_currency_market_tiers_drop.sql
\ir ./refdata_monetary_natures_notify_trigger_drop.sql
\ir ./refdata_monetary_natures_drop.sql
\ir ./refdata_rounding_types_drop.sql

-- Countries
\ir ./refdata_countries_notify_trigger_drop.sql
\ir ./refdata_countries_drop.sql
