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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_domain_entity_create.mustache
 * To modify, update the template and regenerate.
 *
 * Market Series Identity Table
 *
 * A projection of market_series.oresmd_uri, one row per series, so a query can
 * join on the series and filter on a real column. The URI stays the only spelling
 * the system writes and the codec stays the only thing that reads it; this table is
 * written from that parse, never read back to rebuild a URI.
 *
 * The column set is the identity fields ores.marketdata declares in its codec
 * schema, one column per field the schema marks field_role::identity, plus the
 * columns that say which kind of identity the row carries. A field the type's
 * schema row does not declare is empty, because the row holds only the columns its
 * own type fills. A column is text, not a typed relational form, because the codec
 * keeps every value as the key spelled it and a projection that reinterpreted it
 * would be a second spelling of the same identity.
 *
 * The table is a current state, not a history: the series row is already temporal
 * and the identity does not change under it, so one row per series is enough and
 * the primary key is the series. The column list is checked against the codec
 * schema by build/scripts/check_marketdata_identity_columns.py, so the two cannot
 * drift.
 */

create table if not exists "ores_marketdata_market_series_identity_tbl" (
    "series_id" uuid not null,
    "tenant_id" uuid not null,
    "party_id" uuid not null,
    "identity_kind" text not null,
    "asset_class" text null,
    "instrument_type" text null,
    "quote_type" text null,
    "atm" text null,
    "cap_floor" text null,
    "ccy" text null,
    "cds_index_name" text null,
    "commodity_name" text null,
    "contract" text null,
    "contract_name" text null,
    "curve_id" text null,
    "day_counter" text null,
    "doc_clause" text null,
    "dst" text null,
    "eq_name" text null,
    "fixed_ccy" text null,
    "fixed_tenor" text null,
    "flat_ccy" text null,
    "flat_term" text null,
    "float_ccy" text null,
    "float_tenor" text null,
    "future_contract" text null,
    "fwd_start" text null,
    "identifier" text null,
    "index" text null,
    "index1" text null,
    "index2" text null,
    "index_name" text null,
    "index_tenor" text null,
    "index_term" text null,
    "offset" text null,
    "option_type" text null,
    "payer_receiver" text null,
    "qualifier" text null,
    "quote_name" text null,
    "quote_tag" text null,
    "rating_name" text null,
    "relative" text null,
    "running_spread" text null,
    "seasonality_type" text null,
    "security_id" text null,
    "seniority" text null,
    "side" text null,
    "tenor" text null,
    "term" text null,
    "time_unit" text null,
    "underlying_name" text null,
    "unit_ccy" text null,
    primary key (series_id),
    check ("series_id" <> ores_utility_nil_uuid_fn())
);



create index if not exists market_series_identity_instrument_idx
on "ores_marketdata_market_series_identity_tbl" (tenant_id, instrument_type);

create index if not exists market_series_identity_party_idx
on "ores_marketdata_market_series_identity_tbl" (tenant_id, party_id);

create index if not exists market_series_identity_ccy_idx
on "ores_marketdata_market_series_identity_tbl" (tenant_id, ccy)
where ccy is not null;

create index if not exists market_series_identity_unit_ccy_idx
on "ores_marketdata_market_series_identity_tbl" (tenant_id, unit_ccy)
where unit_ccy is not null;

create or replace function ores_marketdata_market_series_identity_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_market_series_identity_insert_trg
before insert on "ores_marketdata_market_series_identity_tbl"
for each row execute function ores_marketdata_market_series_identity_insert_fn();


-- =============================================================================
-- Row-level security: tenant isolation for Market Series Identity
-- =============================================================================
alter table ores_marketdata_market_series_identity_tbl enable row level security;

drop policy if exists market_series_identity_tbl_tenant_isolation_policy
    on ores_marketdata_market_series_identity_tbl;

create policy market_series_identity_tbl_tenant_isolation_policy
on ores_marketdata_market_series_identity_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
