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
 * One-shot migration: market_series and feed_bindings ORE columns -> oresmd_uri
 *
 * The market data identity moves from the ORE key decomposition
 * (series_type/metric/qualifier, plus the coarse asset_class /
 * series_subclass / is_scalar taxonomy) to the canonical oresmd URI, and
 * feed_bindings' ore_key column follows. The URI is read and written
 * end-to-end; validation lives in the oresmd layer, not in SQL checks or
 * stored columns.
 *
 * This script backfills the new oresmd_uri column of both tables from the
 * inverse projection of the old identity columns, using exactly the same
 * mapping the new import boundary uses (oresmd_projections::from_ore_key /
 * from_index_name, serialised by oresmd_parser::to_uri):
 *
 *  - market_series RATES/YIELD rows (the ir_curve_feed auto-created
 *    identity, qualifier "CCY/FAMILY[-TENOR]") project to the fixing URI
 *    of that index, e.g. 'USD/SOFR-FOMC' ->
 *    'oresmd://ir/usd?index=sofr&tenor=fomc&type=fixing'.
 *
 *  - The seed YieldCurve/DISCOUNT/USD/SOFR-FOMC row (the republish
 *    service's output series) projects to the discount curve URI
 *    'oresmd://ir/usd?index=sofr&tenor=fomc&role=discount&type=curve'.
 *
 *  - FIXING/RATE rows (the fixings import) project via the index-name
 *    mapping, e.g. 'USD-LIBOR-3M' ->
 *    'oresmd://ir/usd?index=libor&tenor=3m&type=fixing'.
 *
 *  - feed_bindings ore_keys project via the same two paths: the
 *    RATES/YIELD/<qualifier> shape through the feed-qualifier mapping,
 *    everything else through the full ORE-key inverse projection.
 *
 *  - Every other row goes through the general inverse projection of its
 *    reconstructed key (series_type/metric/qualifier, or the raw ore_key).
 *
 * Rows with no mapping (BOND, the option/capfloor families, malformed or
 * short keys) cannot be represented as a URI and are hard-deleted, with a
 * per-table notice -- the same drop-with-warning contract the new import
 * boundary applies. Deleting the old identity columns without dropping
 * those rows would violate the new NOT NULL constraint.
 *
 * The old identity checks and the natural-key unique indexes on the ORE
 * columns are dropped; the unique indexes move to the URI. Two old
 * identities that project to the same URI (e.g. a FIXING row and a
 * RATES/YIELD row of the same index) collide at the new unique index
 * creation, which fails loudly and rolls the whole transaction back --
 * the collision must be resolved in the data before rerunning.
 *
 * PREREQUISITES (against a database that has NOT been recreated from the
 * new scripts):
 *   1. Run this script BEFORE applying the new create scripts: the new
 *      create script's unique index on oresmd_uri cannot be built on the
 *      old schema (no such column).
 *
 * After this script, the new create scripts apply cleanly over the
 * migrated tables (their statements are all idempotent).
 *
 * On a freshly recreated database the old columns do not exist and this
 * script is a guarded no-op. This script is idempotent.
 */

\echo '--- Migrating market_series and feed_bindings to oresmd URIs ---'

-- The three projection helpers below are migration-only: they mirror
-- oresmd_projections::from_ore_key / from_index_name and the feed-qualifier
-- decomposition for the RATES/YIELD identity, serialised in
-- oresmd_parser::to_uri order (index, tenor, role, type, metric, quote,
-- point). They are dropped at the end so the migrated schema matches a
-- fresh recreate exactly.

-- The inverse projection of an ORE quote key, serialised as an oresmd URI.
-- Mirrors oresmd_projections::from_ore_key + oresmd_parser::to_uri; returns
-- NULL for every key the C++ layer rejects (unmappable types, wrong
-- segment count, unknown metric/family).
create or replace function ores_marketdata_oresmd_uri_from_ore_key_fn(p_key text)
returns text
language plpgsql
immutable
as $fn$
declare
    v_parts text[];
    v_type text;
    v_metric text;
begin
    if p_key is null then
        return null;
    end if;

    v_parts := string_to_array(p_key, '/');
    if v_parts is null or cardinality(v_parts) < 2 then
        return null;
    end if;
    v_type := upper(v_parts[1]);
    v_metric := upper(v_parts[2]);

    -- FX spot: FX/RATE/CCY1/CCY2 (scalar, no point).
    if v_type = 'FX' and cardinality(v_parts) = 4 and v_metric = 'RATE'
        and v_parts[3] ~ '^[A-Za-z]{3}$' and v_parts[4] ~ '^[A-Za-z]{3}$' then
        return 'oresmd://fx/' || lower(v_parts[3] || v_parts[4])
            || '?type=quote&quote=spot';
    end if;

    -- FX forward: FXFWD/RATE/CCY1/CCY2/TENOR.
    if v_type = 'FXFWD' and cardinality(v_parts) = 5 and v_metric = 'RATE'
        and v_parts[3] ~ '^[A-Za-z]{3}$' and v_parts[4] ~ '^[A-Za-z]{3}$' then
        return 'oresmd://fx/' || lower(v_parts[3] || v_parts[4])
            || '?type=quote&quote=fwd&point=' || lower(v_parts[5]);
    end if;

    -- IR swap: IR_SWAP/METRIC/CCY/SETTLE/TENOR/POINT. The settlement
    -- segment is accepted verbatim and dropped; the identifier has no
    -- settle field.
    if v_type = 'IR_SWAP' and cardinality(v_parts) = 6
        and v_metric in ('RATE', 'PRICE', 'BASIS_SPREAD', 'RATIO', 'YIELD_SPREAD') then
        return 'oresmd://ir/' || lower(v_parts[3])
            || '?tenor=' || lower(v_parts[5])
            || '&type=quote&metric=' || lower(v_parts[2])
            || '&quote=ir_swap&point=' || lower(v_parts[6]);
    end if;

    -- IR discount curve: DISCOUNT/METRIC/CCY/CURVE_ID/POINT, where
    -- CURVE_ID = CCY + TENOR.
    if v_type = 'DISCOUNT' and cardinality(v_parts) = 5
        and v_metric in ('RATE', 'PRICE', 'BASIS_SPREAD', 'RATIO', 'YIELD_SPREAD')
        and length(v_parts[4]) > length(v_parts[3])
        and upper(left(v_parts[4], length(v_parts[3]))) = upper(v_parts[3]) then
        return 'oresmd://ir/' || lower(v_parts[3])
            || '?tenor=' || lower(substring(v_parts[4] from length(v_parts[3]) + 1))
            || '&type=quote&metric=' || lower(v_parts[2])
            || '&quote=discount&point=' || lower(v_parts[5]);
    end if;

    -- IR indexed families: TYPE/METRIC/CCY/INDEX/TENOR/POINT.
    if v_type in ('MM', 'FRA', 'IMM_FRA', 'BASIS_SWAP', 'ZERO', 'MM_FUTURE', 'OI_FUTURE')
        and cardinality(v_parts) = 6
        and v_metric in ('RATE', 'PRICE', 'BASIS_SPREAD', 'RATIO', 'YIELD_SPREAD')
        and lower(v_parts[4]) in ('libor', 'euribor', 'sofr', 'estr', 'sonia', 'tona',
            'saron', 'aonia', 'corra', 'honia', 'sora', 'swestr', 'nowa', 'kofr',
            'mibor', 'zaronia', 'destr', 'polonia', 'nzonia', 'shibor', 'tiie',
            'taibor') then
        return 'oresmd://ir/' || lower(v_parts[3])
            || '?index=' || lower(v_parts[4])
            || '&tenor=' || lower(v_parts[5])
            || '&type=quote&quote=' || lower(v_type)
            || '&metric=' || lower(v_parts[2])
            || '&point=' || lower(v_parts[6]);
    end if;

    -- IR no-index families: TYPE/METRIC/CCY/TENOR/POINT.
    if v_type in ('CC_BASIS_SWAP', 'CC_FIX_FLOAT_SWAP', 'BMA_SWAP')
        and cardinality(v_parts) = 5
        and v_metric in ('RATE', 'PRICE', 'BASIS_SPREAD', 'RATIO', 'YIELD_SPREAD') then
        return 'oresmd://ir/' || lower(v_parts[3])
            || '?tenor=' || lower(v_parts[4])
            || '&type=quote&quote=' || lower(v_type)
            || '&metric=' || lower(v_parts[2])
            || '&point=' || lower(v_parts[5]);
    end if;

    -- IR swaption: SWAPTION/MODEL/CCY/EXPIRY/TENOR/STRIKE. The model rides
    -- in the vol struct, serialised into the point; the tenor is a first-class
    -- parameter, emitted before type (the to_uri parameter order).
    if v_type = 'SWAPTION' and cardinality(v_parts) = 6
        and upper(v_parts[2]) in ('RATE_LNVOL', 'RATE_NVOL', 'RATE_SLNVOL', 'SHIFT', 'PRICE') then
        return 'oresmd://ir/' || lower(v_parts[3])
            || '?tenor=' || lower(v_parts[5])
            || '&type=vol&point=' || lower(v_parts[4]) || ',' || lower(v_parts[5])
            || ',' || lower(v_parts[6]);
    end if;

    -- Equity spot: EQUITY/PRICE/TICKER/CCY (scalar, no point).
    if v_type = 'EQUITY' and cardinality(v_parts) = 4 and v_metric = 'PRICE' then
        return 'oresmd://equity/' || lower(v_parts[3])
            || '?ccy=' || lower(v_parts[4]) || '&type=quote&quote=spot';
    end if;

    -- Equity curves: EQUITY_FWD/PRICE/TICKER/CCY/TENOR,
    -- EQUITY_DIVIDEND/RATE/TICKER/CCY/TENOR.
    if v_type = 'EQUITY_FWD' and cardinality(v_parts) = 5 and v_metric = 'PRICE' then
        return 'oresmd://equity/' || lower(v_parts[3])
            || '?ccy=' || lower(v_parts[4])
            || '&type=quote&quote=fwd&point=' || lower(v_parts[5]);
    end if;
    if v_type = 'EQUITY_DIVIDEND' and cardinality(v_parts) = 5 and v_metric = 'RATE' then
        return 'oresmd://equity/' || lower(v_parts[3])
            || '?ccy=' || lower(v_parts[4])
            || '&type=quote&quote=dividend&point=' || lower(v_parts[5]);
    end if;

    -- Commodity spot: COMMODITY/PRICE/CODE/CCY (scalar, no point).
    if v_type = 'COMMODITY' and cardinality(v_parts) = 4 and v_metric = 'PRICE' then
        return 'oresmd://commodity/' || lower(v_parts[3])
            || '?ccy=' || lower(v_parts[4]) || '&type=quote&quote=spot';
    end if;

    -- Commodity curves: COMMODITY_FWD/PRICE/CODE/CCY/TENOR,
    -- CPR/RATE/CODE/CCY/TENOR.
    if v_type = 'COMMODITY_FWD' and cardinality(v_parts) = 5 and v_metric = 'PRICE' then
        return 'oresmd://commodity/' || lower(v_parts[3])
            || '?ccy=' || lower(v_parts[4])
            || '&type=quote&quote=fwd&point=' || lower(v_parts[5]);
    end if;
    if v_type = 'CPR' and cardinality(v_parts) = 5 and v_metric = 'RATE' then
        return 'oresmd://commodity/' || lower(v_parts[3])
            || '?ccy=' || lower(v_parts[4])
            || '&type=quote&quote=cpr&point=' || lower(v_parts[5]);
    end if;

    -- Credit curves: CDS/CREDIT_SPREAD/ENTITY/SENIORITY/CCY/TENOR,
    -- HAZARD_RATE/RATE/ENTITY/SENIORITY/CCY/TENOR; point = seniority,tenor.
    if v_type = 'CDS' and cardinality(v_parts) = 6 and v_metric = 'CREDIT_SPREAD' then
        return 'oresmd://credit/' || lower(v_parts[3])
            || '?ccy=' || lower(v_parts[5])
            || '&type=quote&quote=cds&point=' || lower(v_parts[4]) || ',' || lower(v_parts[6]);
    end if;
    if v_type = 'HAZARD_RATE' and cardinality(v_parts) = 6 and v_metric = 'RATE' then
        return 'oresmd://credit/' || lower(v_parts[3])
            || '?ccy=' || lower(v_parts[5])
            || '&type=quote&quote=hazard_rate&point=' || lower(v_parts[4])
            || ',' || lower(v_parts[6]);
    end if;

    -- Credit recovery: RECOVERY_RATE/RATE/ENTITY/SENIORITY/CCY (scalar;
    -- point is just the seniority).
    if v_type = 'RECOVERY_RATE' and cardinality(v_parts) = 5 and v_metric = 'RATE' then
        return 'oresmd://credit/' || lower(v_parts[3])
            || '?ccy=' || lower(v_parts[5])
            || '&type=quote&quote=recovery_rate&point=' || lower(v_parts[4]);
    end if;

    -- Credit index: CDS_INDEX/BASE_CORRELATION/INDEX/TENOR/DETACHMENT,
    -- INDEX_CDS_TRANCHE/BASE_CORRELATION/INDEX/SERIES_TENOR/DETACHMENT
    -- (no ccy dimension; point = tenor,detachment).
    if v_type = 'CDS_INDEX' and cardinality(v_parts) = 5 and v_metric = 'BASE_CORRELATION' then
        return 'oresmd://credit/' || lower(v_parts[3])
            || '?type=quote&quote=cds_index&point=' || lower(v_parts[4])
            || ',' || lower(v_parts[5]);
    end if;
    if v_type = 'INDEX_CDS_TRANCHE' and cardinality(v_parts) = 5
        and v_metric = 'BASE_CORRELATION' then
        return 'oresmd://credit/' || lower(v_parts[3])
            || '?type=quote&quote=index_cds_tranche&point=' || lower(v_parts[4])
            || ',' || lower(v_parts[5]);
    end if;

    -- Inflation swaps: ZC_INFLATIONSWAP/RATE/INDEX/POINT,
    -- YY_INFLATIONSWAP/RATE/INDEX/POINT.
    if v_type = 'ZC_INFLATIONSWAP' and cardinality(v_parts) = 4 and v_metric = 'RATE' then
        return 'oresmd://inflation/' || lower(v_parts[3])
            || '?type=quote&quote=zc_swap&point=' || lower(v_parts[4]);
    end if;
    if v_type = 'YY_INFLATIONSWAP' and cardinality(v_parts) = 4 and v_metric = 'RATE' then
        return 'oresmd://inflation/' || lower(v_parts[3])
            || '?type=quote&quote=yy_swap&point=' || lower(v_parts[4]);
    end if;

    -- Inflation seasonality: SEASONALITY/RATE/MULT/INDEX/POINT (the MULT
    -- segment is accepted verbatim and dropped).
    if v_type = 'SEASONALITY' and cardinality(v_parts) = 5 and v_metric = 'RATE' then
        return 'oresmd://inflation/' || lower(v_parts[4])
            || '?type=quote&quote=seasonality&point=' || lower(v_parts[5]);
    end if;

    -- Correlation: CORRELATION/RATE/FACTOR_PAIR (scalar, no point).
    if v_type = 'CORRELATION' and cardinality(v_parts) = 3 and v_metric = 'RATE' then
        return 'oresmd://correlation/' || lower(v_parts[3])
            || '?type=quote&quote=pairwise';
    end if;

    -- No oresmd mapping: BOND, the option/capfloor families, and any
    -- malformed key.
    return null;
end;
$fn$;

-- The inverse of the index-name projection: CCY-FAMILY (overnight, no
-- tenor) or CCY-FAMILY-TENOR, serialised as the fixing URI. Mirrors
-- oresmd_projections::from_index_name; a term family (libor/euribor)
-- without a tenor could not have come from a forward projection and is
-- rejected, as is any name with other than 2-3 non-empty segments.
create or replace function ores_marketdata_oresmd_uri_from_index_name_fn(p_name text)
returns text
language plpgsql
immutable
as $fn$
declare
    v_parts text[];
    v_family text;
    v_uri text;
begin
    if p_name is null then
        return null;
    end if;

    v_parts := string_to_array(p_name, '-');
    if v_parts is null or cardinality(v_parts) < 2 or cardinality(v_parts) > 3
        or v_parts[1] = '' or v_parts[2] = ''
        or (cardinality(v_parts) = 3 and v_parts[3] = '') then
        return null;
    end if;

    v_family := lower(v_parts[2]);
    if v_family not in ('libor', 'euribor', 'sofr', 'estr', 'sonia', 'tona', 'saron',
        'aonia', 'corra', 'honia', 'sora', 'swestr', 'nowa', 'kofr', 'mibor',
        'zaronia', 'destr', 'polonia', 'nzonia', 'shibor', 'tiie', 'taibor') then
        return null;
    end if;

    v_uri := 'oresmd://ir/' || lower(v_parts[1]) || '?index=' || v_family;
    if cardinality(v_parts) = 3 then
        v_uri := v_uri || '&tenor=' || lower(v_parts[3]);
    elsif v_family in ('libor', 'euribor') then
        return null;
    end if;
    return v_uri || '&type=fixing';
end;
$fn$;

-- The RATES/YIELD feed identity: qualifier "CCY/FAMILY[-TENOR]" (e.g.
-- 'USD/SOFR-FOMC'), the shape the ir_curve_feed auto-created series and
-- bindings used. Rebuilds the index name and delegates to the index-name
-- mapping, so the two boundaries always agree.
create or replace function ores_marketdata_oresmd_uri_from_feed_qualifier_fn(p_qualifier text)
returns text
language plpgsql
immutable
as $fn$
declare
    v_parts text[];
begin
    if p_qualifier is null then
        return null;
    end if;

    v_parts := string_to_array(p_qualifier, '/');
    if v_parts is null or cardinality(v_parts) <> 2
        or v_parts[1] = '' or v_parts[2] = '' then
        return null;
    end if;
    return ores_marketdata_oresmd_uri_from_index_name_fn(v_parts[1] || '-' || v_parts[2]);
end;
$fn$;

begin;

do $migration$
declare
    v_has_old_shape boolean;
    v_constraint_name text;
    v_series_dropped integer;
    v_bindings_dropped integer;
begin
    select exists (
        select 1 from information_schema.columns
        where table_name = 'ores_marketdata_market_series_tbl'
          and column_name = 'series_type'
    ) into v_has_old_shape;

    if not v_has_old_shape then
        raise notice 'market_series has no series_type column; migration not needed.';
        return;
    end if;

    -- 1. Add the oresmd_uri column to both tables. The guard above proves
    --    the old schema, so a plain add is safe; a rerun after success
    --    exits at the guard.
    alter table ores_marketdata_market_series_tbl add column oresmd_uri text;
    alter table ores_marketdata_feed_bindings_tbl add column oresmd_uri text;

    -- 2. Backfill market_series from the inverse projection of its ORE
    --    columns: the RATES/YIELD feed identity through the feed-qualifier
    --    mapping, the seeded discount curve row to its explicit curve URI,
    --    FIXING rows through the index-name mapping, everything else
    --    through the general inverse projection of the reconstructed key.
    update ores_marketdata_market_series_tbl
    set oresmd_uri = case
        when series_type = 'RATES' and metric = 'YIELD'
            then ores_marketdata_oresmd_uri_from_feed_qualifier_fn(qualifier)
        when series_type = 'YieldCurve' and metric = 'DISCOUNT'
            and qualifier = 'USD/SOFR-FOMC'
            then 'oresmd://ir/usd?index=sofr&tenor=fomc&role=discount&type=curve'
        when series_type = 'FIXING' and metric = 'RATE'
            then ores_marketdata_oresmd_uri_from_index_name_fn(qualifier)
        else ores_marketdata_oresmd_uri_from_ore_key_fn(
            series_type || '/' || metric || '/' || qualifier)
    end;

    -- 3. Backfill feed_bindings: RATES/YIELD/<qualifier> ore_keys through
    --    the feed-qualifier mapping (the prefix is 12 characters), every
    --    other ore_key through the general inverse projection.
    update ores_marketdata_feed_bindings_tbl
    set oresmd_uri = case
        when ore_key like 'RATES/YIELD/%'
            then ores_marketdata_oresmd_uri_from_feed_qualifier_fn(substring(ore_key from 13))
        else ores_marketdata_oresmd_uri_from_ore_key_fn(ore_key)
    end;

    -- 4. Hard-delete the unmappable rows. The delete rules below soft-
    --    delete (close the validity window), which would leave the rows
    --    present with a NULL URI; drop the rules first, delete, recreate.
    drop rule if exists ores_marketdata_market_series_delete_rule
        on "ores_marketdata_market_series_tbl";
    drop rule if exists ores_marketdata_feed_bindings_delete_rule
        on "ores_marketdata_feed_bindings_tbl";

    delete from ores_marketdata_market_series_tbl where oresmd_uri is null;
    get diagnostics v_series_dropped = row_count;
    raise notice 'Dropped % market_series row(s) with no oresmd URI mapping.', v_series_dropped;

    delete from ores_marketdata_feed_bindings_tbl where oresmd_uri is null;
    get diagnostics v_bindings_dropped = row_count;
    raise notice 'Dropped % feed_binding(s) with no oresmd URI mapping.', v_bindings_dropped;

    create or replace rule ores_marketdata_market_series_delete_rule as
    on delete to "ores_marketdata_market_series_tbl" do instead (
        update "ores_marketdata_market_series_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = OLD.tenant_id
          and id = OLD.id
          and valid_to = ores_utility_infinity_timestamp_fn();
    );

    create or replace rule ores_marketdata_feed_bindings_delete_rule as
    on delete to "ores_marketdata_feed_bindings_tbl" do instead (
        update "ores_marketdata_feed_bindings_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = OLD.tenant_id
          and id = OLD.id
          and valid_to = ores_utility_infinity_timestamp_fn();
    );

    -- 5. Drop the superseded identity checks. They were declared inline
    --    and therefore auto-named, so match by definition text rather
    --    than by guessing the generated name. Validation now lives in the
    --    oresmd layer, not in SQL checks.
    for v_constraint_name in
        select conname
        from pg_constraint
        where conrelid = 'ores_marketdata_market_series_tbl'::regclass
          and contype = 'c'
          and (
              pg_get_constraintdef(oid) like '%"series_type" <> ''''%'
              or pg_get_constraintdef(oid) like '%"metric" <> ''''%'
              or pg_get_constraintdef(oid) like '%"qualifier" <> ''''%'
              or pg_get_constraintdef(oid) like '%"asset_class" <> ''''%'
              or pg_get_constraintdef(oid) like '%"series_subclass" <> ''''%'
          )
    loop
        execute format(
            'alter table ores_marketdata_market_series_tbl drop constraint %I',
            v_constraint_name);
    end loop;

    for v_constraint_name in
        select conname
        from pg_constraint
        where conrelid = 'ores_marketdata_feed_bindings_tbl'::regclass
          and contype = 'c'
          and pg_get_constraintdef(oid) like '%"ore_key" <> ''''%'
    loop
        execute format(
            'alter table ores_marketdata_feed_bindings_tbl drop constraint %I',
            v_constraint_name);
    end loop;

    -- 6. Drop the natural-key unique indexes on the ORE columns and the
    --    ORE columns themselves. (The checks above would have been
    --    auto-dropped with their columns; the unique indexes reference
    --    tenant_id/party_id too, so they must go explicitly.)
    drop index if exists market_series_party_id_series_type_metric_qualifier_uniq_idx;
    drop index if exists feed_bindings_party_id_ore_key_source_name_uniq_idx;

    alter table ores_marketdata_market_series_tbl
        drop column if exists series_type,
        drop column if exists metric,
        drop column if exists qualifier,
        drop column if exists asset_class,
        drop column if exists series_subclass,
        drop column if exists is_scalar;
    alter table ores_marketdata_feed_bindings_tbl
        drop column if exists ore_key;

    -- 7. Re-impose the new column shape, declared exactly as the new
    --    create scripts declare it (inline check, auto-named, so a fresh
    --    recreate and a migration produce identical catalogues).
    alter table ores_marketdata_market_series_tbl
        alter column oresmd_uri set not null,
        add check ("oresmd_uri" <> '');
    alter table ores_marketdata_feed_bindings_tbl
        alter column oresmd_uri set not null,
        add check ("oresmd_uri" <> '');

    -- 8. Move the natural-key unique indexes to the URI. A collision here
    --    (two old identities projecting to one URI) fails loudly and
    --    rolls back the whole migration.
    create unique index if not exists market_series_party_id_oresmd_uri_uniq_idx
    on "ores_marketdata_market_series_tbl" (tenant_id, party_id, oresmd_uri)
    where valid_to = ores_utility_infinity_timestamp_fn();

    create unique index if not exists feed_bindings_party_id_oresmd_uri_source_name_uniq_idx
    on "ores_marketdata_feed_bindings_tbl" (tenant_id, party_id, oresmd_uri, source_name)
    where valid_to = ores_utility_infinity_timestamp_fn();
end $migration$;

-- Refresh the market_series insert trigger: the old body validates
-- NEW.asset_class, a column this migration drops, so any insert after the
-- migration would fail at runtime. The new body (no asset_class
-- validation, byte-identical to the new create script) is applied
-- unconditionally; on a fresh database it is a no-op.
create or replace function ores_marketdata_market_series_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate derivation_kind
    NEW.derivation_kind := ores_refdata_validate_derivation_kind_fn(NEW.tenant_id, NEW.derivation_kind);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_marketdata_market_series_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_marketdata_market_series_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < clock_timestamp();
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- Summary
select 'ores_marketdata_market_series_tbl' as entity,
       count(*) as active_series
from ores_marketdata_market_series_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

select 'ores_marketdata_feed_bindings_tbl' as entity,
       count(*) as active_bindings
from ores_marketdata_feed_bindings_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

-- Migration-only helpers: drop so the migrated schema matches a fresh
-- recreate exactly.
drop function ores_marketdata_oresmd_uri_from_ore_key_fn(text);
drop function ores_marketdata_oresmd_uri_from_index_name_fn(text);
drop function ores_marketdata_oresmd_uri_from_feed_qualifier_fn(text);

commit;
