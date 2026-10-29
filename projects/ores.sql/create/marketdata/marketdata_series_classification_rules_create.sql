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
 * Series Classification Rule Table
 *
 * The taxonomy rule for a market series, one row per identity the runtime
 * classifies. The runtime reads the whole table once, builds a classifier from it,
 * and asks that classifier for the asset classes and the series subclass of every
 * series it creates.
 *
 * The table replaces a compiled C++ map, so the taxonomy is managed like every
 * other catalogue: an instrument a corpus adds later, or one a user brings, is an
 * inserted row rather than a rebuild. This matters because an ingest that cannot
 * classify a series it must create aborts on it, so a compiled table makes every
 * new instrument a code change.
 *
 * The key is the oresmd identity and not an ORE key. Two kinds of identity reach
 * this table, and key_kind says which a row is keyed by:
 *
 * - datum, keyed by instrument -- the text the URI writes under
 *   instrument, such as ir_swap or discount -- and by quote, the text it
 *   writes under quote, or the empty string for a rule that applies to every
 *   quote of its instrument. Most instruments classify the same way whatever their
 *   quote carries, so their row holds the empty string; a lookup tries the exact
 *   quote first and falls back to the empty one.
 * - index, keyed by instrument too, which for this kind holds the index
 *   family's oresmd name or the empty string for every family. A fixing series is
 *   an index and not a datum, so it is keyed by the index's own vocabulary.
 *
 * asset_class_source says where a row's classes come from:
 *
 * - literal names its class in asset_class_code.
 * - correlation_operands reads the classes from the two operand fields of a
 *   correlation datum, because a pairwise correlation belongs to the two classes
 *   it relates and to no class of its own.
 * - index_authority reads the class from the index family's schema row, whose
 *   asset authority names the market-data namespace the index is written in, and
 *   ores.refdata.asset_class_catalogue maps that authority onto a refdata class.
 *   One row serves every index family, and a family the catalogue maps to no class
 *   leaves the class list empty rather than mis-classing the series.
 *
 * The class codes name rows in refdata.asset_class_code and the subclass codes
 * name rows in refdata.series_subclass_code, and the insert trigger checks both
 * against those catalogues.
 *
 * The reader rejects an empty table, because an empty table aborts the first
 * ingest that reads it and the message should name the table rather than the first
 * series it met.
 */

create table if not exists "ores_marketdata_series_classification_rules_tbl" (
    "instrument" text not null,
    "quote" text not null default '',
    "key_kind" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "asset_class_source" text not null default 'literal',
    "asset_class_code" text null,
    "series_subclass_code" text not null,
    "description" text not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, instrument, quote, key_kind, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        instrument WITH =,
        quote WITH =,
        key_kind WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("key_kind" <> ''),
    check ("key_kind" in ('datum', 'index')),
    check ("asset_class_source" in ('literal', 'correlation_operands', 'index_authority')),
    check (("asset_class_source" <> 'literal') or ("asset_class_code" is not null)),
    check (("asset_class_source" = 'literal') or ("asset_class_code" is null))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists series_classification_rules_version_uniq_idx
on "ores_marketdata_series_classification_rules_tbl" (tenant_id, instrument, quote, key_kind, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists series_classification_rules_instrument_quote_key_kind_uniq_idx
on "ores_marketdata_series_classification_rules_tbl" (tenant_id, instrument, quote, key_kind)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists series_classification_rules_tenant_idx
on "ores_marketdata_series_classification_rules_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_marketdata_series_classification_rules_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate asset_class_code (optional field -- skip validation when null)
    if NEW.asset_class_code is not null then
        NEW.asset_class_code := ores_refdata_validate_asset_class_code_fn(NEW.tenant_id, NEW.asset_class_code);
    end if;

    -- Validate series_subclass_code
    NEW.series_subclass_code := ores_refdata_validate_series_subclass_code_fn(NEW.tenant_id, NEW.series_subclass_code);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_marketdata_series_classification_rules_tbl"
    where tenant_id = NEW.tenant_id
      and instrument = NEW.instrument and quote = NEW.quote and key_kind = NEW.key_kind
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        -- The write states what it believes about the row, and the store is
        -- what decides. Version zero means one thing: no current row exists.
        -- So a create that collides with a live row is refused here, for every
        -- client, rather than by a check each client has to remember.
        if NEW.version = 0 then
            if not ores_utility_version_replace_allowed_fn() then
                perform ores_outcome_raise_fn(
                    'already_exists',
                    'series_classification_rule',
                    'instrument');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'series_classification_rule',
                'instrument',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_marketdata_series_classification_rules_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and instrument = NEW.instrument and quote = NEW.quote and key_kind = NEW.key_kind
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < clock_timestamp();
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_series_classification_rules_insert_trg
before insert on "ores_marketdata_series_classification_rules_tbl"
for each row execute function ores_marketdata_series_classification_rules_insert_fn();

create or replace rule ores_marketdata_series_classification_rules_delete_rule as
on delete to "ores_marketdata_series_classification_rules_tbl" do instead (
    update "ores_marketdata_series_classification_rules_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and instrument = OLD.instrument and quote = OLD.quote and key_kind = OLD.key_kind
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Series Classification Rule
-- =============================================================================
alter table ores_marketdata_series_classification_rules_tbl enable row level security;

drop policy if exists series_classification_rules_tbl_tenant_isolation_policy
    on ores_marketdata_series_classification_rules_tbl;

create policy series_classification_rules_tbl_tenant_isolation_policy
on ores_marketdata_series_classification_rules_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
