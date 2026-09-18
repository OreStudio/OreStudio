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
 * The taxonomy rule for an ORE market data series, one row per series
 * type and metric. An import reads the whole table once, builds a
 * classifier from it, and asks that classifier for the asset classes and
 * the series subclass of every key it meets.
 *
 * The table replaces a compiled C++ map, so the taxonomy is now managed
 * like every other catalogue: a type ORE adds later, or one a user
 * brings, is an inserted row rather than a rebuild. This matters because
 * an import aborts on a type it cannot classify, so a compiled table
 * makes every new ORE type a code change.
 *
 * The class codes name rows in refdata.asset_class_code and the
 * subclass codes name rows in refdata.series_subclass_code, and the
 * insert trigger checks both against those catalogues.
 *
 * Two things a single class column could not hold, and how this table
 * holds them:
 *
 * - A pairwise correlation relates two classes and belongs to both, so
 *   neither is a property of the type. Its row names no class at all and
 *   its classes are read from the two operands of the key, which is what
 *   asset_class_source records.
 * - GENERIC-MD is a wrapper whose metric slot names the instrument
 *   inside it, so its rule is keyed by the metric rather than by the type
 *   alone.
 *
 * The reader rejects an empty table, because an empty table aborts the
 * first import that reads it and the message should name the table rather
 * than the first series it met.
 */

create table if not exists "ores_marketdata_series_classification_rules_tbl" (
    "series_type" text not null,
    "metric" text not null,
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
    primary key (tenant_id, series_type, metric, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        series_type WITH =,
        metric WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("series_type" <> ''),
    check ("asset_class_source" in ('literal', 'correlation_operands')),
    check (("asset_class_source" <> 'literal') or ("asset_class_code" is not null)),
    check (("asset_class_source" = 'literal') or ("asset_class_code" is null))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists series_classification_rules_version_uniq_idx
on "ores_marketdata_series_classification_rules_tbl" (tenant_id, series_type, metric, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists series_classification_rules_series_type_metric_uniq_idx
on "ores_marketdata_series_classification_rules_tbl" (tenant_id, series_type, metric)
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

    -- Version management
    select version into current_version
    from "ores_marketdata_series_classification_rules_tbl"
    where tenant_id = NEW.tenant_id
      and series_type = NEW.series_type and metric = NEW.metric
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
        update "ores_marketdata_series_classification_rules_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and series_type = NEW.series_type and metric = NEW.metric
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

create or replace trigger ores_marketdata_series_classification_rules_insert_trg
before insert on "ores_marketdata_series_classification_rules_tbl"
for each row execute function ores_marketdata_series_classification_rules_insert_fn();

create or replace rule ores_marketdata_series_classification_rules_delete_rule as
on delete to "ores_marketdata_series_classification_rules_tbl" do instead (
    update "ores_marketdata_series_classification_rules_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and series_type = OLD.series_type and metric = OLD.metric
      and valid_to = ores_utility_infinity_timestamp_fn();
);
