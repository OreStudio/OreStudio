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
 * Equity Underlying Table
 *
 * The equity underlyings a population may draw from. An equity instrument
 * states underlying_name, documented as the ORE equity Name identifier, and
 * nothing held the set a tenant may use, so a generator that needed one had to
 * invent it.
 *
 * The code is the name the engine resolves, not a display name. ORE's own
 * example trades price underlyings as bare index names such as .SPX and
 * .STOXX50E, as exchange-suffixed shares such as MSFT.O and CR.N, and as
 * ISINs such as GB0005405286. The catalogue therefore stores the spelling a
 * document states, because that is the string the engine resolves.
 *
 * The same instrument appears under more than one spelling: .SPX and
 * RIC:.SPX are one index, and the catalogue holds both because a document may
 * state either. Resolving them to one instrument is what resolves_to is for: a
 * row that is the instrument leaves it empty, and a row that is an alias names
 * the instrument it resolves to. A population that read the two as two
 * instruments would price one index twice.
 *
 * The kind travels with the underlying because it decides what the trade is.
 * An option on an index and an option on a share are the same instrument type in
 * the store, but a book that claimed to trade single names while holding only
 * indices would misreport itself.
 *
 * Only underlyings a document states as Equity belong here. ORE states each
 * underlying's kind beside its name, and one corpus trade proves the kind is not
 * implied by the trade type: an EquityWorstOfBasketSwap in the examples carries
 * two underlyings whose kind is FX, total return indices on currency pairs. A
 * catalogue that took every name an equity typed trade mentions would file a
 * currency pair as an equity.
 *
 * This model holds the vocabulary only. The prices and the volatility surfaces an
 * equity prices on are market data, and they are not here.
 */

create table if not exists "ores_refdata_equity_underlyings_tbl" (
    "code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "description" text not null,
    "instrument_type" text not null,
    "resolves_to" text null,
    "currency_code" text null,
    "display_order" integer not null default 0,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, code, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("code" <> '')
);

-- Unique name for active records
create unique index if not exists equity_underlyings_name_uniq_idx
on "ores_refdata_equity_underlyings_tbl" (tenant_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists equity_underlyings_version_uniq_idx
on "ores_refdata_equity_underlyings_tbl" (tenant_id, code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists equity_underlyings_code_uniq_idx
on "ores_refdata_equity_underlyings_tbl" (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists equity_underlyings_tenant_idx
on "ores_refdata_equity_underlyings_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_equity_underlyings_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_refdata_equity_underlyings_tbl"
    where tenant_id = NEW.tenant_id
      and code = NEW.code
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
                    'equity_underlying',
                    'code');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'equity_underlying',
                'code',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_refdata_equity_underlyings_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and code = NEW.code
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

create or replace trigger ores_refdata_equity_underlyings_insert_trg
before insert on "ores_refdata_equity_underlyings_tbl"
for each row execute function ores_refdata_equity_underlyings_insert_fn();

create or replace rule ores_refdata_equity_underlyings_delete_rule as
on delete to "ores_refdata_equity_underlyings_tbl" do instead (
    update "ores_refdata_equity_underlyings_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and code = OLD.code
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Equity Underlying
-- =============================================================================
alter table ores_refdata_equity_underlyings_tbl enable row level security;

drop policy if exists equity_underlyings_tbl_tenant_isolation_policy
    on ores_refdata_equity_underlyings_tbl;

create policy equity_underlyings_tbl_tenant_isolation_policy
on ores_refdata_equity_underlyings_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
