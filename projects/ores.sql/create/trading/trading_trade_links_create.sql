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
 * Trade Link Table
 *
 * One relation between two trades, with a direction, a type and nothing
 * else. A close-out answers a live trade, a roll replaces one, an exercise
 * produces one. The link holds no cash flow, no amount and no state: the
 * [[id:20D446E8-EA13-47AA-BE0C-FDDD7CF428F3][trade id type]] pattern of a
 * typed reference applies here as well, with the type carrying the role of
 * each end.
 *
 * The row is keyed by its two ends and its type, and the type is a
 * [[id:8C8D3355-1797-4D8E-A47D-07D4402B2794][trade link type]] code. It
 * records the trade activity that made it, because a link is created by an
 * amendment like anything else, and it copies the party of the from end so
 * row level security can see it on every row.
 *
 * A process that created a link records the run on the trade group, not
 * here: a link holds nothing and stands alone.
 */

create table if not exists "ores_trading_trade_links_tbl" (
    "from_trade_id" uuid not null,
    "to_trade_id" uuid not null,
    "link_type" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "trade_activity_id" uuid not null,
    "party_id" uuid not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, from_trade_id, to_trade_id, link_type, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        from_trade_id WITH =,
        to_trade_id WITH =,
        link_type WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("from_trade_id" <> ores_utility_nil_uuid_fn()),
    check ("to_trade_id" <> ores_utility_nil_uuid_fn()),
    check ("link_type" <> ''),
    check ("from_trade_id" <> "to_trade_id"),
    constraint ores_trading_trade_links_from_trade_id_fk foreign key ("tenant_id", "from_trade_id") references "ores_trading_trades_tbl" ("tenant_id", "id"),
    constraint ores_trading_trade_links_to_trade_id_fk foreign key ("tenant_id", "to_trade_id") references "ores_trading_trades_tbl" ("tenant_id", "id"),
    constraint ores_trading_trade_links_trade_activity_id_fk foreign key ("tenant_id", "trade_activity_id") references "ores_trading_trade_activities_tbl" ("tenant_id", "id"),
    constraint ores_trading_trade_links_anchor_party_pin foreign key ("tenant_id", "from_trade_id", "party_id") references "ores_trading_trades_tbl" ("tenant_id", "id", "party_id")
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists trade_links_version_uniq_idx
on "ores_trading_trade_links_tbl" (tenant_id, from_trade_id, to_trade_id, link_type, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists trade_links_id_uniq_idx
on "ores_trading_trade_links_tbl" (tenant_id, from_trade_id, to_trade_id, link_type)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists trade_links_tenant_idx
on "ores_trading_trade_links_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_trade_links_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate link_type (soft FK to ores_trading_trade_link_types_tbl)
    if not exists (
        select 1 from ores_trading_trade_link_types_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.link_type
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid link_type: %. No active trade link type found with this code.', NEW.link_type
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_trading_trade_links_tbl"
    where tenant_id = NEW.tenant_id
      and from_trade_id = NEW.from_trade_id and to_trade_id = NEW.to_trade_id and link_type = NEW.link_type
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
                    'trade_link',
                    'from_trade_id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'trade_link',
                'from_trade_id',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_trading_trade_links_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and from_trade_id = NEW.from_trade_id and to_trade_id = NEW.to_trade_id and link_type = NEW.link_type
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

create or replace trigger ores_trading_trade_links_insert_trg
before insert on "ores_trading_trade_links_tbl"
for each row execute function ores_trading_trade_links_insert_fn();

create or replace rule ores_trading_trade_links_delete_rule as
on delete to "ores_trading_trade_links_tbl" do instead (
    update "ores_trading_trade_links_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and from_trade_id = OLD.from_trade_id and to_trade_id = OLD.to_trade_id and link_type = OLD.link_type
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Trade Link
-- =============================================================================
alter table ores_trading_trade_links_tbl enable row level security;

drop policy if exists trade_links_tbl_tenant_isolation_policy
    on ores_trading_trade_links_tbl;

create policy trade_links_tbl_tenant_isolation_policy
on ores_trading_trade_links_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
