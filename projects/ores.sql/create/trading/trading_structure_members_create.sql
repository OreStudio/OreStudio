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
 * Structure Member Table
 *
 * The link between a structure and one of its legs. The trade is not owned by
 * the deal: it points at it, so the same trade can be unlinked from one
 * structure and linked to another without being rewritten.
 *
 * The row is keyed by the trade and is temporal, which says the rule directly: a
 * trade sits in at most one structure at a time, and unlinking closes the row
 * rather than deleting it. The history of what a trade belonged to survives.
 *
 * The member copies the party and the counterparty from its structure, and the
 * pins hold the copies to their source, so a leg cannot disagree with the deal
 * it belongs to. The insert trigger holds a leg to the roles its template
 * allows, and to the number of legs the template states for that role.
 */

create table if not exists "ores_trading_structure_members_tbl" (
    "trade_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "structure_id" uuid not null,
    "role" text not null,
    "sequence_number" integer not null,
    "party_id" uuid not null,
    "counterparty_id" uuid not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, trade_id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        trade_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("trade_id" <> ores_utility_nil_uuid_fn()),
    check ("sequence_number" >= 1)
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists structure_members_version_uniq_idx
on "ores_trading_structure_members_tbl" (tenant_id, trade_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists structure_members_id_uniq_idx
on "ores_trading_structure_members_tbl" (tenant_id, trade_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists structure_members_tenant_idx
on "ores_trading_structure_members_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_structure_members_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate trade_id (soft FK to ores_trading_trades_tbl)
    if not exists (
        select 1 from ores_trading_trades_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.trade_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid trade_id: %. No active trade found with this id.', NEW.trade_id
            using errcode = '23503';
    end if;

    -- Validate structure_id (soft FK to ores_trading_structures_tbl)
    if not exists (
        select 1 from ores_trading_structures_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.structure_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid structure_id: %. No active structure found with this id.', NEW.structure_id
            using errcode = '23503';
    end if;

    -- Validate the structure_party pin to ores_trading_structures_tbl
    if NEW.structure_id is not null and NEW.party_id is not null and not exists (
        select 1 from ores_trading_structures_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.structure_id
          and party_id = NEW.party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid structure_id: %. The member''s party must be the structure''s.', NEW.structure_id
            using errcode = '23503';
    end if;

    -- Validate the structure_counterparty pin to ores_trading_structures_tbl
    if NEW.structure_id is not null and NEW.counterparty_id is not null and not exists (
        select 1 from ores_trading_structures_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.structure_id
          and counterparty_id = NEW.counterparty_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid structure_id: %. The member''s counterparty must be the structure''s.', NEW.structure_id
            using errcode = '23503';
    end if;

    -- A leg fills a role its template allows, and no more legs than the
    -- template states for that role. A package rests on no template, so it
    -- constrains nothing and any role is allowed.
    declare
        v_template text;
        v_max      integer;
        v_used     integer;
    begin
        select template_code into v_template
        from ores_trading_structures_tbl
        where tenant_id = NEW.tenant_id and id = NEW.structure_id;

        if v_template is not null then
            select max_legs into v_max
            from ores_trading_structure_template_roles_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and template_code = v_template
              and role = NEW.role
              and valid_to = ores_utility_infinity_timestamp_fn();

            if not found then
                raise exception 'Invalid role: %. The template % does not allow this role.',
                    NEW.role, v_template
                    using errcode = '23514';
            end if;

            -- A maximum of zero means the role states no upper bound.
            if v_max > 0 then
                select count(*) into v_used
                from ores_trading_structure_members_tbl
                where tenant_id = NEW.tenant_id
                  and structure_id = NEW.structure_id
                  and role = NEW.role
                  and valid_to = ores_utility_infinity_timestamp_fn();

                if v_used >= v_max then
                    raise exception 'Invalid role: %. The template % allows % of that role and the structure holds %.',
                        NEW.role, v_template, v_max, v_used
                        using errcode = '23514';
                end if;
            end if;
        end if;
    end;
    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_trading_structure_members_tbl"
    where tenant_id = NEW.tenant_id
      and trade_id = NEW.trade_id
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
                    'structure_member',
                    'trade_id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'structure_member',
                'trade_id',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_trading_structure_members_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and trade_id = NEW.trade_id
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

create or replace trigger ores_trading_structure_members_insert_trg
before insert on "ores_trading_structure_members_tbl"
for each row execute function ores_trading_structure_members_insert_fn();

create or replace rule ores_trading_structure_members_delete_rule as
on delete to "ores_trading_structure_members_tbl" do instead (
    update "ores_trading_structure_members_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and trade_id = OLD.trade_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Structure Member
-- =============================================================================
alter table ores_trading_structure_members_tbl enable row level security;

drop policy if exists structure_members_tbl_tenant_isolation_policy
    on ores_trading_structure_members_tbl;

create policy structure_members_tbl_tenant_isolation_policy
on ores_trading_structure_members_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
