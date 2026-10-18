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
 * Credit Limit Table
 *
 * A credit limit is the amount of credit the firm allows one counterparty,
 * set by one legal entity of the firm. It is the dimension that splits a
 * [[id:6AADB1A7-1229-4D10-8951-58446788957F][netting agreement]]'s trades into separate
 * [[id:83C9697A-0D6D-42A2-9917-6C68268B5404][netting sets]]: where one agreement covers trades under several limits,
 * netting is restricted to the trades within one limit (see
 * [[id:5F12A37F-9F26-4B49-BAAF-1BAA7B2BB94F][Netting sets]]).
 *
 * The limit is a credit fact, not a legal one, so it stands apart from the
 * agreement: one agreement may cover several limits, and one limit may
 * cover trades under several agreements. A netting set names both, and a
 * pinned key holds the set's counterparty and party to the limit's, as it
 * does for the agreement.
 */

create table if not exists "ores_refdata_credit_limits_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "code" text not null,
    "party_id" uuid not null,
    "counterparty_id" uuid not null,
    "limit_amount" numeric(38, 12) null,
    "description" text null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("limit_amount" is null or "limit_amount" >= 0)
);

-- Composite natural key: unique combination for active records
create unique index if not exists credit_limits_code_party_id_uniq_idx
on "ores_refdata_credit_limits_tbl" (tenant_id, code, party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists credit_limits_version_uniq_idx
on "ores_refdata_credit_limits_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists credit_limits_id_uniq_idx
on "ores_refdata_credit_limits_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists credit_limits_tenant_idx
on "ores_refdata_credit_limits_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_credit_limits_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate party_id (soft FK to ores_refdata_parties_tbl)
    if not exists (
        select 1 from ores_refdata_parties_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid party_id: %. No active party found with this id.', NEW.party_id
            using errcode = '23503';
    end if;

    -- Validate counterparty_id (soft FK to ores_refdata_counterparties_tbl)
    if not exists (
        select 1 from ores_refdata_counterparties_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.counterparty_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid counterparty_id: %. No active counterparty found with this id.', NEW.counterparty_id
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
    from "ores_refdata_credit_limits_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
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
                    'credit_limit',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'credit_limit',
                'id',
                NEW.version::text,
                current_version::text);
        end if;
        if exists (
            select 1 from "ores_refdata_credit_limits_tbl"
            where tenant_id = NEW.tenant_id
              and id = NEW.id
              and valid_to = ores_utility_infinity_timestamp_fn()
              and "counterparty_id" is distinct from NEW."counterparty_id"
        ) then
            raise exception 'counterparty_id cannot change: it is fixed for the life of the credit_limit.'
                using errcode = '23514';
        end if;
        if exists (
            select 1 from "ores_refdata_credit_limits_tbl"
            where tenant_id = NEW.tenant_id
              and id = NEW.id
              and valid_to = ores_utility_infinity_timestamp_fn()
              and "party_id" is distinct from NEW."party_id"
        ) then
            raise exception 'party_id cannot change: it is fixed for the life of the credit_limit.'
                using errcode = '23514';
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_refdata_credit_limits_tbl"
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
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_refdata_credit_limits_insert_trg
before insert on "ores_refdata_credit_limits_tbl"
for each row execute function ores_refdata_credit_limits_insert_fn();

create or replace rule ores_refdata_credit_limits_delete_rule as
on delete to "ores_refdata_credit_limits_tbl" do instead (
    update "ores_refdata_credit_limits_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
