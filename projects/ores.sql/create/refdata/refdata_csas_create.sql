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
 * CSA Table
 *
 * A Credit Support Annex holds the collateral terms of a [[id:83C9697A-0D6D-42A2-9917-6C68268B5404][netting set]]:
 * thresholds, minimum transfer amounts, margining frequencies, the margin
 * period of risk and the eligible collateral. Netting reduces exposure, and
 * collateral mitigates what remains (see [[id:5F12A37F-9F26-4B49-BAAF-1BAA7B2BB94F][Netting sets]]).
 *
 * A CSA has a lifecycle of its own: it is added, changed or switched off
 * without touching the trades in its set. A set has at most one active CSA;
 * an inactive one keeps its terms, as ORE keeps the details of a set whose
 * CSA flag is off. The columns follow ORE's CSADetails one for one, and
 * the eligible collateral currencies are [[id:25090994-3093-470E-BB3F-7832EDCA26A4][rows of their own]].
 */

create table if not exists "ores_refdata_csas_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "netting_set_id" uuid not null,
    "party_id" uuid not null,
    "is_active" boolean not null,
    "bilateral" text null,
    "csa_currency" text null,
    "index_name" text null,
    "threshold_pay" double precision null,
    "threshold_receive" double precision null,
    "minimum_transfer_amount_pay" double precision null,
    "minimum_transfer_amount_receive" double precision null,
    "independent_amount_held" double precision null,
    "independent_amount_type" text null,
    "call_frequency" text null,
    "post_frequency" text null,
    "margin_period_of_risk" text null,
    "collateral_compounding_spread_receive" double precision null,
    "collateral_compounding_spread_pay" double precision null,
    "apply_initial_margin" boolean null,
    "initial_margin_type" text null,
    "calculate_im_amount" boolean null,
    "calculate_vm_amount" boolean null,
    "non_exempt_im_regulations" text null,
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
    check ("bilateral" is null or "bilateral" in ('Bilateral', 'CallOnly', 'PostOnly')),
    check ("initial_margin_type" is null or "initial_margin_type" in ('Bilateral', 'CallOnly', 'PostOnly')),
    check ("independent_amount_type" is null or "independent_amount_type" = 'FIXED'),
    check ("threshold_pay" is null or "threshold_pay" >= 0),
    check ("threshold_receive" is null or "threshold_receive" >= 0),
    check ("minimum_transfer_amount_pay" is null or "minimum_transfer_amount_pay" >= 0),
    check ("minimum_transfer_amount_receive" is null or "minimum_transfer_amount_receive" >= 0)
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists csas_version_uniq_idx
on "ores_refdata_csas_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists csas_id_uniq_idx
on "ores_refdata_csas_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists csas_tenant_idx
on "ores_refdata_csas_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists csas_active_set_idx
on "ores_refdata_csas_tbl" (netting_set_id)
where valid_to = ores_utility_infinity_timestamp_fn()
  and is_active;

create or replace function ores_refdata_csas_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate netting_set_id (soft FK to ores_refdata_netting_sets_tbl)
    if not exists (
        select 1 from ores_refdata_netting_sets_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.netting_set_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid netting_set_id: %. No active netting set found with this id.', NEW.netting_set_id
            using errcode = '23503';
    end if;

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

    -- Validate the netting_set pin to ores_refdata_netting_sets_tbl
    if NEW.netting_set_id is not null and NEW.party_id is not null and not exists (
        select 1 from ores_refdata_netting_sets_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.netting_set_id
          and party_id = NEW.party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid netting_set_id: %. The CSA''s party must be the netting set''s.', NEW.netting_set_id
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
    from "ores_refdata_csas_tbl"
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
                    'csa',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'csa',
                'id',
                NEW.version::text,
                current_version::text);
        end if;
        if exists (
            select 1 from "ores_refdata_csas_tbl"
            where tenant_id = NEW.tenant_id
              and id = NEW.id
              and valid_to = ores_utility_infinity_timestamp_fn()
              and "party_id" is distinct from NEW."party_id"
        ) then
            raise exception 'party_id cannot change: it is fixed for the life of the csa.'
                using errcode = '23514';
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_refdata_csas_tbl"
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

create or replace trigger ores_refdata_csas_insert_trg
before insert on "ores_refdata_csas_tbl"
for each row execute function ores_refdata_csas_insert_fn();

create or replace rule ores_refdata_csas_delete_rule as
on delete to "ores_refdata_csas_tbl" do instead (
    update "ores_refdata_csas_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for CSA
-- =============================================================================
alter table ores_refdata_csas_tbl enable row level security;

drop policy if exists csas_tbl_tenant_isolation_policy
    on ores_refdata_csas_tbl;

create policy csas_tbl_tenant_isolation_policy
on ores_refdata_csas_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation (RESTRICTIVE): ANDed with the permissive tenant
-- policy above, a session sees only rows whose party_id its visible
-- party set admits. The visible_party_ids-is-null passthrough applies
-- for sessions with no party restriction (tenant admins, service
-- contexts).
drop policy if exists csas_tbl_party_isolation_policy
    on ores_refdata_csas_tbl;

create policy csas_tbl_party_isolation_policy
on ores_refdata_csas_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
