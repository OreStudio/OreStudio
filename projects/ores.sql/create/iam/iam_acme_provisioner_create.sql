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

-- =============================================================================
-- Acme Tenant Provisioner
--
-- One-click server-side orchestration for --source acme: imports the Acme
-- LEI party hierarchy (all four legal entities in one shot), then for each
-- of the three operating companies (Acme's holding company has no desks
-- of its own) publishes that company's business units, portfolios, books,
-- accounts, and account contact informations, plus a single tenant-wide
-- import of real GLEIF counterparties (small) so every Acme party can
-- trade against a realistic counterparty set. Called from a single NATS
-- request/handler (see ores.iam.core/messaging/acme_provisioner_handler.hpp)
-- -- no repeated per-party logins, no orchestration logic client-side.
-- =============================================================================

create or replace function ores_iam_provision_acme_tenant_fn(
    p_target_tenant_id uuid,
    p_performed_by text default current_user
)
returns table (
    step text,
    action text,
    record_count bigint
) as $$
declare
    v_company record;
    v_party_id uuid;
    v_dataset_id uuid;
    v_row record;
begin
    -- Step 1: business centres (required for party creation).
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'fpml.business_center'
      and valid_to = ores_utility_infinity_timestamp_fn();
    if v_dataset_id is not null then
        for v_row in
            select * from ores_refdata_publish_business_centres_from_dq_fn(
                v_dataset_id, p_target_tenant_id, 'upsert', '{}'::jsonb)
        loop
            step := 'business_centres'; action := v_row.action; record_count := v_row.record_count;
            return next;
        end loop;
    end if;

    -- Step 2: the four-party Acme Bank hierarchy, in one shot.
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'acme.lei_parties'
      and valid_to = ores_utility_infinity_timestamp_fn();
    if v_dataset_id is null then
        raise exception 'Dataset not found: acme.lei_parties';
    end if;
    for v_row in
        select * from ores_refdata_publish_lei_parties_from_dq_fn(
            v_dataset_id, p_target_tenant_id, 'upsert',
            '{"root_lei": "9695ACMEGROUP0000030"}'::jsonb)
    loop
        step := 'lei_parties'; action := v_row.action; record_count := v_row.record_count;
        return next;
    end loop;

    -- Step 3: real GLEIF counterparties (small), tenant-wide, so every
    -- Acme party can trade against a realistic counterparty set.
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'gleif.lei_counterparties.small'
      and valid_to = ores_utility_infinity_timestamp_fn();
    if v_dataset_id is not null then
        for v_row in
            select * from ores_refdata_publish_lei_counterparties_from_dq_fn(
                v_dataset_id, p_target_tenant_id, 'upsert', '{}'::jsonb)
        loop
            step := 'lei_counterparties'; action := v_row.action; record_count := v_row.record_count;
            return next;
        end loop;
    end if;

    -- Step 4: per-company business units, portfolios, books, accounts,
    -- and account contact informations (the holding company has no
    -- desks/staff of its own).
    for v_company in
        select * from (values
            ('acme_uk', 'Acme Bank UK plc'),
            ('acme_us', 'Acme Bank US Inc'),
            ('acme_hk', 'Acme Bank HK Ltd')
        ) as t(code, full_name)
    loop
        select id into v_party_id
        from ores_refdata_parties_tbl
        where tenant_id = p_target_tenant_id
          and full_name = v_company.full_name
          and valid_to = ores_utility_infinity_timestamp_fn();

        if v_party_id is null then
            continue;
        end if;

        select id into v_dataset_id
        from ores_dq_datasets_tbl
        where code = 'acme.' || v_company.code || '.business_units'
          and valid_to = ores_utility_infinity_timestamp_fn();
        if v_dataset_id is not null then
            for v_row in
                select * from ores_refdata_publish_business_units_from_dq_fn(
                    v_dataset_id, p_target_tenant_id, 'upsert',
                    jsonb_build_object('party_id', v_party_id))
            loop
                step := v_company.code || '.business_units';
                action := v_row.action; record_count := v_row.record_count;
                return next;
            end loop;
        end if;

        select id into v_dataset_id
        from ores_dq_datasets_tbl
        where code = 'acme.' || v_company.code || '.portfolios'
          and valid_to = ores_utility_infinity_timestamp_fn();
        if v_dataset_id is not null then
            for v_row in
                select * from ores_refdata_publish_portfolios_from_dq_fn(
                    v_dataset_id, p_target_tenant_id, 'upsert',
                    jsonb_build_object('party_id', v_party_id))
            loop
                step := v_company.code || '.portfolios';
                action := v_row.action; record_count := v_row.record_count;
                return next;
            end loop;
        end if;

        select id into v_dataset_id
        from ores_dq_datasets_tbl
        where code = 'acme.' || v_company.code || '.books'
          and valid_to = ores_utility_infinity_timestamp_fn();
        if v_dataset_id is not null then
            for v_row in
                select * from ores_refdata_publish_books_from_dq_fn(
                    v_dataset_id, p_target_tenant_id, 'upsert',
                    jsonb_build_object('party_id', v_party_id))
            loop
                step := v_company.code || '.books';
                action := v_row.action; record_count := v_row.record_count;
                return next;
            end loop;
        end if;

        select id into v_dataset_id
        from ores_dq_datasets_tbl
        where code = 'acme.' || v_company.code || '.accounts'
          and valid_to = ores_utility_infinity_timestamp_fn();
        if v_dataset_id is not null then
            for v_row in
                select * from ores_iam_publish_accounts_from_dq_fn(
                    v_dataset_id, p_target_tenant_id, 'upsert',
                    jsonb_build_object('party_id', v_party_id))
            loop
                step := v_company.code || '.accounts';
                action := v_row.action; record_count := v_row.record_count;
                return next;
            end loop;
        end if;

        select id into v_dataset_id
        from ores_dq_datasets_tbl
        where code = 'acme.' || v_company.code || '.account_contact_informations'
          and valid_to = ores_utility_infinity_timestamp_fn();
        if v_dataset_id is not null then
            for v_row in
                select * from ores_iam_publish_account_contact_informations_from_dq_fn(
                    v_dataset_id, p_target_tenant_id, 'upsert', '{}'::jsonb)
            loop
                step := v_company.code || '.account_contact_informations';
                action := v_row.action; record_count := v_row.record_count;
                return next;
            end loop;
        end if;
    end loop;

    return;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
