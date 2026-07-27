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
-- of the three operating companies (ACME Corporation's holding company has no desks
-- of its own) publishes that company's business units, portfolios, books,
-- accounts, and account contact informations, plus a single tenant-wide
-- import of real GLEIF counterparties (small) so every ACME Corporation party can
-- trade against a realistic counterparty set, with a demo logo already
-- attached to one of them (BARCLAYS PLC). Called from a single NATS
-- request/handler (see ores.iam.core/messaging/tenant_handler.hpp)
-- -- no repeated per-party logins, no orchestration logic client-side.
--
-- Note: the downstream ores_refdata_publish_*_from_dq_fn /
-- ores_iam_publish_*_from_dq_fn functions this orchestrates do not accept
-- a performed_by/actor parameter today (they all attribute rows to
-- current_user), so this function does not accept one either -- there is
-- nothing to thread it through to yet.
-- =============================================================================

-- =============================================================================
-- Activates a party, attaches the Acme Corporation demo logo, and marks its
-- onboarding wizard complete -- effects normally produced by the shell/
-- wizard "provision party" flow's final phase (see
-- ores.shell/provision_commands.cpp), needed here because --source acme
-- publishes party-scoped data directly in SQL rather than via that generic
-- per-party flow. LEI-imported parties land Inactive with no
-- onboarding.party setting, which would otherwise re-launch the Party
-- Provisioning Wizard on every login (see auth_is_party_onboarding_complete
-- in ores.iam.core/messaging/auth_handler.hpp).
--
-- Uses the same select-rowtype/mutate/reinsert idiom as
-- ores_refdata_parties_touch_version_fn: the parties table only has an
-- INSERT trigger (append-only temporal versioning), so a raw UPDATE would
-- bypass it and silently skip the version bump / history trail. Status and
-- image_id are combined into a single reinsert (rather than one each) to
-- avoid a spurious double version bump on first activation.
-- =============================================================================
create or replace function ores_iam_acme_activate_party_fn(
    p_tenant_id uuid,
    p_party_id uuid
) returns void as $$
declare
    rec ores_refdata_parties_tbl%rowtype;
    v_actor text;
    v_logo_image_id uuid;
    v_template_image record;
begin
    select * into rec
    from "ores_refdata_parties_tbl"
    where tenant_id = p_tenant_id
      and id = p_party_id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    v_actor := coalesce(ores_iam_current_service_fn(), current_user);

    if found and rec.image_id is null then
        select image_id into v_logo_image_id
        from ores_assets_images_tbl
        where tenant_id = p_tenant_id
          and key = 'acme_party_logo'
          and valid_to = ores_utility_infinity_timestamp_fn();

        if v_logo_image_id is null then
            select image_id, key, description, mime_type, data into v_template_image
            from ores_assets_images_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and key = 'acme_party_logo'
              and valid_to = ores_utility_infinity_timestamp_fn();

            if v_template_image.image_id is not null then
                v_logo_image_id := gen_random_uuid();
                insert into ores_assets_images_tbl (
                    image_id, tenant_id, version, key, description, mime_type, data,
                    modified_by, performed_by, change_reason_code, change_commentary
                ) values (
                    v_logo_image_id, p_tenant_id, 0,
                    v_template_image.key, v_template_image.description,
                    v_template_image.mime_type, v_template_image.data,
                    v_actor, current_user, 'system.external_data_import',
                    'Copied from system-tenant template: ' || v_template_image.key
                );
            end if;
        end if;
    end if;

    if found and (rec.status != 'Active' or (rec.image_id is null and v_logo_image_id is not null)) then
        rec.version := 0;
        rec.status := 'Active';
        if rec.image_id is null then
            rec.image_id := v_logo_image_id;
        end if;
        rec.modified_by := v_actor;
        rec.performed_by := current_user;
        rec.change_reason_code := 'system.external_data_import';
        rec.change_commentary := 'Activated (and logo attached) during Acme provisioning';

        insert into "ores_refdata_parties_tbl"
        select (rec).*;
    end if;

    if not exists (
        select 1 from ores_variability_system_settings_tbl
        where tenant_id = p_tenant_id
          and party_id = p_party_id
          and name = 'onboarding.party'
          and value = 'true'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_variability_system_settings_tbl (
            name, tenant_id, party_id, version, value, data_type,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            'onboarding.party', p_tenant_id, p_party_id, 0, 'true', 'boolean',
            v_actor, current_user,
            'system.external_data_import', 'Set during Acme provisioning'
        );
    end if;

    -- Associate every existing tenant account with this party -- the
    -- generic "provision party" flow does this as part of activation
    -- (see accounts_commands.cpp's account_party_repository usage);
    -- without it, the tenant admin has no party membership beyond the
    -- auto-created System Party and cannot select or default to any
    -- Acme Corporation party.
    insert into ores_iam_account_parties_tbl (
        account_id, tenant_id, party_id, version,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select a.id, p_tenant_id, p_party_id, 0,
        v_actor, current_user, 'system.external_data_import',
        'Associated during Acme provisioning'
    from ores_iam_accounts_tbl a
    where a.tenant_id = p_tenant_id
      and a.account_type != 'service'
      and a.valid_to = ores_utility_infinity_timestamp_fn()
      and not exists (
          select 1 from ores_iam_account_parties_tbl ap
          where ap.tenant_id = p_tenant_id
            and ap.account_id = a.id
            and ap.party_id = p_party_id
            and ap.valid_to = ores_utility_infinity_timestamp_fn()
      );
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace function ores_iam_provision_acme_tenant_fn(
    p_target_tenant_id uuid
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
    v_template_image record;
    v_logo_image_id uuid;
    v_account_rec ores_iam_accounts_tbl%rowtype;
    v_counterparty_rec ores_refdata_counterparties_tbl%rowtype;
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

    -- Step 2: the four-party ACME Corporation hierarchy, in one shot.
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

    -- The holding company has no desks/staff (skipped by Step 4's loop
    -- below), but still needs activating and its onboarding wizard
    -- suppressed -- LEI-imported parties land Inactive, and the
    -- per-party onboarding flag defaults to unset (see
    -- auth_is_party_onboarding_complete), which would otherwise
    -- re-launch the Party Provisioning Wizard on every login.
    select id into v_party_id
    from ores_refdata_parties_tbl
    where tenant_id = p_target_tenant_id
      and full_name = 'Acme Corporation Plc'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_party_id is not null then
        perform ores_iam_acme_activate_party_fn(p_target_tenant_id, v_party_id);
        step := 'acme_group.onboarding'; action := 'completed'; record_count := 1;
        return next;

        -- Default every tenant account to the holding party (mirrors
        -- barclays_system_provision.ores's "accounts set-default-party") so
        -- login lands on Acme Corporation Plc instead of the auto-created,
        -- inactive System Party -- without this, real users of --source
        -- acme still see the Party Provisioning Wizard on first login.
        -- Accounts is append-only temporal (INSERT trigger only, same as
        -- parties) -- a raw UPDATE would bypass versioning/history.
        for v_account_rec in
            select * from ores_iam_accounts_tbl
            where tenant_id = p_target_tenant_id
              and account_type != 'service'
              and (default_party_id is distinct from v_party_id)
              and valid_to = ores_utility_infinity_timestamp_fn()
            for update
        loop
            v_account_rec.version := 0;
            v_account_rec.default_party_id := v_party_id;
            v_account_rec.modified_by := coalesce(ores_iam_current_service_fn(), current_user);
            v_account_rec.performed_by := current_user;
            v_account_rec.change_reason_code := 'system.external_data_import';
            v_account_rec.change_commentary := 'Defaulted to holding party during Acme provisioning';

            insert into ores_iam_accounts_tbl
            select (v_account_rec).*;

            step := 'acme_group.default_party'; action := 'set'; record_count := 1;
            return next;
        end loop;
    end if;

    -- Step 3: real GLEIF counterparties (small), tenant-wide, so every
    -- ACME Corporation party can trade against a realistic counterparty set.
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
            ('acme_uk', 'ACME Corporation UK plc'),
            ('acme_us', 'ACME Corporation US Inc'),
            ('acme_hk', 'ACME Corporation HK Ltd')
        ) as t(code, full_name)
    loop
        select id into v_party_id
        from ores_refdata_parties_tbl
        where tenant_id = p_target_tenant_id
          and full_name = v_company.full_name
          and valid_to = ores_utility_infinity_timestamp_fn();

        if v_party_id is null then
            step := v_company.code || '.skipped';
            action := 'party_not_found'; record_count := 0;
            return next;
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

        perform ores_iam_acme_activate_party_fn(p_target_tenant_id, v_party_id);
        step := v_company.code || '.onboarding'; action := 'completed'; record_count := 1;
        return next;
    end loop;

    -- Step 5: attach a demo logo to one of the real, already-imported GLEIF
    -- counterparties -- Acme Corporation is a party (the synthetic
    -- holding-group tenant under test); it trades with real GLEIF
    -- counterparties, it is not itself also a counterparty, so there is no
    -- synthetic "Acme Corp" counterparty to create here. BARCLAYS PLC
    -- (short_code BRCLYS) is picked as a recognisable, deterministic
    -- example so a tester sees a working logo with no manual upload
    -- needed.
    select image_id, key, description, mime_type, data into v_template_image
    from ores_assets_images_tbl
    where tenant_id = ores_utility_system_tenant_id_fn()
      and key = 'demo_counterparty_logo'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_template_image.image_id is null then
        step := 'demo_counterparty_logo.skipped';
        action := 'template_not_found'; record_count := 0;
        return next;
    else
        select image_id into v_logo_image_id
        from ores_assets_images_tbl
        where tenant_id = p_target_tenant_id
          and key = v_template_image.key
          and valid_to = ores_utility_infinity_timestamp_fn();

        if v_logo_image_id is null then
            v_logo_image_id := gen_random_uuid();
            insert into ores_assets_images_tbl (
                image_id, tenant_id, version, key, description, mime_type, data,
                modified_by, performed_by, change_reason_code, change_commentary
            ) values (
                v_logo_image_id, p_target_tenant_id, 0,
                v_template_image.key, v_template_image.description,
                v_template_image.mime_type, v_template_image.data,
                coalesce(ores_iam_current_service_fn(), current_user), current_user,
                'system.external_data_import',
                'Copied from system-tenant template: ' || v_template_image.key
            );
            step := 'demo_counterparty_logo'; action := 'inserted'; record_count := 1;
            return next;
        end if;

        -- Counterparties is append-only temporal (INSERT trigger only,
        -- same idiom as ores_iam_acme_activate_party_fn above) -- a raw
        -- UPDATE would bypass versioning/history.
        select * into v_counterparty_rec
        from ores_refdata_counterparties_tbl
        where tenant_id = p_target_tenant_id
          and short_code = 'BRCLYS'
          and valid_to = ores_utility_infinity_timestamp_fn()
        for update;

        if found and v_counterparty_rec.image_id is null then
            v_counterparty_rec.version := 0;
            v_counterparty_rec.image_id := v_logo_image_id;
            v_counterparty_rec.modified_by := coalesce(ores_iam_current_service_fn(), current_user);
            v_counterparty_rec.performed_by := current_user;
            v_counterparty_rec.change_reason_code := 'system.external_data_import';
            v_counterparty_rec.change_commentary :=
                'Attached demo logo to BARCLAYS PLC during Acme provisioning';

            insert into ores_refdata_counterparties_tbl
            select (v_counterparty_rec).*;

            step := 'demo_counterparty_logo.attached'; action := 'updated'; record_count := 1;
            return next;
        end if;
    end if;

    return;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
