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
-- Tenant Reset Function
-- =============================================================================
-- Soft-deletes admin-created data for a tenant, returning it to the state
-- right after tenant onboarding completed. Intended for development iteration:
-- wipe the outputs of the party-level provisioning wizard and admin activity,
-- so those flows can be re-run without re-creating the tenant, its admin
-- account, or its tenant-onboarding state (parties, account_parties, refdata).
--
-- Sits alongside the other tenant-lifecycle functions:
--   * terminate    -- mark inactive, preserve ALL data
--   * reset        -- soft-delete admin-created data (this fn)
--   * deprovision  -- soft-delete ALL temporal data including IAM
--   * purge        -- hard-delete everything
--
-- IMPLEMENTATION NOTE — curated, default-preserve wipe list
--
-- This function soft-deletes an explicit list of tables (see v_wipe_tables
-- below). Everything else is preserved. The list is tightly coupled to the
-- PartyProvisioningWizard's publish steps and admin-activity tables:
--
--   * Counterparties (+ identifiers, contacts, party-counterparty links) —
--     written by ores_dq_lei_counterparties_publish_fn via the
--     gleif.lei_counterparties.* datasets.
--   * Business units, portfolios, books — written by the `organisation`
--     bundle (testdata.business_units / .portfolios / .books).
--   * Report definitions and scheduler job definitions — created by the
--     wizard's report-setup step and admin activity.
--
-- An earlier revision of this function used a dynamic info_schema loop with a
-- default-delete, opt-out preservation list. That was wrong: the preservation
-- list had to grow to include parties and account_parties (mandatory for
-- login — tenant onboarding sets them up and bootstrap mode does not skip the
-- check), plus all base-bundle refdata (currencies, countries, fpml, etc.).
-- A missed entry locked the admin out. Curated default-preserve is the safer
-- failure mode: a missing entry leaves the tenant in a partially-reset state,
-- which is easier to spot and fix than a broken login.
--
-- When extending: add tables only for data the tenant admin or a post-
-- onboarding wizard creates. Do NOT add tables populated by tenant
-- onboarding (base bundle publish or tenant provisioner) — those are
-- tenant infrastructure.
--
-- Trading data (trades, instruments, identifiers) is intentionally not in
-- the list today. Add it here if the dev workflow starts requiring trade
-- wipes as part of reset.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Reset a tenant (soft-delete admin-created data)
-- -----------------------------------------------------------------------------
-- The system tenant cannot be reset.
-- Caller must have system tenant context set.
--
-- Parameters:
--   p_tenant_id: The tenant ID to reset
--
create or replace function ores_iam_reset_tenant_fn(
    p_tenant_id uuid
) returns void as $$
declare
    v_system_tenant_id uuid;
    v_tenant_code text;
    v_table_name text;
    v_affected_count integer;
    v_total_soft_deleted integer := 0;
    v_sql text;
    v_wipe_tables text[] := array[
        -- Counterparties and related, populated by the PartyProvisioningWizard
        -- via ores_dq_lei_counterparties_publish_fn (gleif.lei_counterparties.*).
        'ores_refdata_counterparties_tbl',
        'ores_refdata_counterparty_identifiers_tbl',
        'ores_refdata_counterparty_contact_informations_tbl',
        'ores_refdata_party_counterparties_tbl',

        -- Organisation bundle outputs, populated by the PartyProvisioningWizard
        -- via the `organisation` bundle (testdata.business_units/.portfolios/
        -- .books).
        'ores_refdata_business_units_tbl',
        'ores_refdata_portfolios_tbl',
        'ores_refdata_books_tbl',

        -- Report and scheduler definitions, created by the wizard's report
        -- setup step and by admin activity.
        'ores_reporting_report_definitions_tbl',
        'ores_scheduler_job_definitions_tbl'
    ];
begin
    v_system_tenant_id := ores_iam_system_tenant_id_fn();

    -- Cannot reset system tenant
    if p_tenant_id = v_system_tenant_id then
        raise exception 'Cannot reset the system tenant' using errcode = '42501';
    end if;

    -- Verify we're in system tenant context
    if ores_iam_current_tenant_id_fn() != v_system_tenant_id then
        raise exception 'Must be in system tenant context to reset a tenant. Current tenant: %',
            ores_iam_current_tenant_id_fn() using errcode = '42501';
    end if;

    -- Get tenant code for logging; must be active
    select code into v_tenant_code
    from ores_iam_tenants_tbl
    where id = p_tenant_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        raise exception 'Tenant not found or inactive: %', p_tenant_id
            using errcode = '23503';
    end if;

    raise notice 'Resetting tenant (tenant-onboarding state preserved): % (id: %)',
        v_tenant_code, p_tenant_id;

    -- Soft-delete only the explicitly listed tables.
    foreach v_table_name in array v_wipe_tables
    loop
        v_sql := format(
            'UPDATE %I SET valid_to = current_timestamp ' ||
            'WHERE tenant_id = $1 AND valid_to = ores_utility_infinity_timestamp_fn()',
            v_table_name
        );

        execute v_sql using p_tenant_id;
        get diagnostics v_affected_count = row_count;

        if v_affected_count > 0 then
            raise notice 'Soft-deleted % rows from %', v_affected_count, v_table_name;
            v_total_soft_deleted := v_total_soft_deleted + v_affected_count;
        end if;
    end loop;

    raise notice 'Total soft-deleted: % rows', v_total_soft_deleted;

    -- Flip Operational parties back to 'Inactive'. The login flow computes
    -- party_setup_required from party.status == 'Inactive' (see
    -- ores.iam.core/messaging/account_handler.hpp), so leaving parties Active
    -- means the PartyProvisioningWizard will not re-trigger on next login.
    -- Direct UPDATE bypasses the insert trigger's actor validation, which is
    -- acceptable for a dev reset — the notify trigger still fires so clients
    -- pick up the change.
    update ores_refdata_parties_tbl
    set status = 'Inactive'
    where tenant_id = p_tenant_id
      and party_category = 'Operational'
      and status = 'Active'
      and valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_affected_count = row_count;
    if v_affected_count > 0 then
        raise notice 'Marked % Operational parties as Inactive (wizard will fire on next login)',
            v_affected_count;
    end if;

    raise notice 'Tenant reset complete: % (id: %)', v_tenant_code, p_tenant_id;
end;
$$ language plpgsql;
