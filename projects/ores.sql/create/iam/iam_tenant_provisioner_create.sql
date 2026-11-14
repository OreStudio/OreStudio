/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
-- Tenant Provisioning Functions
-- =============================================================================
-- These functions handle the complete lifecycle of tenant creation,
-- including copying required IAM base data from the system tenant.

-- -----------------------------------------------------------------------------
-- Provision a new tenant with IAM base data
-- -----------------------------------------------------------------------------
-- Creates a new tenant and copies system IAM and refdata lookup tables:
--   - permissions: Permission definitions
--   - roles: Role definitions
--   - role_permissions: Role-permission assignments
--   - party_categories, party_types, party_statuses: Party classification
--   - contact_types, party_id_schemes: Contact and identifier lookups
--   - book_statuses, regulatory_book_types, book_purpose_types,
--     ledger_feed_types, purpose_types: Book and purpose lookups
--   - business_day_convention_types: Business day adjustment conventions
--   - convention_kinds: The kinds an atomised convention row may name
--   - day_count_fraction_types: Day count fraction conventions
--   - day_counters, calendar_names, curve_sections, curve_segment_types: ORE
--     curve configuration vocabulary
--   - diary_entry_types: Calendar event kinds (e.g. central_bank_meeting)
--   - rounding_types: Rounding method definitions
--   - monetary_natures, currency_market_tiers, currency_pair_classifications:
--     Currency and pair classification
--   - tenor_anchors, tenors, tenor_conventions, tenor_convention_resolutions:
--     Tenor labels and their per-convention resolution rules. A resolution's
--     schedule_code and schedule_step_count are not copied: the tenor
--     schedules they name arrive later, from the refdata.tenor_schedules
--     dataset, whose publish sets them.
--
-- Transactional reference data (currencies, countries, parties, etc.)
-- is NOT copied during provisioning. Tenants populate that data via the
-- dataset library using the "Publish Datasets" feature.
--
-- The system tenant must exist and be active.
-- Caller must have system tenant context set.
--
-- Parameters:
--   p_type: Tenant type ('production', 'evaluation', or 'automation')
--   p_code: Unique tenant code (e.g., 'acme', 'test_20260201_143052_abc')
--   p_name: Display name for the tenant
--   p_hostname: Unique hostname (e.g., 'acme.example.com', 'test_123.localhost')
--   p_description: Optional description
--
-- Returns: (tenant_id uuid, system_party_id uuid)
--
create or replace function ores_iam_provision_tenant_fn(
    p_type        text,
    p_code        text,
    p_name        text,
    p_hostname    text,
    p_description text default null,
    p_actor       text default null,
    out tenant_id      uuid,
    out system_party_id uuid
) as $$
declare
    v_system_tenant_id uuid;
    v_tenant_id        uuid;
    v_copied_count     integer;
    v_actor            text;
    v_owner_id         uuid;
begin
    -- Resolve actor: use explicit parameter if provided, otherwise fall back to
    -- session GUC (ores_iam_current_actor_fn), then current_user.
    -- current_user is always ores_ddl_user inside SECURITY DEFINER functions.
    v_actor := coalesce(nullif(p_actor, ''), ores_iam_current_actor_fn(), current_user);

    -- Get system tenant ID
    v_system_tenant_id := ores_utility_system_tenant_id_fn();

    -- Verify we're in system tenant context (required to create tenants)
    if ores_iam_current_tenant_id_fn() != v_system_tenant_id then
        raise exception 'Must be in system tenant context to provision a new tenant. Current tenant: %',
            ores_iam_current_tenant_id_fn() using errcode = '42501';
    end if;

    -- Verify system tenant exists and is active
    if not exists (
        select 1 from ores_iam_tenants_tbl
        where id = v_system_tenant_id
        and status = 'active'
        and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'System tenant does not exist or is not active. Cannot provision new tenant.'
            using errcode = '23503';
    end if;

    -- Generate new tenant ID
    tenant_id := gen_random_uuid();
    v_tenant_id := tenant_id;

    -- Create the tenant record
    -- Note: The trigger sets tenant_id = system_tenant_id (all tenants owned by system)
    insert into ores_iam_tenants_tbl (
        id, type, code, name, description, hostname, status,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        tenant_id, p_type, p_code, p_name, p_description, p_hostname, 'bootstrapping',
        v_actor, v_actor, 'system.new_record', 'Tenant provisioned'
    );

    raise notice 'Created tenant: % (id: %)', p_code, tenant_id;

    -- NOTE: We keep system tenant context during data copying.
    -- The SELECTs need system tenant context to read source data (due to RLS).
    -- The INSERTs explicitly specify tenant_id, which the trigger validates.

    -- =========================================================================
    -- Copy IAM data from system tenant
    -- =========================================================================

    -- Copy permissions
    insert into ores_iam_permissions_tbl (id, tenant_id, code, description, valid_from, valid_to)
    select gen_random_uuid(), v_tenant_id, p.code, p.description,
           current_timestamp, ores_utility_infinity_timestamp_fn()
    from ores_iam_permissions_tbl p
    where p.tenant_id = v_system_tenant_id
    and p.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % permissions', v_copied_count;

    -- Copy roles (versioned table with audit columns). The askable flag comes
    -- across, so a provisioned tenant offers its members the same roles the
    -- starting point does and no others.
    insert into ores_iam_roles_tbl (
        id, tenant_id, name, description, is_requestable,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        gen_random_uuid(), v_tenant_id, r.name, r.description, r.is_requestable,
        v_actor, v_actor, 'system.new_record', 'Copied from system tenant during provisioning'
    from ores_iam_roles_tbl r
    where r.tenant_id = v_system_tenant_id
    and r.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % roles', v_copied_count;

    -- Copy role_permissions. Need to map old role/permission IDs to new IDs
    -- by name/code. The copy states the same tail the roles copy does, so a
    -- provisioned tenant's bundle rows say where they came from.
    -- A version of 0 states no earlier version to match: the trigger numbers the row.
    insert into ores_iam_role_permissions_tbl (
        tenant_id, role_id, permission_id, version,
        modified_by, performed_by,
        change_reason_code, change_commentary)
    select
        v_tenant_id,
        new_r.id,
        new_p.id,
        0,
        v_actor, v_actor,
        'system.new_record', 'Copied from system tenant during provisioning'
    from ores_iam_role_permissions_tbl rp
    join ores_iam_roles_tbl old_r on old_r.id = rp.role_id
        and old_r.tenant_id = v_system_tenant_id
        and old_r.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_iam_permissions_tbl old_p on old_p.id = rp.permission_id
        and old_p.tenant_id = v_system_tenant_id
        and old_p.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_iam_roles_tbl new_r on new_r.name = old_r.name
        and new_r.tenant_id = v_tenant_id
        and new_r.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_iam_permissions_tbl new_p on new_p.code = old_p.code
        and new_p.tenant_id = v_tenant_id
        and new_p.valid_to = ores_utility_infinity_timestamp_fn()
    where rp.tenant_id = v_system_tenant_id
    and rp.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % role-permission assignments', v_copied_count;

    -- Give every copied role the whole product type catalogue. The junction
    -- is the grant, so a role with no rows permits nothing, and a tenant that
    -- could not book what the system catalogue states would be provisioned
    -- unable to trade. The catalogue is the one copy in the system tenant, and
    -- it is read, not copied: a product type added to it is bookable by the
    -- next tenant provisioned without this script changing.
    insert into ores_iam_role_product_types_tbl (
        tenant_id, role_id, product_type_code, version,
        modified_by, performed_by,
        change_reason_code, change_commentary)
    select
        v_tenant_id,
        r.id,
        c.code,
        0,
        v_actor, v_actor,
        'system.new_record', 'The system catalogue copied during provisioning'
    from ores_iam_roles_tbl r
    cross join ores_trading_product_types_tbl c
    where r.tenant_id = v_tenant_id
      and r.valid_to = ores_utility_infinity_timestamp_fn()
      and c.tenant_id = v_system_tenant_id
      and c.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % role-product type grants', v_copied_count;

    -- =========================================================================
    -- Copy refdata lookup tables from system tenant
    -- =========================================================================
    -- Validation triggers on parties and counterparties check these lookup
    -- tables against the tenant's own data. They must be seeded before
    -- inserting the system party.

    -- Party categories (e.g. Operational, System)
    insert into ores_refdata_party_categories_tbl (
        code, tenant_id, version, name, description, display_order,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        code, v_tenant_id, 0, name, description, display_order,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_refdata_party_categories_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % party categories', v_copied_count;

    /*
     * The trading code lists a booked trade is read against: the scheme its
     * identifier is stated in, the kinds of link two trades can have, the
     * activities that move it, and the structures it can be a leg of. Without
     * them a tenant answers an empty list to every picker while the writes still
     * accept the codes. The product type catalogue is not among them: it is one
     * copy in the system tenant, which every tenant reads.
     */
    -- Trade ID types
    insert into ores_trading_trade_id_types_tbl (
        code, description, tenant_id, version,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        code, description, v_tenant_id, 0,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_trading_trade_id_types_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % trade ID types', v_copied_count;

    -- Trade link types
    insert into ores_trading_trade_link_types_tbl (
        code, description, from_role, to_role, has_economic_effect, tenant_id, version,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        code, description, from_role, to_role, has_economic_effect, v_tenant_id, 0,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_trading_trade_link_types_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % trade link types', v_copied_count;

    -- Activity types
    insert into ores_trading_activity_types_tbl (
        code, category, requires_confirmation, is_economic, is_real, priority, description, fpml_event_type_code, fsm_transition_id, tenant_id, version,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        code, category, requires_confirmation, is_economic, is_real, priority, description, fpml_event_type_code, fsm_transition_id, v_tenant_id, 0,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_trading_activity_types_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % activity types', v_copied_count;

    -- Structure kinds
    insert into ores_trading_structure_kinds_tbl (
        code, description, confirms_as_whole, tenant_id, version,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        code, description, confirms_as_whole, v_tenant_id, 0,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_trading_structure_kinds_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % structure kinds', v_copied_count;

    -- Structure templates
    insert into ores_trading_structure_templates_tbl (
        code, description, kind, tenant_id, version,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        code, description, kind, v_tenant_id, 0,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_trading_structure_templates_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % structure templates', v_copied_count;

    -- Structure template roles
    insert into ores_trading_structure_template_roles_tbl (
        template_code, role, min_legs, max_legs, description, tenant_id, version,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        template_code, role, min_legs, max_legs, description, v_tenant_id, 0,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_trading_structure_template_roles_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % structure template roles', v_copied_count;

    -- DQ change reason categories (system, common, trade taxonomy)
    insert into ores_dq_change_reason_categories_tbl (
        code, tenant_id, version, description,
        modified_by, performed_by, change_commentary
    )
    select
        t.code, v_tenant_id, 0, t.description,
        v_actor, v_actor, 'Copied from system tenant during provisioning'
    from ores_dq_change_reason_categories_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % change reason categories', v_copied_count;

    -- DQ change reasons (full regulatory taxonomy)
    insert into ores_dq_change_reasons_tbl (
        code, tenant_id, version, description, category_code,
        applies_to_new, applies_to_amend, applies_to_delete,
        requires_commentary, display_order,
        modified_by, performed_by, change_commentary
    )
    select
        t.code, v_tenant_id, 0, t.description, t.category_code,
        t.applies_to_new, t.applies_to_amend, t.applies_to_delete,
        t.requires_commentary, t.display_order,
        v_actor, v_actor, 'Copied from system tenant during provisioning'
    from ores_dq_change_reasons_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % change reasons', v_copied_count;

    -- DQ artefact types (staging/publish infrastructure mapping) -- copied
    -- per-tenant rather than left system-tenant-only + shared-read, so a
    -- tenant admin's edit through the Qt UI closes THIS tenant's own row
    -- (the insert trigger's "close previous version" lookup filters on
    -- tenant_id = NEW.tenant_id, which can never match a system-tenant row).
    insert into ores_dq_artefact_types_tbl (
        code, tenant_id, version, name, description,
        artefact_table, target_table, target_subject, display_order,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        t.code, v_tenant_id, 0, t.name, t.description,
        t.artefact_table, t.target_table, t.target_subject, t.display_order,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_dq_artefact_types_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % artefact types', v_copied_count;

    -- DQ dataset bundles -- same rationale as artefact types above.
    insert into ores_dq_dataset_bundles_tbl (
        id, tenant_id, version, code, name, description,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        t.id, v_tenant_id, 0, t.code, t.name, t.description,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_dq_dataset_bundles_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % dataset bundles', v_copied_count;

    -- The following DQ reference/lookup tables share the exact same
    -- shared-tenant-edit hazard as artefact_types/dataset_bundles above:
    -- DDL-seeded under the system tenant, tenant_read_scope: shared (or
    -- hand-maintained equivalent), and editable through the Qt UI. Copied
    -- in dependency order (data_domains before subject_areas/coding_schemes,
    -- badge_severities before badge_definitions) though none of the FK
    -- validations below are actually tenant-scoped -- they just check
    -- existence, which the untouched system-tenant rows still satisfy.
    -- ores_dq_datasets_tbl is deliberately NOT included here: its insert
    -- trigger validates seven FK references (catalog, subject_area, coding
    -- scheme, three dimensions, methodology, artefact_type), and it is not
    -- DDL-seeded as a standalone entity the same way as the tables below --
    -- tracked separately rather than rushed.

    -- DQ data domains
    insert into ores_dq_data_domains_tbl (
        name, tenant_id, version, description,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        t.name, v_tenant_id, 0, t.description,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_dq_data_domains_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % data domains', v_copied_count;

    -- DQ subject areas (compound natural key: name + domain_name)
    insert into ores_dq_subject_areas_tbl (
        name, tenant_id, version, domain_name, description,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        t.name, v_tenant_id, 0, t.domain_name, t.description,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_dq_subject_areas_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % subject areas', v_copied_count;

    -- DQ coding scheme authority types
    insert into ores_dq_coding_scheme_authority_types_tbl (
        code, tenant_id, version, name, description,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        t.code, v_tenant_id, 0, t.name, t.description,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_dq_coding_scheme_authority_types_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % coding scheme authority types', v_copied_count;

    -- DQ coding schemes
    insert into ores_dq_coding_schemes_tbl (
        code, tenant_id, version, name, authority_type,
        subject_area_name, domain_name, uri, description,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        t.code, v_tenant_id, 0, t.name, t.authority_type,
        t.subject_area_name, t.domain_name, t.uri, t.description,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_dq_coding_schemes_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % coding schemes', v_copied_count;

    -- DQ origin dimensions
    insert into ores_dq_origin_dimensions_tbl (
        code, tenant_id, version, name, description,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        t.code, v_tenant_id, 0, t.name, t.description,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_dq_origin_dimensions_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % origin dimensions', v_copied_count;

    -- DQ nature dimensions
    insert into ores_dq_nature_dimensions_tbl (
        code, tenant_id, version, name, description,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        t.code, v_tenant_id, 0, t.name, t.description,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_dq_nature_dimensions_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % nature dimensions', v_copied_count;

    -- DQ treatment dimensions
    insert into ores_dq_treatment_dimensions_tbl (
        code, tenant_id, version, name, description,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        t.code, v_tenant_id, 0, t.name, t.description,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_dq_treatment_dimensions_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % treatment dimensions', v_copied_count;

    -- DQ methodologies
    insert into ores_dq_methodologies_tbl (
        id, tenant_id, version, name, description,
        logic_reference, implementation_details,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        t.id, v_tenant_id, 0, t.name, t.description,
        t.logic_reference, t.implementation_details,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_dq_methodologies_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % methodologies', v_copied_count;

    -- DQ badge severities
    insert into ores_dq_badge_severities_tbl (
        code, tenant_id, version, name, description, display_order,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        t.code, v_tenant_id, 0, t.name, t.description, t.display_order,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_dq_badge_severities_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % badge severities', v_copied_count;

    -- DQ badge definitions (FK: severity_code -> badge_severities, copied above)
    insert into ores_dq_badge_definitions_tbl (
        code, tenant_id, version, name, description,
        background_colour, text_colour, severity_code, css_class, display_order,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        t.code, v_tenant_id, 0, t.name, t.description,
        t.background_colour, t.text_colour, t.severity_code, t.css_class, t.display_order,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_dq_badge_definitions_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % badge definitions', v_copied_count;

    -- Series classification rules (the taxonomy: which asset classes and
    -- which series subclass each ORE series type maps onto). Every tenant
    -- reads this table to build its classifier, and the reader rejects an
    -- empty table, so a tenant without these rows cannot import market data
    -- at all.
    insert into ores_marketdata_series_classification_rules_tbl (
        instrument, quote, key_kind, tenant_id, version, asset_class_source,
        asset_class_code, series_subclass_code, description,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        t.instrument, t.quote, t.key_kind, v_tenant_id, 0, t.asset_class_source,
        t.asset_class_code, t.series_subclass_code, t.description,
        v_actor, v_actor, 'system.new_record',
        'Copied from system tenant during provisioning'
    from ores_marketdata_series_classification_rules_tbl t
    where t.tenant_id = v_system_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % series classification rules', v_copied_count;

    -- =========================================================================
    -- Create the system party for the new tenant
    -- =========================================================================
    -- Every tenant gets exactly one system party (party_category='System')
    -- which serves as the root of the party hierarchy. The system party
    -- represents the tenant organisation itself. It is the only party with
    -- parent_party_id = NULL, enforced by a partial unique index.

    -- Seed the WRLD business centre for the new tenant so that the system
    -- party's business_center_code passes validation if later edited via UI.
    insert into ores_refdata_business_centres_tbl (
        code, tenant_id, version, coding_scheme_code,
        description, modified_by, performed_by,
        change_reason_code, change_commentary
    ) values (
        'WRLD', v_tenant_id, 0, 'NONE',
        'World. Global business centre for entities not tied to a specific geographic location.',
        v_actor, v_actor,
        'system.new_record', 'System business centre for tenant'
    );

    raise notice 'Created WRLD business centre for tenant: %', p_code;

    system_party_id := gen_random_uuid();
    insert into ores_refdata_parties_tbl (
        id, tenant_id, full_name, short_code, party_category,
        party_type, business_center_code, parent_party_id, status,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        system_party_id, v_tenant_id,
        'System Party', p_code || '_system', 'System',
        'Internal', 'WRLD', null, 'Active',
        v_actor, v_actor, 'system.new_record',
        'System party created during tenant provisioning'
    );

    raise notice 'Created system party for tenant: %', p_code;

    -- =========================================================================
    -- Seed bootstrap mode flag for the new tenant
    -- =========================================================================
    -- New tenants start in bootstrap mode so the tenant provisioning wizard
    -- appears on first login. The wizard clears this flag on completion.

    perform ores_variability_system_settings_upsert_fn(
        v_tenant_id,
        'system.bootstrap_mode',
        'true',
        'boolean',
        'Indicates whether the tenant is in bootstrap mode (waiting for initial setup).'
    );

    -- Seed the tenant onboarding-completion flag (system-party-scoped,
    -- p_party_id omitted). Cleared by the tenant provisioner wizard on
    -- completion via the same NATS-routed pattern as the bootstrap_mode
    -- clear — see ores.iam.core/messaging/tenant_handler.hpp.
    perform ores_variability_system_settings_upsert_fn(
        v_tenant_id,
        'onboarding.tenant',
        'false',
        'boolean',
        'Whether the tenant provisioner wizard has completed for this tenant.'
    );

    -- System settings are tenant-scoped (system_settings_service.get_all()
    -- reads only the caller's tenant), so a "system."-prefixed setting
    -- meant to be a global default must still be seeded per-tenant here —
    -- otherwise SettingGatedActionController silently hides the feature
    -- for every tenant except the system tenant. Copies every current
    -- system.* policy setting from the system tenant (bootstrap_mode
    -- excluded — seeded explicitly above, it's per-tenant state, not a
    -- copied policy default) so a newly-added setting is automatically
    -- seeded for future tenants without a second list to remember.
    perform ores_variability_seed_tenant_system_settings_fn(v_tenant_id);

    raise notice 'Seeded system settings for tenant: %', p_code;

    -- =========================================================================
    -- Provisioning complete
    -- =========================================================================
    -- NOTE: Reference data (currencies, countries, etc.) is NOT copied here.
    -- Tenants populate refdata via the dataset library using the "Publish
    -- Datasets" feature in the Data Librarian.

    raise notice 'Tenant provisioning complete: % (id: %)', p_code, tenant_id;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
