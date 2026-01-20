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

/**
 * Seed Data Upsert Functions
 *
 * These helper functions are used by population scripts to seed the database
 * with reference data in an idempotent manner. Each function checks if a record
 * already exists before inserting, making population scripts safe to re-run.
 *
 * Functions are organized by domain:
 * - Data Quality: Datasets, dimensions, catalogs, coding schemes, methodologies
 * - IAM: Roles, permissions, role-permission assignments
 * - Variability: Feature flags
 *
 * All functions follow the pattern:
 * 1. Check if record exists (using natural key + valid_to check for temporal tables)
 * 2. If not exists, insert with standard audit columns (modified_by, change_reason_code, etc.)
 * 3. Log action via RAISE NOTICE for visibility during population
 */

set schema 'ores';

-- =============================================================================
-- Validation Helpers
-- =============================================================================

/**
 * Validates that a text value is not null or empty (after trimming whitespace).
 * Raises an exception if validation fails.
 *
 * Validation can be disabled at runtime by setting the session variable
 * ores.skip_validation to 'on' (e.g., via recreate_database.sh -n flag).
 *
 * @param p_value The value to validate
 * @param p_field_name Descriptive name of the field (used in error message)
 */
create or replace function ores.seed_validate_not_empty(
    p_value text,
    p_field_name text
) returns void as $$
begin
    -- Check if validation is disabled via session variable
    if current_setting('ores.skip_validation', true) = 'on' then
        return;
    end if;

    if p_value is null or trim(p_value) = '' then
        raise exception '% cannot be empty', p_field_name;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Data Quality: Data Domains
-- =============================================================================

/**
 * Upsert a data domain (e.g., Reference Data, Trade Data, Market Data).
 */
create or replace function ores.upsert_dq_data_domains(
    p_name text,
    p_description text
) returns void as $$
begin
    perform ores.seed_validate_not_empty(p_name, 'Data domain name');

    if not exists (
        select 1 from ores.dq_data_domains_tbl
        where name = p_name and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_data_domains_tbl (
            name, version, description,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            p_name, 0, p_description,
            'system', 'system.new_record', 'System seed data - data quality data domain',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created data quality data domain: %', p_name;
    else
        raise notice 'Data quality data domain already exists: %', p_name;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Data Quality: Subject Areas
-- =============================================================================

/**
 * Upsert a subject area within a data domain.
 */
create or replace function ores.upsert_dq_subject_areas(
    p_domain_name text,
    p_name text,
    p_description text
) returns void as $$
begin
    perform ores.seed_validate_not_empty(p_name, 'Subject area name');
    perform ores.seed_validate_not_empty(p_domain_name, 'Domain name');

    if not exists (
        select 1 from ores.dq_subject_areas_tbl
        where name = p_name and domain_name = p_domain_name and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_subject_areas_tbl (
            name, version, domain_name, description,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            p_name, 0, p_domain_name, p_description,
            'system', 'system.new_record', 'System seed data - data quality subject area',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created data quality subject area: % in domain %', p_name, p_domain_name;
    else
        raise notice 'Data quality subject area already exists: % in domain %', p_name, p_domain_name;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Data Quality: Catalogs
-- =============================================================================

/**
 * Upsert a data quality catalog (grouping of related datasets).
 */
create or replace function ores.upsert_dq_catalogs(
    p_name text,
    p_description text,
    p_owner text default null
) returns void as $$
begin
    perform ores.seed_validate_not_empty(p_name, 'Catalog name');

    if not exists (
        select 1 from ores.dq_catalogs_tbl
        where name = p_name and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_catalogs_tbl (
            name, version, description, owner,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            p_name, 0, p_description, p_owner,
            'system', 'system.new_record', 'System seed data - data quality catalog',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created data quality catalog: %', p_name;
    else
        raise notice 'Data quality catalog already exists: %', p_name;
    end if;
end;
$$ language plpgsql;

/**
 * Upsert a catalog dependency (declares load order between catalogs).
 */
create or replace function ores.upsert_dq_catalog_dependency(
    p_catalog_name text,
    p_dependency_name text
) returns void as $$
begin
    perform ores.seed_validate_not_empty(p_catalog_name, 'Catalog name');
    perform ores.seed_validate_not_empty(p_dependency_name, 'Dependency name');

    if not exists (
        select 1 from ores.dq_catalog_dependencies_tbl
        where catalog_name = p_catalog_name
          and dependency_name = p_dependency_name
          and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_catalog_dependencies_tbl (
            catalog_name, dependency_name,
            recorded_by, change_reason_code, change_commentary,
            valid_from, valid_to
        )
        values (
            p_catalog_name, p_dependency_name,
            'system', 'system.new_record', 'System seed data - catalog dependency',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created catalog dependency: % depends on %', p_catalog_name, p_dependency_name;
    else
        raise notice 'Catalog dependency already exists: % depends on %', p_catalog_name, p_dependency_name;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Data Quality: Dimensions (Origin, Nature, Treatment)
-- =============================================================================

/**
 * Upsert an origin dimension (Primary, Derived).
 */
create or replace function ores.upsert_dq_origin_dimensions(
    p_code text,
    p_name text,
    p_description text
) returns void as $$
begin
    perform ores.seed_validate_not_empty(p_code, 'Origin dimension code');

    if not exists (
        select 1 from ores.dq_origin_dimensions_tbl
        where code = p_code and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_origin_dimensions_tbl (
            code, version, name, description,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            p_code, 0, p_name, p_description,
            'system', 'system.new_record', 'System seed data - data quality origin dimension',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created data quality origin: %', p_code;
    else
        raise notice 'Data quality origin already exists: %', p_code;
    end if;
end;
$$ language plpgsql;

/**
 * Upsert a nature dimension (Actual, Synthetic, Mock).
 */
create or replace function ores.upsert_dq_nature_dimensions(
    p_code text,
    p_name text,
    p_description text
) returns void as $$
begin
    perform ores.seed_validate_not_empty(p_code, 'Nature dimension code');

    if not exists (
        select 1 from ores.dq_nature_dimensions_tbl
        where code = p_code and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_nature_dimensions_tbl (
            code, version, name, description,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            p_code, 0, p_name, p_description,
            'system', 'system.new_record', 'System seed data - data quality nature dimension',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created data quality nature: %', p_code;
    else
        raise notice 'Data quality nature already exists: %', p_code;
    end if;
end;
$$ language plpgsql;

/**
 * Upsert a treatment dimension (Raw, Masked, Anonymized).
 */
create or replace function ores.upsert_dq_treatment_dimensions(
    p_code text,
    p_name text,
    p_description text
) returns void as $$
begin
    perform ores.seed_validate_not_empty(p_code, 'Treatment dimension code');

    if not exists (
        select 1 from ores.dq_treatment_dimensions_tbl
        where code = p_code and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_treatment_dimensions_tbl (
            code, version, name, description,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            p_code, 0, p_name, p_description,
            'system', 'system.new_record', 'System seed data - data quality treatment dimension',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created data quality treatment: %', p_code;
    else
        raise notice 'Data quality treatment already exists: %', p_code;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Data Quality: Change Reasons
-- =============================================================================

/**
 * Upsert a change reason category (system, common, trade).
 */
create or replace function ores.upsert_change_reason_category(
    p_code text,
    p_description text
) returns void as $$
begin
    perform ores.seed_validate_not_empty(p_code, 'Change reason category code');

    if not exists (
        select 1 from ores.dq_change_reason_categories_tbl
        where code = p_code and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_change_reason_categories_tbl (
            code, description, modified_by, change_commentary,
            valid_from, valid_to
        )
        values (
            p_code, p_description, 'system',
            'System seed data - standard regulatory taxonomy',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created change reason category: %', p_code;
    else
        raise notice 'Change reason category already exists: %', p_code;
    end if;
end;
$$ language plpgsql;

/**
 * Upsert a change reason with full configuration.
 */
create or replace function ores.upsert_change_reason(
    p_code text,
    p_description text,
    p_category_code text,
    p_applies_to_amend boolean,
    p_applies_to_delete boolean,
    p_requires_commentary boolean,
    p_display_order integer
) returns void as $$
begin
    perform ores.seed_validate_not_empty(p_code, 'Change reason code');

    if not exists (
        select 1 from ores.dq_change_reasons_tbl
        where code = p_code and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_change_reasons_tbl (
            code, description, category_code,
            applies_to_amend, applies_to_delete, requires_commentary, display_order,
            modified_by, change_commentary, valid_from, valid_to
        )
        values (
            p_code, p_description, p_category_code,
            p_applies_to_amend, p_applies_to_delete, p_requires_commentary, p_display_order,
            'system', 'System seed data - standard regulatory taxonomy',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created change reason: %', p_code;
    else
        raise notice 'Change reason already exists: %', p_code;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Data Quality: Coding Schemes
-- =============================================================================

/**
 * Upsert a coding scheme authority type (official, industry, internal).
 */
create or replace function ores.upsert_dq_coding_scheme_authority_type(
    p_code text,
    p_name text,
    p_description text
) returns void as $$
begin
    perform ores.seed_validate_not_empty(p_code, 'Coding scheme authority type code');

    if not exists (
        select 1 from ores.dq_coding_scheme_authority_types_tbl
        where code = p_code and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_coding_scheme_authority_types_tbl (
            code, version, name, description,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            p_code, 0, p_name, p_description,
            'system', 'system.new_record', 'System seed data - coding scheme authority type',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created coding scheme authority type: %', p_code;
    else
        raise notice 'Coding scheme authority type already exists: %', p_code;
    end if;
end;
$$ language plpgsql;

/**
 * Upsert a coding scheme (e.g., ISO 4217, LEI, BIC).
 */
create or replace function ores.upsert_dq_coding_schemes(
    p_code text,
    p_name text,
    p_authority_type text,
    p_subject_area_name text,
    p_domain_name text,
    p_uri text,
    p_description text
) returns void as $$
begin
    perform ores.seed_validate_not_empty(p_code, 'Coding scheme code');

    if not exists (
        select 1 from ores.dq_coding_schemes_tbl
        where code = p_code and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_coding_schemes_tbl (
            code, version, name, authority_type, subject_area_name, domain_name, uri, description,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            p_code, 0, p_name, p_authority_type, p_subject_area_name, p_domain_name, p_uri, p_description,
            'system', 'system.new_record', 'System seed data - coding scheme',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created coding scheme: %', p_code;
    else
        raise notice 'Coding scheme already exists: %', p_code;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Data Quality: Methodologies
-- =============================================================================

/**
 * Upsert a data sourcing methodology.
 */
create or replace function ores.upsert_dq_methodologies(
    p_name text,
    p_description text,
    p_logic_reference text default null,
    p_implementation_details text default null
) returns void as $$
begin
    perform ores.seed_validate_not_empty(p_name, 'Methodology name');

    if not exists (
        select 1 from ores.dq_methodologies_tbl
        where name = p_name
          and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_methodologies_tbl (
            id, version, name, description, logic_reference, implementation_details,
            modified_by, change_reason_code, change_commentary,
            valid_from, valid_to
        )
        values (
            gen_random_uuid(), 0, p_name, p_description, p_logic_reference, p_implementation_details,
            'system', 'system.new_record', 'System seed data',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created dq_methodologies: %', p_name;
    else
        raise notice 'dq_methodologies already exists: %', p_name;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Data Quality: Datasets and Tags
-- =============================================================================

/**
 * Upsert a dataset.
 */
create or replace function ores.upsert_dq_datasets(
    p_catalog_name text,
    p_subject_area_name text,
    p_domain_name text,
    p_coding_scheme_code text,
    p_origin_code text,
    p_nature_code text,
    p_treatment_code text,
    p_methodology_name text,
    p_name text,
    p_description text,
    p_source_system_id text,
    p_business_context text,
    p_as_of_date date,
    p_license_info text default null
) returns void as $$
declare
    v_methodology_id uuid;
begin
    perform ores.seed_validate_not_empty(p_name, 'Dataset name');

    -- Get methodology ID (only table that still uses UUID PK)
    select id into v_methodology_id from ores.dq_methodologies_tbl where name = p_methodology_name and valid_to = ores.utility_infinity_timestamp_fn();

    if v_methodology_id is null then raise exception 'Methodology not found: %', p_methodology_name; end if;

    if not exists (
        select 1 from ores.dq_datasets_tbl
        where name = p_name
          and subject_area_name = p_subject_area_name
          and domain_name = p_domain_name
          and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_datasets_tbl (
            id, version, catalog_name, subject_area_name, domain_name, coding_scheme_code,
            origin_code, nature_code, treatment_code, methodology_id,
            name, description, source_system_id, business_context,
            upstream_derivation_id, lineage_depth, as_of_date, ingestion_timestamp, license_info,
            modified_by, change_reason_code, change_commentary,
            valid_from, valid_to
        )
        values (
            gen_random_uuid(), 0, p_catalog_name, p_subject_area_name, p_domain_name, p_coding_scheme_code,
            p_origin_code, p_nature_code, p_treatment_code, v_methodology_id,
            p_name, p_description, p_source_system_id, p_business_context,
            null, 0, p_as_of_date, current_timestamp, p_license_info,
            'system', 'system.new_record', 'System seed data',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created dq_datasets: %', p_name;
    else
        raise notice 'dq_datasets already exists: %', p_name;
    end if;
end;
$$ language plpgsql;

/**
 * Upsert a tag for a dataset.
 */
create or replace function ores.upsert_dq_tag(
    p_dataset_name text,
    p_subject_area_name text,
    p_domain_name text,
    p_tag_name text,
    p_tag_description text
) returns void as $$
declare
    v_dataset_id uuid;
begin
    perform ores.seed_validate_not_empty(p_tag_name, 'Tag name');

    -- Get dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where name = p_dataset_name
      and subject_area_name = p_subject_area_name
      and domain_name = p_domain_name
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: name=%, subject_area=%, domain=%',
            p_dataset_name, p_subject_area_name, p_domain_name;
    end if;

    -- Insert tag if it doesn't exist (uses unique constraint for atomicity)
    insert into ores.dq_tags_artefact_tbl (
        dataset_id, tag_id, version, name, description
    ) values (
        v_dataset_id, gen_random_uuid(), 0, p_tag_name, p_tag_description
    )
    on conflict (dataset_id, name) do nothing;

    if found then
        raise notice 'Created dq_tag: % for dataset %', p_tag_name, p_dataset_name;
    else
        raise notice 'Tag already exists: % for dataset %', p_tag_name, p_dataset_name;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- IAM: Permissions
-- =============================================================================

/**
 * Upsert a permission code.
 */
create or replace function ores.upsert_permission(
    p_code text,
    p_description text
) returns void as $$
declare
    v_id uuid;
begin
    perform ores.seed_validate_not_empty(p_code, 'Permission code');

    -- Check if permission already exists
    select id into v_id
    from ores.iam_permissions_tbl
    where code = p_code and valid_to = ores.utility_infinity_timestamp_fn();

    if v_id is null then
        -- The update_permissions trigger will set valid_from/valid_to
        insert into ores.iam_permissions_tbl (id, code, description, valid_from, valid_to)
        values (gen_random_uuid(), p_code, p_description,
                current_timestamp, ores.utility_infinity_timestamp_fn());
        raise notice 'Created permission: %', p_code;
    else
        raise notice 'Permission already exists: %', p_code;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- IAM: Roles
-- =============================================================================

/**
 * Upsert a role.
 */
create or replace function ores.upsert_role(
    p_name text,
    p_description text,
    p_recorded_by text default 'system'
) returns void as $$
begin
    perform ores.seed_validate_not_empty(p_name, 'Role name');

    -- Check if role already exists
    if not exists (
        select 1 from ores.iam_roles_tbl
        where name = p_name and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.iam_roles_tbl (id, version, name, description, modified_by,
            change_reason_code, change_commentary, valid_from, valid_to)
        values (gen_random_uuid(), 1, p_name, p_description, p_recorded_by,
                'system.new_record', 'System seed data',
                current_timestamp, ores.utility_infinity_timestamp_fn());
        raise notice 'Created role: %', p_name;
    else
        raise notice 'Role already exists: %', p_name;
    end if;
end;
$$ language plpgsql;

/**
 * Assign a permission to a role.
 */
create or replace function ores.assign_permission_to_role(
    p_role_name text,
    p_permission_code text,
    p_assigned_by text default 'system'
) returns void as $$
declare
    v_role_id uuid;
    v_permission_id uuid;
begin
    -- Get role ID (roles table uses 'id' column)
    select id into v_role_id
    from ores.iam_roles_tbl
    where name = p_role_name and valid_to = ores.utility_infinity_timestamp_fn();

    if v_role_id is null then
        raise exception 'Role not found: %', p_role_name;
    end if;

    -- Get permission ID (permissions table uses 'id' column)
    select id into v_permission_id
    from ores.iam_permissions_tbl
    where code = p_permission_code and valid_to = ores.utility_infinity_timestamp_fn();

    if v_permission_id is null then
        raise exception 'Permission not found: %', p_permission_code;
    end if;

    -- Check if assignment already exists
    if not exists (
        select 1 from ores.iam_role_permissions_tbl
        where role_id = v_role_id
          and permission_id = v_permission_id
          and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.iam_role_permissions_tbl (role_id, permission_id, valid_from, valid_to)
        values (v_role_id, v_permission_id, current_timestamp, ores.utility_infinity_timestamp_fn());
        raise notice 'Assigned permission % to role %', p_permission_code, p_role_name;
    else
        raise notice 'Permission % already assigned to role %', p_permission_code, p_role_name;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Variability: Feature Flags
-- =============================================================================

/**
 * Upsert a system feature flag.
 */
create or replace function ores.upsert_system_flag(
    p_name text,
    p_enabled boolean,
    p_description text
) returns void as $$
begin
    perform ores.seed_validate_not_empty(p_name, 'System flag name');

    -- Only insert if flag doesn't exist (preserve existing values)
    if not exists (
        select 1 from ores.variability_feature_flags_tbl
        where name = p_name and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        -- The variability_feature_flags_insert_trg trigger will set version and valid_from/valid_to
        -- Cast boolean to integer for the enabled column
        insert into ores.variability_feature_flags_tbl (name, enabled, description, modified_by,
            change_reason_code, change_commentary, valid_from, valid_to)
        values (p_name, p_enabled::int, p_description, 'system',
                'system.new_record', 'System seed data',
                current_timestamp, ores.utility_infinity_timestamp_fn());
        raise notice 'Created system flag: % (default: %)', p_name, p_enabled;
    else
        raise notice 'System flag already exists: %', p_name;
    end if;
end;
$$ language plpgsql;
