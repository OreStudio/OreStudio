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
 * Refdata Publish-from-DQ Functions
 *
 * SECURITY DEFINER functions called by the refdata service to bulk-install
 * template data from DQ artefact tables into refdata tables.
 *
 * Each function reads from ores_dq_<entity>_artefact_tbl (cross-service read,
 * runs at DDL-owner privilege) and writes only to ores_refdata_* tables
 * (intra-service write, normal grant model).
 *
 * Per-entity NATS subjects: refdata.v1.<entity>.publish-from-dq
 * Invoked by: ores.refdata.core handlers
 * Naming convention: ores_refdata_publish_<entity>_from_dq_fn
 */

-- =============================================================================
-- Countries
-- =============================================================================

create or replace function ores_refdata_publish_countries_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    v_coding_scheme_code text;
    r record;
    v_resolved_image_id uuid;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name, coding_scheme_code into v_dataset_name, v_coding_scheme_code
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_countries_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- The ZZ sentinel (ISO 3166-1's own reserved user-assigned code) is not
    -- part of any real ISO country dataset, so it never arrives via the DQ
    -- artefact loop below -- ensure it exists for the target tenant here,
    -- mirroring refdata_countries_populate.sql's system-tenant seed, so
    -- supranational calendars (e.g. TARGET) can reference it after
    -- provisioning a new tenant.
    insert into ores_refdata_countries_tbl (
        tenant_id, alpha2_code, version, alpha3_code, numeric_code, name, official_name,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        p_target_tenant_id, 'ZZ', 0, 'ZZZ', '999',
        'Supranational / Not Country-Specific', 'Supranational / Not Country-Specific',
        coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
        'ISO 3166-1 user-assigned sentinel for supranational calendars'
    )
    on conflict (tenant_id, alpha2_code)
    where valid_to = ores_utility_infinity_timestamp_fn()
    do nothing;

    for r in
        select
            dq.alpha2_code,
            dq.alpha3_code,
            dq.numeric_code,
            dq.name,
            dq.official_name,
            dq.image_id
        from ores_dq_countries_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_countries_tbl existing
            where existing.alpha2_code = r.alpha2_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        if r.image_id is not null then
            select assets.id into v_resolved_image_id
            from ores_dq_images_artefact_tbl dq_img
            join ores_assets_images_tbl assets on assets.code = dq_img.key
              and assets.tenant_id = p_target_tenant_id
            where dq_img.image_id = r.image_id
              and dq_img.tenant_id = ores_utility_system_tenant_id_fn()
              and assets.valid_to = ores_utility_infinity_timestamp_fn();

            if v_resolved_image_id is null then
                raise warning 'Image % not found in assets_images_tbl for country %. Populate images first.',
                    r.image_id, r.alpha2_code;
            end if;
        else
            v_resolved_image_id := null;
        end if;

        insert into ores_refdata_countries_tbl (
            tenant_id,
            alpha2_code, version, alpha3_code, numeric_code, name, official_name,
            coding_scheme_code, image_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.alpha2_code, 0, r.alpha3_code, r.numeric_code, r.name, r.official_name,
            v_coding_scheme_code, v_resolved_image_id,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Currencies
-- =============================================================================

create or replace function ores_refdata_publish_currencies_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    v_coding_scheme_code text;
    v_monetary_nature_filter text;
    r record;
    v_resolved_image_id uuid;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name, coding_scheme_code into v_dataset_name, v_coding_scheme_code
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    v_monetary_nature_filter := p_params ->> 'monetary_nature_filter';

    if p_mode = 'replace_all' then
        update ores_refdata_currencies_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.iso_code,
            dq.name,
            dq.numeric_code,
            dq.symbol,
            dq.fraction_symbol,
            dq.fractions_per_unit,
            dq.rounding_type,
            dq.rounding_precision,
            dq.format,
            dq.monetary_nature,
            dq.market_tier,
            dq.image_id,
            dq.spot_days,
            dq.day_basis,
            dq.base_precedence
        from ores_dq_currencies_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
          and (v_monetary_nature_filter is null or dq.monetary_nature = v_monetary_nature_filter)
    loop
        select exists (
            select 1 from ores_refdata_currencies_tbl existing
            where existing.iso_code = r.iso_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        if r.image_id is not null then
            select assets.id into v_resolved_image_id
            from ores_dq_images_artefact_tbl dq_img
            join ores_assets_images_tbl assets on assets.code = dq_img.key
              and assets.tenant_id = p_target_tenant_id
            where dq_img.image_id = r.image_id
              and dq_img.tenant_id = ores_utility_system_tenant_id_fn()
              and assets.valid_to = ores_utility_infinity_timestamp_fn();

            if v_resolved_image_id is null then
                raise warning 'Image % not found in assets_images_tbl for currency %. Populate images first.',
                    r.image_id, r.iso_code;
            end if;
        else
            v_resolved_image_id := null;
        end if;

        insert into ores_refdata_currencies_tbl (
            tenant_id,
            iso_code, version, name, numeric_code, symbol, fraction_symbol,
            fractions_per_unit, rounding_type, rounding_precision, format, monetary_nature, market_tier,
            image_id,
            spot_days, day_basis, base_precedence,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.iso_code, 0, r.name, r.numeric_code, r.symbol, r.fraction_symbol,
            r.fractions_per_unit, r.rounding_type, r.rounding_precision, r.format, r.monetary_nature, r.market_tier,
            v_resolved_image_id,
            r.spot_days, r.day_basis, r.base_precedence,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Currency Pairs
-- =============================================================================

create or replace function ores_refdata_publish_currency_pairs_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    v_classification_filter text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    v_classification_filter := p_params ->> 'classification_filter';

    if p_mode = 'replace_all' then
        update ores_refdata_currency_pairs_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.pair_code,
            dq.base_currency,
            dq.quote_currency,
            dq.classification
        from ores_dq_currency_pairs_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
          and (v_classification_filter is null or dq.classification = v_classification_filter)
    loop
        select exists (
            select 1 from ores_refdata_currency_pairs_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.pair_code = r.pair_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_currency_pairs_tbl (
            tenant_id,
            pair_code, version, base_currency, quote_currency,
            classification,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.pair_code, 0, r.base_currency, r.quote_currency,
            r.classification,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Currency Pair Conventions
-- =============================================================================

create or replace function ores_refdata_publish_currency_pair_conventions_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
    v_calendar_code text;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_currency_pair_conventions_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;

        update ores_refdata_currency_pair_convention_calendars_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn();
    end if;

    for r in
        select
            dq.pair_code,
            dq.pip_factor,
            dq.tick_size,
            dq.decimal_places,
            dq.advance_calendar,
            dq.business_day_convention,
            dq.spot_relative,
            dq.end_of_month
        from ores_dq_currency_pair_conventions_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_currency_pair_conventions_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.pair_code = r.pair_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_currency_pair_conventions_tbl (
            tenant_id,
            pair_code, version, pip_factor, tick_size, decimal_places,
            business_day_convention, spot_relative, end_of_month,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.pair_code, 0, r.pip_factor, r.tick_size, r.decimal_places,
            r.business_day_convention, r.spot_relative, r.end_of_month,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;

        -- advance_calendar comma-joins one or two calendar codes (see
        -- refdata_currency_pair_conventions_seed_populate.sql); each maps to
        -- its own row in the pair<->calendar junction table, not a column on
        -- the convention itself. Close out any calendars no longer present
        -- (the update-not-insert path) before inserting the current set.
        update ores_refdata_currency_pair_convention_calendars_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and pair_code = r.pair_code
          and valid_to = ores_utility_infinity_timestamp_fn()
          and calendar_code != all(
              coalesce(string_to_array(r.advance_calendar, ','), array[]::text[]));

        if r.advance_calendar is not null then
            foreach v_calendar_code in array string_to_array(r.advance_calendar, ',')
            loop
                if not exists (
                    select 1 from ores_refdata_currency_pair_convention_calendars_tbl existing
                    where existing.tenant_id = p_target_tenant_id
                      and existing.pair_code = r.pair_code
                      and existing.calendar_code = v_calendar_code
                      and existing.valid_to = ores_utility_infinity_timestamp_fn()
                ) then
                    insert into ores_refdata_currency_pair_convention_calendars_tbl (
                        tenant_id,
                        pair_code, calendar_code, version,
                        modified_by, performed_by, change_reason_code, change_commentary
                    ) values (
                        p_target_tenant_id,
                        r.pair_code, v_calendar_code, 0,
                        coalesce(ores_iam_current_service_fn(), current_user), current_user,
                        'system.external_data_import',
                        'Imported from DQ dataset: ' || v_dataset_name
                    );
                end if;
            end loop;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Account Types
-- =============================================================================

create or replace function ores_refdata_publish_account_types_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_account_types_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_account_types_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_account_types_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_account_types_tbl (
            tenant_id,
            code, version, coding_scheme_code, source, description,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.coding_scheme_code, r.source, r.description,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Asset Classes
-- =============================================================================

create or replace function ores_refdata_publish_asset_classes_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_asset_classes_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_asset_classes_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_asset_classes_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_asset_classes_tbl (
            tenant_id,
            code, version, coding_scheme_code, source, description,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.coding_scheme_code, r.source, r.description,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Asset Measures
-- =============================================================================

create or replace function ores_refdata_publish_asset_measures_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_asset_measures_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_asset_measures_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_asset_measures_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_asset_measures_tbl (
            tenant_id,
            code, version, coding_scheme_code, source, description,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.coding_scheme_code, r.source, r.description,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Benchmark Rates
-- =============================================================================

create or replace function ores_refdata_publish_benchmark_rates_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_benchmark_rates_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_benchmark_rates_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_benchmark_rates_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_benchmark_rates_tbl (
            tenant_id,
            code, version, coding_scheme_code, source, description,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.coding_scheme_code, r.source, r.description,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Business Centres
-- =============================================================================

create or replace function ores_refdata_publish_business_centres_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
    v_country_alpha2 text;
    v_city_name text;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_business_centres_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_business_centres_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_business_centres_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        v_country_alpha2 := left(r.code, 2);
        if not exists (
            select 1 from ores_refdata_countries_tbl
            where alpha2_code = v_country_alpha2
              and tenant_id = p_target_tenant_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            v_country_alpha2 := null;
        end if;

        if r.code in ('NYFD', 'NYSE') then
            v_country_alpha2 := 'US';
        end if;

        v_city_name := trim((regexp_match(r.description, '^([^,(]+)'))[1]);

        insert into ores_refdata_business_centres_tbl (
            tenant_id,
            code, version, coding_scheme_code, country_alpha2_code,
            source, description, city_name,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.coding_scheme_code, v_country_alpha2,
            r.source, r.description, v_city_name,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Business Processes
-- =============================================================================

create or replace function ores_refdata_publish_business_processes_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_business_processes_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_business_processes_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_business_processes_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_business_processes_tbl (
            tenant_id,
            code, version, coding_scheme_code, source, description,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.coding_scheme_code, r.source, r.description,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Cashflow Types
-- =============================================================================

create or replace function ores_refdata_publish_cashflow_types_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_cashflow_types_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_cashflow_types_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_cashflow_types_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_cashflow_types_tbl (
            tenant_id,
            code, version, coding_scheme_code, source, description,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.coding_scheme_code, r.source, r.description,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Entity Classifications
-- =============================================================================

create or replace function ores_refdata_publish_entity_classifications_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_entity_classifications_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_entity_classifications_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_entity_classifications_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_entity_classifications_tbl (
            tenant_id,
            code, version, coding_scheme_code, source, description,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.coding_scheme_code, r.source, r.description,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Local Jurisdictions
-- =============================================================================

create or replace function ores_refdata_publish_local_jurisdictions_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_local_jurisdictions_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_local_jurisdictions_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_local_jurisdictions_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_local_jurisdictions_tbl (
            tenant_id,
            code, version, coding_scheme_code, source, description,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.coding_scheme_code, r.source, r.description,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Party Relationships
-- =============================================================================

create or replace function ores_refdata_publish_party_relationships_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_party_relationships_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_party_relationships_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_party_relationships_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_party_relationships_tbl (
            tenant_id,
            code, version, coding_scheme_code, source, description,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.coding_scheme_code, r.source, r.description,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Party Roles
-- =============================================================================

create or replace function ores_refdata_publish_party_roles_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_party_roles_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_party_roles_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_party_roles_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_party_roles_tbl (
            tenant_id,
            code, version, coding_scheme_code, source, description,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.coding_scheme_code, r.source, r.description,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Person Roles
-- =============================================================================

create or replace function ores_refdata_publish_person_roles_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_person_roles_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_person_roles_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_person_roles_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_person_roles_tbl (
            tenant_id,
            code, version, coding_scheme_code, source, description,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.coding_scheme_code, r.source, r.description,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Regulatory Corporate Sectors
-- =============================================================================

create or replace function ores_refdata_publish_regulatory_corporate_sectors_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_regulatory_corporate_sectors_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_regulatory_corporate_sectors_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_regulatory_corporate_sectors_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_regulatory_corporate_sectors_tbl (
            tenant_id,
            code, version, coding_scheme_code, source, description,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.coding_scheme_code, r.source, r.description,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Reporting Regimes
-- =============================================================================

create or replace function ores_refdata_publish_reporting_regimes_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_reporting_regimes_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_reporting_regimes_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_reporting_regimes_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_reporting_regimes_tbl (
            tenant_id,
            code, version, coding_scheme_code, source, description,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.coding_scheme_code, r.source, r.description,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Supervisory Bodies
-- =============================================================================

create or replace function ores_refdata_publish_supervisory_bodies_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_supervisory_bodies_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_supervisory_bodies_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_supervisory_bodies_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_supervisory_bodies_tbl (
            tenant_id,
            code, version, coding_scheme_code, source, description,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.coding_scheme_code, r.source, r.description,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Counterparty Aliases
-- =============================================================================

/**
 * Publishes counterparty aliases from a DQ dataset into a tenant.
 *
 * An alias is a name a source system uses for a counterparty, such as the
 * CPTY_A an ORE document puts in its envelope. The staging row names the
 * counterparty by LEI, because a counterparty's id differs from tenant to
 * tenant: the alias is written as an identifier, under the row's scheme, of
 * the counterparty that holds that LEI in the target tenant. A name already
 * held, and a LEI the tenant holds no counterparty for, are skipped. A publish
 * only inserts: it never changes an alias the tenant already holds, whatever
 * the mode.
 */
create or replace function ores_refdata_publish_counterparty_aliases_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (action text, record_count bigint) as $$
declare
    v_dataset_name text;
    v_staged bigint;
    v_inserted bigint;
begin
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    select count(*) into v_staged
    from ores_dq_counterparty_aliases_artefact_tbl
    where dataset_id = p_dataset_id;

    insert into ores_refdata_counterparty_identifiers_tbl (
        tenant_id, id, version, counterparty_id, id_scheme, id_value, description,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select distinct on (a.id_scheme, a.id_value)
        p_target_tenant_id, gen_random_uuid(), 0, ci.counterparty_id, a.id_scheme,
        a.id_value, a.description,
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.external_data_import', 'Imported from DQ dataset: ' || v_dataset_name
    from ores_dq_counterparty_aliases_artefact_tbl a
    join ores_refdata_counterparty_identifiers_tbl ci
      on ci.tenant_id = p_target_tenant_id
     and ci.id_scheme = 'LEI'
     and ci.id_value = a.lei
     and ci.valid_to = ores_utility_infinity_timestamp_fn()
    where a.dataset_id = p_dataset_id
      and not exists (
        select 1 from ores_refdata_counterparty_identifiers_tbl o
        where o.tenant_id = p_target_tenant_id
          and o.id_scheme = a.id_scheme
          and o.id_value = a.id_value
          and o.valid_to = ores_utility_infinity_timestamp_fn())
    order by a.id_scheme, a.id_value, ci.counterparty_id;

    get diagnostics v_inserted = row_count;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'skipped'::text, v_staged - v_inserted
    where v_staged - v_inserted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Netting Agreements, Netting Sets, CSAs and Netting Set Aliases
-- =============================================================================

/**
 * The party a party-scoped publish writes for.
 *
 * The party named by the publish parameters, which must be an active party of
 * the target tenant, else the tenant's root party: the one with no parent that
 * is not the system party. Null when no party is named and the tenant holds no
 * root party, as while a tenant is provisioned before its party exists.
 */
create or replace function ores_refdata_publish_target_party_fn(
    p_target_tenant_id uuid,
    p_params jsonb
)
returns uuid as $$
declare
    v_named text := p_params ->> 'party_id';
    v_party_id uuid;
begin
    if v_named is null then
        select id into v_party_id
        from ores_refdata_parties_tbl
        where tenant_id = p_target_tenant_id
          and parent_party_id is null
          and party_category <> 'System'
          and valid_to = ores_utility_infinity_timestamp_fn()
        order by id
        limit 1;
        return v_party_id;
    end if;

    select id into v_party_id
    from ores_refdata_parties_tbl
    where tenant_id = p_target_tenant_id
      and id::text = v_named
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_party_id is null then
        raise exception 'Invalid party_id: %. No active party of tenant % has this id.',
            v_named, p_target_tenant_id
            using errcode = '23503';
    end if;
    return v_party_id;
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

/**
 * Publishes netting agreements from a DQ dataset to a party.
 *
 * Each staged agreement is written between the target party and the
 * counterparty that holds the staged LEI in the target tenant. An agreement
 * whose number the tenant already holds, and one whose LEI names no
 * counterparty, are skipped. A publish only inserts.
 */
create or replace function ores_refdata_publish_netting_agreements_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (action text, record_count bigint) as $$
declare
    v_dataset_name text;
    v_party_id uuid;
    v_staged bigint;
    v_inserted bigint;
begin
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    v_party_id := ores_refdata_publish_target_party_fn(p_target_tenant_id, p_params);
    if v_party_id is null then
        return query select 'skipped_no_party'::text, 0::bigint;
        return;
    end if;

    select count(*) into v_staged
    from ores_dq_netting_agreements_artefact_tbl
    where dataset_id = p_dataset_id;

    insert into ores_refdata_netting_agreements_tbl (
        tenant_id, id, version, agreement_number, party_id, counterparty_id,
        agreement_type, governing_law, description,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select distinct on (a.agreement_number)
        p_target_tenant_id, gen_random_uuid(), 0, a.agreement_number, v_party_id,
        ci.counterparty_id, a.agreement_type, a.governing_law, a.description,
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.external_data_import', 'Imported from DQ dataset: ' || v_dataset_name
    from ores_dq_netting_agreements_artefact_tbl a
    join ores_refdata_counterparty_identifiers_tbl ci
      on ci.tenant_id = p_target_tenant_id
     and ci.id_scheme = 'LEI'
     and ci.id_value = a.counterparty_lei
     and ci.valid_to = ores_utility_infinity_timestamp_fn()
    where a.dataset_id = p_dataset_id
      and not exists (
        select 1 from ores_refdata_netting_agreements_tbl o
        where o.tenant_id = p_target_tenant_id
          and o.agreement_number = a.agreement_number
          and o.valid_to = ores_utility_infinity_timestamp_fn())
    order by a.agreement_number, ci.counterparty_id;

    get diagnostics v_inserted = row_count;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'skipped'::text, v_staged - v_inserted
    where v_staged - v_inserted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

/**
 * Publishes netting sets from a DQ dataset to a party.
 *
 * A staged set is written for the target party. A set opened under an
 * agreement takes the agreement's counterparty; the agreement is found by
 * number among the party's agreements. A set with no agreement takes the
 * counterparty holding its staged LEI, if it names one. A set whose code the
 * tenant already holds, and one whose agreement or LEI does not resolve, are
 * skipped. A publish only inserts.
 *
 * An agreement number is unique within the tenant, but a set looks its
 * agreement up among the target party's only: a set must share its
 * agreement's party, so an agreement of another party leaves the set
 * unresolved rather than refused.
 */
create or replace function ores_refdata_publish_netting_sets_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (action text, record_count bigint) as $$
declare
    v_dataset_name text;
    v_party_id uuid;
    v_staged bigint;
    v_inserted bigint;
begin
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    v_party_id := ores_refdata_publish_target_party_fn(p_target_tenant_id, p_params);
    if v_party_id is null then
        return query select 'skipped_no_party'::text, 0::bigint;
        return;
    end if;

    select count(*) into v_staged
    from ores_dq_netting_sets_artefact_tbl
    where dataset_id = p_dataset_id;

    insert into ores_refdata_netting_sets_tbl (
        tenant_id, id, version, code, netting_agreement_id, counterparty_id, party_id,
        call_type, initial_margin_type, risk_weight, description,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select distinct on (s.code)
        p_target_tenant_id, gen_random_uuid(), 0, s.code, ag.id,
        coalesce(ag.counterparty_id, ci.counterparty_id), v_party_id,
        s.call_type, s.initial_margin_type, s.risk_weight, s.description,
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.external_data_import', 'Imported from DQ dataset: ' || v_dataset_name
    from ores_dq_netting_sets_artefact_tbl s
    left join ores_refdata_netting_agreements_tbl ag
      on ag.tenant_id = p_target_tenant_id
     and ag.party_id = v_party_id
     and ag.agreement_number = s.agreement_number
     and ag.valid_to = ores_utility_infinity_timestamp_fn()
    left join ores_refdata_counterparty_identifiers_tbl ci
      on ci.tenant_id = p_target_tenant_id
     and ci.id_scheme = 'LEI'
     and ci.id_value = s.counterparty_lei
     and ci.valid_to = ores_utility_infinity_timestamp_fn()
    where s.dataset_id = p_dataset_id
      and (s.agreement_number is null or ag.id is not null)
      and (s.counterparty_lei is null or ci.counterparty_id is not null)
      and not exists (
        select 1 from ores_refdata_netting_sets_tbl o
        where o.tenant_id = p_target_tenant_id
          and o.code = s.code
          and o.valid_to = ores_utility_infinity_timestamp_fn())
    order by s.code, ci.counterparty_id;

    get diagnostics v_inserted = row_count;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'skipped'::text, v_staged - v_inserted
    where v_staged - v_inserted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

/**
 * Publishes CSAs from a DQ dataset into a tenant.
 *
 * Each staged CSA is written for the netting set holding its code in the
 * target tenant, with its eligible currencies in the staged order. A set that
 * already holds a CSA, and a code that names no set, are skipped. A publish
 * only inserts.
 */
create or replace function ores_refdata_publish_csas_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (action text, record_count bigint) as $$
declare
    v_dataset_name text;
    v_modified_by text := coalesce(ores_iam_current_service_fn(), current_user);
    v_commentary text;
    v_staged bigint;
    v_inserted bigint;
begin
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;
    v_commentary := 'Imported from DQ dataset: ' || v_dataset_name;

    select count(*) into v_staged
    from ores_dq_csas_artefact_tbl
    where dataset_id = p_dataset_id;

    with staged as materialized (
        select gen_random_uuid() as csa_id, ns.id as netting_set_id, c.*
        from ores_dq_csas_artefact_tbl c
        join ores_refdata_netting_sets_tbl ns
          on ns.tenant_id = p_target_tenant_id
         and ns.code = c.netting_set_code
         and ns.valid_to = ores_utility_infinity_timestamp_fn()
        where c.dataset_id = p_dataset_id
          and not exists (
            select 1 from ores_refdata_csas_tbl o
            where o.tenant_id = p_target_tenant_id
              and o.netting_set_id = ns.id
              and o.valid_to = ores_utility_infinity_timestamp_fn())
    ),
    csas as (
        insert into ores_refdata_csas_tbl (
            tenant_id, id, version, netting_set_id, is_active, bilateral, csa_currency,
            index_name, threshold_pay, threshold_receive, minimum_transfer_amount_pay,
            minimum_transfer_amount_receive, independent_amount_held,
            independent_amount_type, call_frequency, post_frequency, margin_period_of_risk,
            collateral_compounding_spread_receive, collateral_compounding_spread_pay,
            apply_initial_margin, initial_margin_type, calculate_im_amount,
            calculate_vm_amount, non_exempt_im_regulations,
            modified_by, performed_by, change_reason_code, change_commentary
        )
        select p_target_tenant_id, s.csa_id, 0, s.netting_set_id, s.is_active, s.bilateral,
            s.csa_currency, s.index_name, s.threshold_pay, s.threshold_receive,
            s.minimum_transfer_amount_pay, s.minimum_transfer_amount_receive,
            s.independent_amount_held, s.independent_amount_type, s.call_frequency,
            s.post_frequency, s.margin_period_of_risk,
            s.collateral_compounding_spread_receive, s.collateral_compounding_spread_pay,
            s.apply_initial_margin, s.initial_margin_type, s.calculate_im_amount,
            s.calculate_vm_amount, s.non_exempt_im_regulations,
            v_modified_by, current_user, 'system.external_data_import', v_commentary
        from staged s
        returning id
    ),
    currencies as (
        insert into ores_refdata_csa_eligible_currencies_tbl (
            tenant_id, id, version, csa_id, currency_code, position,
            modified_by, performed_by, change_reason_code, change_commentary
        )
        select p_target_tenant_id, gen_random_uuid(), 0, s.csa_id, trim(e.currency_code),
            (e.ordinal - 1)::integer,
            v_modified_by, current_user, 'system.external_data_import', v_commentary
        from staged s
        join csas on csas.id = s.csa_id
        cross join lateral unnest(string_to_array(s.eligible_currencies, ','))
            with ordinality as e(currency_code, ordinal)
        where trim(e.currency_code) <> ''
        returning 1
    )
    select count(*) into v_inserted from csas;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'skipped'::text, v_staged - v_inserted
    where v_staged - v_inserted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

/**
 * Publishes netting set aliases from a DQ dataset into a tenant.
 *
 * Each alias is written, under its scheme, as an identifier of the netting set
 * holding the staged code in the target tenant. A name already held, and a
 * code that names no set, are skipped. A publish only inserts.
 */
create or replace function ores_refdata_publish_netting_set_aliases_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (action text, record_count bigint) as $$
declare
    v_dataset_name text;
    v_staged bigint;
    v_inserted bigint;
begin
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    select count(*) into v_staged
    from ores_dq_netting_set_aliases_artefact_tbl
    where dataset_id = p_dataset_id;

    insert into ores_refdata_netting_set_identifiers_tbl (
        tenant_id, id, version, netting_set_id, id_scheme, id_value, description,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select distinct on (a.id_scheme, a.id_value)
        p_target_tenant_id, gen_random_uuid(), 0, ns.id, a.id_scheme, a.id_value,
        a.description,
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.external_data_import', 'Imported from DQ dataset: ' || v_dataset_name
    from ores_dq_netting_set_aliases_artefact_tbl a
    join ores_refdata_netting_sets_tbl ns
      on ns.tenant_id = p_target_tenant_id
     and ns.code = a.netting_set_code
     and ns.valid_to = ores_utility_infinity_timestamp_fn()
    where a.dataset_id = p_dataset_id
      and not exists (
        select 1 from ores_refdata_netting_set_identifiers_tbl o
        where o.tenant_id = p_target_tenant_id
          and o.id_scheme = a.id_scheme
          and o.id_value = a.id_value
          and o.valid_to = ores_utility_infinity_timestamp_fn())
    order by a.id_scheme, a.id_value, ns.id;

    get diagnostics v_inserted = row_count;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'skipped'::text, v_staged - v_inserted
    where v_staged - v_inserted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Named Portfolios
-- =============================================================================

/**
 * Publishes portfolios by name from a DQ dataset to a party.
 *
 * Each staged portfolio is written as a top-level portfolio of the target
 * party under its staged name; the staged id and parent are not used, because
 * a portfolio a document names by its name needs no place in a tree. A name
 * the party already holds is skipped, so the publish adds names to a party
 * that already has portfolios, which the portfolio tree publish does not. A
 * publish only inserts.
 */
create or replace function ores_refdata_publish_named_portfolios_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (action text, record_count bigint) as $$
declare
    v_dataset_name text;
    v_party_id uuid;
    v_staged bigint;
    v_inserted bigint;
begin
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    v_party_id := ores_refdata_publish_target_party_fn(p_target_tenant_id, p_params);
    if v_party_id is null then
        return query select 'skipped_no_party'::text, 0::bigint;
        return;
    end if;

    select count(*) into v_staged
    from ores_dq_portfolios_artefact_tbl
    where dataset_id = p_dataset_id;

    insert into ores_refdata_portfolios_tbl (
        tenant_id, id, version, party_id, name, parent_portfolio_id, owner_unit_id,
        purpose_type, aggregation_ccy, is_virtual, status,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select distinct on (s.name)
        p_target_tenant_id, gen_random_uuid(), 0, v_party_id, s.name, null, null,
        s.purpose_type, s.aggregation_ccy, s.is_virtual, 'Active',
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.external_data_import', 'Imported from DQ dataset: ' || v_dataset_name
    from ores_dq_portfolios_artefact_tbl s
    where s.dataset_id = p_dataset_id
      and not exists (
        select 1 from ores_refdata_portfolios_tbl o
        where o.tenant_id = p_target_tenant_id
          and o.party_id = v_party_id
          and o.name = s.name
          and o.valid_to = ores_utility_infinity_timestamp_fn())
    order by s.name;

    get diagnostics v_inserted = row_count;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'skipped'::text, v_staged - v_inserted
    where v_staged - v_inserted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- LEI Counterparties
-- =============================================================================

create or replace function ores_refdata_publish_lei_counterparties_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted_counterparties bigint := 0;
    v_inserted_identifiers bigint := 0;
    v_inserted_bic_identifiers bigint := 0;
    v_dataset_name text;
    v_dataset_code text;
    v_entity_dataset_code text;
    v_relationship_dataset_code text;
    v_entity_dataset_id uuid;
    v_rel_dataset_id uuid;
    v_bic_dataset_id uuid;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name, code into v_dataset_name, v_dataset_code
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- See ores_refdata_publish_lei_parties_from_dq_fn for why the
    -- sibling entity/relationship dataset codes are derived from this
    -- dataset's own code rather than hardcoded to the 'gleif.' family.
    v_entity_dataset_code := coalesce(
        p_params ->> 'entity_dataset_code',
        replace(v_dataset_code, 'lei_counterparties', 'lei_entities'));
    v_relationship_dataset_code := coalesce(
        p_params ->> 'relationship_dataset_code',
        replace(v_dataset_code, 'lei_counterparties', 'lei_relationships'));

    select id into v_entity_dataset_id
    from ores_dq_datasets_tbl
    where code = v_entity_dataset_code
      and valid_to = ores_utility_infinity_timestamp_fn();

    select id into v_rel_dataset_id
    from ores_dq_datasets_tbl
    where code = v_relationship_dataset_code
      and valid_to = ores_utility_infinity_timestamp_fn();

    select id into v_bic_dataset_id
    from ores_dq_datasets_tbl
    where code = 'gleif.lei_bic'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_entity_dataset_id is null or v_rel_dataset_id is null then
        raise exception 'LEI dataset not found for: % / %', v_entity_dataset_code, v_relationship_dataset_code;
    end if;

    -- Explicit drop, not just ON COMMIT DROP: this function may be
    -- called more than once within a single enclosing transaction
    -- (e.g. a multi-party orchestrator), so the temp table from a
    -- prior call in the same transaction must not collide.
    drop table if exists lei_counterparty_uuid_map;
    create temp table lei_counterparty_uuid_map (
        lei text primary key,
        counterparty_uuid uuid not null default gen_random_uuid(),
        parent_lei text null,
        depth int not null default 0,
        entity_legal_name text not null,
        entity_legal_address_country text not null,
        entity_entity_status text not null,
        entity_transliterated_name_1 text null,
        short_code text not null default ''
    ) on commit drop;

    insert into lei_counterparty_uuid_map (lei, entity_legal_name, entity_legal_address_country, entity_entity_status, entity_transliterated_name_1)
    select distinct on (e.lei)
        e.lei,
        e.entity_legal_name,
        e.entity_legal_address_country,
        e.entity_entity_status,
        e.entity_transliterated_name_1
    from ores_dq_lei_entities_artefact_tbl e
    where e.dataset_id = v_entity_dataset_id
    order by e.lei;

    -- A tenant may already hold some of these counterparties, from this
    -- dataset or another: a second dataset adds the LEIs the tenant lacks
    -- and leaves the rest alone. A parent is linked whether this run writes
    -- it or the tenant already holds it.
    delete from lei_counterparty_uuid_map m
    using ores_refdata_counterparty_identifiers_tbl ci
    where ci.tenant_id = p_target_tenant_id
      and ci.id_scheme = 'LEI'
      and ci.id_value = m.lei
      and ci.valid_to = ores_utility_infinity_timestamp_fn();

    if not exists (select 1 from lei_counterparty_uuid_map) then
        raise notice 'Target tenant already has every LEI counterparty, skipping.';
        return query select 'skipped'::text, 0::bigint;
        return;
    end if;

    update lei_counterparty_uuid_map m
    set parent_lei = r.relationship_end_node_node_id
    from ores_dq_lei_relationships_artefact_tbl r
    where r.relationship_start_node_node_id = m.lei
      and r.relationship_relationship_type = 'IS_DIRECTLY_CONSOLIDATED_BY'
      and r.relationship_relationship_status = 'ACTIVE'
      and r.dataset_id = v_rel_dataset_id
      and (exists (select 1 from lei_counterparty_uuid_map p
                   where p.lei = r.relationship_end_node_node_id)
           or exists (select 1 from ores_refdata_counterparty_identifiers_tbl ci
                      where ci.tenant_id = p_target_tenant_id
                        and ci.id_scheme = 'LEI'
                        and ci.id_value = r.relationship_end_node_node_id
                        and ci.valid_to = ores_utility_infinity_timestamp_fn()));

    declare
        v_changed boolean := true;
    begin
        while v_changed loop
            update lei_counterparty_uuid_map child
            set depth = parent.depth + 1
            from lei_counterparty_uuid_map parent
            where child.parent_lei = parent.lei
              and child.depth <= parent.depth;
            v_changed := found;
        end loop;
    end;

    update lei_counterparty_uuid_map m
    set short_code = sub.resolved_code
    from (
        select lei,
            case when cnt > 1 then base_code || rn::text
                 else base_code end as resolved_code
        from (
            select lei,
                ores_utility_generate_short_code_fn(
                    entity_legal_name, entity_transliterated_name_1) as base_code,
                row_number() over (
                    partition by ores_utility_generate_short_code_fn(
                        entity_legal_name, entity_transliterated_name_1)
                    order by lei) as rn,
                count(*) over (
                    partition by ores_utility_generate_short_code_fn(
                        entity_legal_name, entity_transliterated_name_1)) as cnt
            from lei_counterparty_uuid_map
        ) numbered
    ) sub
    where sub.lei = m.lei;
    -- The business centre a registered country implies. The publisher states
    -- one centre per counterparty; a tenant that deals through more adds them
    -- through the junction.
    -- Explicit drop for the same reason as the uuid map above: a second call
    -- in the same transaction must not collide with the first call's table.
    drop table if exists lei_counterparty_centre_map;
    create temp table lei_counterparty_centre_map (
        country_code text not null,
        business_centre_code text not null
    ) on commit drop;

    insert into lei_counterparty_centre_map (country_code, business_centre_code) values
        ('AE', 'AEDU'), ('AT', 'ATVI'), ('AU', 'AUSY'), ('BE', 'BEBR'),
        ('BR', 'BRSP'), ('CA', 'CATO'), ('CH', 'CHZU'), ('CL', 'CLSA'),
        ('CN', 'CNBE'), ('CO', 'COBO'), ('CZ', 'CZPR'), ('DE', 'DEFR'),
        ('DK', 'DKCO'), ('ES', 'ESMA'), ('FI', 'FIHE'), ('FR', 'FRPA'),
        ('GB', 'GBLO'), ('GR', 'GRAT'), ('HK', 'HKHK'), ('HU', 'HUBU'),
        ('ID', 'IDJA'), ('IE', 'IEDU'), ('IL', 'ILTA'), ('IN', 'INMU'),
        ('IT', 'ITMI'), ('JP', 'JPTO'), ('KR', 'KRSE'), ('KY', 'KYGE'),
        ('LU', 'LULU'), ('MX', 'MXMC'), ('MY', 'MYKL'), ('NL', 'NLAM'),
        ('NO', 'NOOS'), ('NZ', 'NZAU'), ('PH', 'PHMA'), ('PL', 'PLWA'),
        ('PT', 'PTLI'), ('RO', 'ROBU'), ('RU', 'RUMO'), ('SA', 'SARI'),
        ('SE', 'SEST'), ('SG', 'SGSI'), ('TH', 'THBA'), ('TR', 'TRIS'),
        ('TW', 'TWTA'), ('US', 'USNY'), ('ZA', 'ZAJO');

    declare
        v_current_depth int := 0;
        v_max_depth int;
        v_level_count bigint;
    begin
        select max(depth) into v_max_depth from lei_counterparty_uuid_map;

        for v_current_depth in 0..coalesce(v_max_depth, 0) loop
            insert into ores_refdata_counterparties_tbl (
                tenant_id,
                id, version, full_name, short_code, transliterated_name, party_type,
                parent_counterparty_id, status,
                modified_by, performed_by, change_reason_code, change_commentary
            )
            select
                p_target_tenant_id,
                m.counterparty_uuid, 0,
                m.entity_legal_name,
                m.short_code, m.entity_transliterated_name_1, 'Corporate',
                coalesce(parent_map.counterparty_uuid, held_parent.counterparty_id),
                'Active',
                coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
                'Imported from GLEIF LEI dataset: ' || v_dataset_name
            from lei_counterparty_uuid_map m
            left join lei_counterparty_uuid_map parent_map on parent_map.lei = m.parent_lei
            left join ores_refdata_counterparty_identifiers_tbl held_parent
                on parent_map.lei is null
               and held_parent.tenant_id = p_target_tenant_id
               and held_parent.id_scheme = 'LEI'
               and held_parent.id_value = m.parent_lei
               and held_parent.valid_to = ores_utility_infinity_timestamp_fn()
            where m.depth = v_current_depth;

            -- The centre each counterparty deals through, from the country its
            -- registered address sits in. A counterparty may deal through
            -- several; this import states the one its address implies.
            insert into ores_refdata_counterparty_business_centres_tbl (
                tenant_id, counterparty_id, business_centre_code, version,
                modified_by, performed_by, change_reason_code, change_commentary
            )
            select
                p_target_tenant_id, m.counterparty_uuid,
                coalesce(centre_map.business_centre_code, 'WRLD'), 0,
                coalesce(ores_iam_current_service_fn(), current_user), current_user,
                'system.external_data_import',
                'Imported from GLEIF LEI dataset: ' || v_dataset_name
            from lei_counterparty_uuid_map m
            left join lei_counterparty_centre_map centre_map
                on centre_map.country_code = m.entity_legal_address_country
            where m.depth = v_current_depth;

            get diagnostics v_level_count = row_count;
            v_inserted_counterparties := v_inserted_counterparties + v_level_count;
        end loop;
    end;

    -- Attach a logo to any newly-inserted counterparty whose LEI is in
    -- the known-logo map below, as part of this same publish -- not a
    -- separate post-hoc step racing this import's own completion (see
    -- the "GLEIF import should natively attach counterparty logos"
    -- task). One entry today (Barclays Plc, the Acme Corporation demo's
    -- real trading counterparty); extend the VALUES list as further
    -- logos are sourced. Each tenant gets its own copy of the
    -- system-tenant template image (ores_assets_get_template_image_fn),
    -- created once and reused on subsequent publishes into the same
    -- tenant.
    declare
        v_logo_map record;
        v_template record;
        v_image_id uuid;
    begin
        for v_logo_map in
            select * from (values
                ('213800LBQA1Y9L22JB70', 'demo_counterparty_logo')
            ) as m(lei, image_key)
        loop
            if not exists (
                select 1 from lei_counterparty_uuid_map where lei = v_logo_map.lei
            ) then
                continue;
            end if;

            select id into v_image_id
            from ores_assets_images_tbl
            where tenant_id = p_target_tenant_id
              and code = v_logo_map.image_key
              and valid_to = ores_utility_infinity_timestamp_fn();

            if v_image_id is null then
                select * into v_template
                from ores_assets_get_template_image_fn(v_logo_map.image_key);

                if v_template is not null then
                    v_image_id := gen_random_uuid();
                    insert into ores_assets_images_tbl (
                        id, tenant_id, version, code, description, mime_type, data,
                        modified_by, performed_by, change_reason_code, change_commentary
                    ) values (
                        v_image_id, p_target_tenant_id, 0, v_logo_map.image_key,
                        v_template.description, v_template.mime_type, v_template.data,
                        coalesce(ores_iam_current_service_fn(), current_user), current_user,
                        'system.external_data_import',
                        'Copied from system-tenant template: ' || v_logo_map.image_key
                    );
                end if;
            end if;

            if v_image_id is not null then
                update ores_refdata_counterparties_tbl
                set image_id = v_image_id
                where tenant_id = p_target_tenant_id
                  and id = (select counterparty_uuid from lei_counterparty_uuid_map where lei = v_logo_map.lei)
                  and valid_to = ores_utility_infinity_timestamp_fn();
            end if;
        end loop;
    end;

    insert into ores_refdata_counterparty_identifiers_tbl (
        tenant_id,
        id, version, counterparty_id, id_scheme, id_value,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        p_target_tenant_id,
        gen_random_uuid(), 0, m.counterparty_uuid, 'LEI', m.lei,
        coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
        'Imported from GLEIF LEI dataset: ' || v_dataset_name
    from lei_counterparty_uuid_map m;

    get diagnostics v_inserted_identifiers = row_count;

    if v_bic_dataset_id is not null then
        insert into ores_refdata_counterparty_identifiers_tbl (
            tenant_id,
            id, version, counterparty_id, id_scheme, id_value,
            modified_by, performed_by, change_reason_code, change_commentary
        )
        select
            p_target_tenant_id,
            gen_random_uuid(), 0, m.counterparty_uuid, 'BIC', b.bic,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from GLEIF LEI-BIC dataset'
        from lei_counterparty_uuid_map m
        join ores_dq_lei_bic_artefact_tbl b
            on b.lei = m.lei
            and b.dataset_id = v_bic_dataset_id;

        get diagnostics v_inserted_bic_identifiers = row_count;
    end if;

    return query
    select 'inserted'::text, v_inserted_counterparties
    where v_inserted_counterparties > 0
    union all
    select 'inserted_identifiers'::text, v_inserted_identifiers
    where v_inserted_identifiers > 0
    union all
    select 'inserted_bic_identifiers'::text, v_inserted_bic_identifiers
    where v_inserted_bic_identifiers > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- LEI Parties
-- =============================================================================

create or replace function ores_refdata_publish_lei_parties_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_root_lei text;
    v_inserted_parties bigint := 0;
    v_inserted_identifiers bigint := 0;
    v_inserted_bic_identifiers bigint := 0;
    v_dataset_name text;
    v_dataset_code text;
    v_entity_dataset_code text;
    v_relationship_dataset_code text;
    v_entity_dataset_id uuid;
    v_rel_dataset_id uuid;
    v_bic_dataset_id uuid;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name, code into v_dataset_name, v_dataset_code
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Sibling entity/relationship dataset codes are derived from this
    -- dataset's own code by convention (e.g. 'gleif.lei_parties.small'
    -- -> 'gleif.lei_entities.small'/'gleif.lei_relationships.small';
    -- 'acme.lei_parties' -> 'acme.lei_entities'/'acme.lei_relationships')
    -- rather than hardcoded to the 'gleif.' family -- any explicit
    -- override always wins, so a family whose datasets don't follow the
    -- convention isn't blocked.
    v_entity_dataset_code := coalesce(
        p_params ->> 'entity_dataset_code',
        replace(v_dataset_code, 'lei_parties', 'lei_entities'));
    v_relationship_dataset_code := coalesce(
        p_params ->> 'relationship_dataset_code',
        replace(v_dataset_code, 'lei_parties', 'lei_relationships'));

    select id into v_entity_dataset_id
    from ores_dq_datasets_tbl
    where code = v_entity_dataset_code
      and valid_to = ores_utility_infinity_timestamp_fn();

    select id into v_rel_dataset_id
    from ores_dq_datasets_tbl
    where code = v_relationship_dataset_code
      and valid_to = ores_utility_infinity_timestamp_fn();

    select id into v_bic_dataset_id
    from ores_dq_datasets_tbl
    where code = 'gleif.lei_bic'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_entity_dataset_id is null or v_rel_dataset_id is null then
        raise exception 'LEI dataset not found for: % / %', v_entity_dataset_code, v_relationship_dataset_code;
    end if;

    v_root_lei := coalesce(
        p_params ->> 'root_lei',
        p_params -> 'lei_parties' ->> 'root_lei'
    );
    -- No root_lei: base bundle publish without tenant-specific params — skip gracefully.
    if v_root_lei is null or v_root_lei = '' then
        return query select 'skipped'::text, 0::bigint;
        return;
    end if;

    if not exists (
        select 1
        from ores_dq_lei_entities_artefact_tbl
        where lei = v_root_lei
          and dataset_id = v_entity_dataset_id
    ) then
        raise exception 'Root LEI not found in staging data: %', v_root_lei;
    end if;

    -- Root party already exists: another size variant already ran — skip gracefully.
    if exists (
        select 1
        from ores_refdata_parties_tbl p
        where p.tenant_id = p_target_tenant_id
          and p.parent_party_id is null
          and p.party_category <> 'System'
          and p.valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return query select 'skipped'::text, 0::bigint;
        return;
    end if;

    -- Explicit drop, not just ON COMMIT DROP: this function may be
    -- called more than once within a single enclosing transaction
    -- (e.g. a multi-party orchestrator), so the temp table from a
    -- prior call in the same transaction must not collide.
    drop table if exists lei_party_subtree;
    create temp table lei_party_subtree (
        lei text primary key,
        party_uuid uuid not null default gen_random_uuid(),
        parent_lei text null,
        depth int not null default 0,
        entity_legal_name text not null,
        entity_legal_address_country text not null,
        entity_entity_status text not null,
        entity_transliterated_name_1 text null,
        short_code text not null default ''
    ) on commit drop;

    with recursive subtree as (
        select e.lei, 0 as depth
        from ores_dq_lei_entities_artefact_tbl e
        where e.lei = v_root_lei
          and e.dataset_id = v_entity_dataset_id

        union

        select distinct r.relationship_start_node_node_id, s.depth + 1
        from ores_dq_lei_relationships_artefact_tbl r
        join subtree s on s.lei = r.relationship_end_node_node_id
        where r.relationship_relationship_type = 'IS_DIRECTLY_CONSOLIDATED_BY'
          and r.relationship_relationship_status = 'ACTIVE'
          and r.dataset_id = v_rel_dataset_id
    )
    insert into lei_party_subtree (lei, depth, entity_legal_name, entity_legal_address_country, entity_entity_status, entity_transliterated_name_1)
    select distinct on (s.lei)
        s.lei,
        s.depth,
        e.entity_legal_name,
        e.entity_legal_address_country,
        e.entity_entity_status,
        e.entity_transliterated_name_1
    from subtree s
    join ores_dq_lei_entities_artefact_tbl e on e.lei = s.lei
        and e.dataset_id = v_entity_dataset_id
    order by s.lei;

    update lei_party_subtree m
    set parent_lei = r.relationship_end_node_node_id
    from ores_dq_lei_relationships_artefact_tbl r
    where r.relationship_start_node_node_id = m.lei
      and r.relationship_relationship_type = 'IS_DIRECTLY_CONSOLIDATED_BY'
      and r.relationship_relationship_status = 'ACTIVE'
      and r.dataset_id = v_rel_dataset_id
      and m.lei <> v_root_lei;

    update lei_party_subtree m
    set short_code = sub.resolved_code
    from (
        select lei,
            case when cnt > 1 then base_code || rn::text
                 else base_code end as resolved_code
        from (
            select lei,
                ores_utility_generate_short_code_fn(
                    entity_legal_name, entity_transliterated_name_1) as base_code,
                row_number() over (
                    partition by ores_utility_generate_short_code_fn(
                        entity_legal_name, entity_transliterated_name_1)
                    order by lei) as rn,
                count(*) over (
                    partition by ores_utility_generate_short_code_fn(
                        entity_legal_name, entity_transliterated_name_1)) as cnt
            from lei_party_subtree
        ) numbered
    ) sub
    where sub.lei = m.lei;

    declare
        v_current_depth int := 0;
        v_max_depth int;
        v_level_count bigint;
    begin
        select max(depth) into v_max_depth from lei_party_subtree;

        for v_current_depth in 0..coalesce(v_max_depth, 0) loop
            insert into ores_refdata_parties_tbl (
                tenant_id,
                id, version, full_name, short_code, transliterated_name,
                party_category, party_type,
                parent_party_id, business_center_code, status,
                modified_by, performed_by, change_reason_code, change_commentary
            )
            select
                p_target_tenant_id,
                m.party_uuid, 0,
                m.entity_legal_name,
                m.short_code, m.entity_transliterated_name_1,
                'Operational', 'Corporate',
                parent_map.party_uuid,
                coalesce(bc_map.business_center_code, 'WRLD'),
                'Inactive',
                coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
                'Imported from GLEIF LEI dataset: ' || v_dataset_name
            from lei_party_subtree m
            left join lei_party_subtree parent_map on parent_map.lei = m.parent_lei
            left join (values
                ('AE', 'AEDU'), ('AT', 'ATVI'), ('AU', 'AUSY'), ('BE', 'BEBR'),
                ('BR', 'BRSP'), ('CA', 'CATO'), ('CH', 'CHZU'), ('CL', 'CLSA'),
                ('CN', 'CNBE'), ('CO', 'COBO'), ('CZ', 'CZPR'), ('DE', 'DEFR'),
                ('DK', 'DKCO'), ('ES', 'ESMA'), ('FI', 'FIHE'), ('FR', 'FRPA'),
                ('GB', 'GBLO'), ('GR', 'GRAT'), ('HK', 'HKHK'), ('HU', 'HUBU'),
                ('ID', 'IDJA'), ('IE', 'IEDU'), ('IL', 'ILTA'), ('IN', 'INMU'),
                ('IT', 'ITMI'), ('JP', 'JPTO'), ('KR', 'KRSE'), ('KY', 'KYGE'),
                ('LU', 'LULU'), ('MX', 'MXMC'), ('MY', 'MYKL'), ('NL', 'NLAM'),
                ('NO', 'NOOS'), ('NZ', 'NZAU'), ('PH', 'PHMA'), ('PL', 'PLWA'),
                ('PT', 'PTLI'), ('RO', 'ROBU'), ('RU', 'RUMO'), ('SA', 'SARI'),
                ('SE', 'SEST'), ('SG', 'SGSI'), ('TH', 'THBA'), ('TR', 'TRIS'),
                ('TW', 'TWTA'), ('US', 'USNY'), ('ZA', 'ZAJO')
            ) as bc_map(country_code, business_center_code)
                on bc_map.country_code = m.entity_legal_address_country
            where m.depth = v_current_depth;

            get diagnostics v_level_count = row_count;
            v_inserted_parties := v_inserted_parties + v_level_count;
        end loop;
    end;

    insert into ores_refdata_party_identifiers_tbl (
        tenant_id,
        id, version, party_id, id_scheme, id_value,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        p_target_tenant_id,
        gen_random_uuid(), 0, m.party_uuid, 'LEI', m.lei,
        coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
        'Imported from GLEIF LEI dataset: ' || v_dataset_name
    from lei_party_subtree m;

    get diagnostics v_inserted_identifiers = row_count;

    if v_bic_dataset_id is not null then
        insert into ores_refdata_party_identifiers_tbl (
            tenant_id,
            id, version, party_id, id_scheme, id_value,
            modified_by, performed_by, change_reason_code, change_commentary
        )
        select
            p_target_tenant_id,
            gen_random_uuid(), 0, m.party_uuid, 'BIC', b.bic,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from GLEIF LEI-BIC dataset'
        from lei_party_subtree m
        join ores_dq_lei_bic_artefact_tbl b
            on b.lei = m.lei
            and b.dataset_id = v_bic_dataset_id;

        get diagnostics v_inserted_bic_identifiers = row_count;
    end if;

    return query
    select 'inserted'::text, v_inserted_parties
    where v_inserted_parties > 0
    union all
    select 'inserted_identifiers'::text, v_inserted_identifiers
    where v_inserted_identifiers > 0
    union all
    select 'inserted_bic_identifiers'::text, v_inserted_bic_identifiers
    where v_inserted_bic_identifiers > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Business Units
-- =============================================================================

create or replace function ores_refdata_publish_business_units_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_root_party_id uuid;
    v_inserted bigint := 0;
    v_current_depth int;
    v_max_depth int;
    v_level_count bigint;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    v_root_party_id := coalesce(
        (p_params ->> 'party_id')::uuid,
        (select id from ores_refdata_parties_tbl
         where tenant_id = p_target_tenant_id
           and parent_party_id is null
           and party_category <> 'System'
           and valid_to = ores_utility_infinity_timestamp_fn()
         limit 1)
    );

    if v_root_party_id is null then
        return query select 'skipped_no_party'::text, 0::bigint;
        return;
    end if;

    if exists (
        select 1 from ores_refdata_business_units_tbl
        where tenant_id = p_target_tenant_id
          and party_id = v_root_party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return query select 'skipped'::text, 0::bigint;
        return;
    end if;

    if not exists (
        select 1 from ores_refdata_business_unit_types_tbl
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_refdata_business_unit_types_tbl (
            id, tenant_id, version, coding_scheme_code, code, name, level, description,
            modified_by, performed_by, change_reason_code, change_commentary
        )
        select gen_random_uuid(), p_target_tenant_id, 0, 'ORES-ORG',
               code, name, level, description,
               coalesce(ores_iam_current_service_fn(), current_user), current_user,
               'system.external_data_import', 'Provisioned with organisation dataset'
        from (values
            ('DIVISION',      'Division',      0, 'Top-level functional grouping within a party.'),
            ('BRANCH',        'Branch',        0, 'Top-level geographic grouping within a party.'),
            ('BUSINESS_AREA', 'Business Area', 1, 'Cohesive set of business activities within a division.'),
            ('DESK',          'Trading Desk',  2, 'Operational trading or risk desk; direct owner of books.'),
            ('COST_CENTRE',   'Cost Centre',   2, 'Finance/accounting unit, leaf of the hierarchy.')
        ) as t(code, name, level, description);
    end if;

    -- Explicit drop, not just ON COMMIT DROP: this function may be
    -- called more than once within a single enclosing transaction
    -- (e.g. a multi-party orchestrator), so the temp table from a
    -- prior call in the same transaction must not collide.
    drop table if exists bu_publish_map;
    create temp table bu_publish_map (
        artefact_id uuid primary key,
        new_id uuid not null default gen_random_uuid(),
        parent_artefact_id uuid,
        depth int not null default 0,
        unit_name text not null,
        unit_code text,
        business_centre_code text,
        unit_type_code text
    ) on commit drop;

    insert into bu_publish_map (
        artefact_id, parent_artefact_id, depth,
        unit_name, unit_code, business_centre_code, unit_type_code
    )
    select id, parent_business_unit_id, 0,
           unit_name, unit_code, business_centre_code, unit_type_code
    from ores_dq_business_units_artefact_tbl
    where dataset_id = p_dataset_id
      and parent_business_unit_id is null;

    v_current_depth := 0;
    loop
        insert into bu_publish_map (
            artefact_id, parent_artefact_id, depth,
            unit_name, unit_code, business_centre_code, unit_type_code
        )
        select a.id, a.parent_business_unit_id, v_current_depth + 1,
               a.unit_name, a.unit_code, a.business_centre_code, a.unit_type_code
        from ores_dq_business_units_artefact_tbl a
        join bu_publish_map m on m.artefact_id = a.parent_business_unit_id
        where a.dataset_id = p_dataset_id
          and m.depth = v_current_depth
          and not exists (select 1 from bu_publish_map x where x.artefact_id = a.id);

        if not found then exit; end if;
        v_current_depth := v_current_depth + 1;
    end loop;

    select max(depth) into v_max_depth from bu_publish_map;

    for v_current_depth in 0..coalesce(v_max_depth, 0) loop
        insert into ores_refdata_business_units_tbl (
            tenant_id, id, version, party_id, unit_name,
            parent_business_unit_id, unit_code, business_centre_code,
            unit_type_id,
            modified_by, performed_by, change_reason_code, change_commentary
        )
        select
            p_target_tenant_id,
            m.new_id, 0, v_root_party_id, m.unit_name,
            parent_m.new_id, m.unit_code, m.business_centre_code,
            but.id,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Published from organisation dataset'
        from bu_publish_map m
        left join bu_publish_map parent_m
            on parent_m.artefact_id = m.parent_artefact_id
        left join ores_refdata_business_unit_types_tbl but
            on but.tenant_id = p_target_tenant_id
           and but.code = m.unit_type_code
           and but.valid_to = ores_utility_infinity_timestamp_fn()
        where m.depth = v_current_depth;

        get diagnostics v_level_count = row_count;
        v_inserted := v_inserted + v_level_count;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Portfolios
-- =============================================================================

create or replace function ores_refdata_publish_portfolios_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_dataset_code text;
    v_bu_dataset_code text;
    v_root_party_id uuid;
    v_bu_dataset_id uuid;
    v_inserted bigint := 0;
    v_current_depth int;
    v_max_depth int;
    v_level_count bigint;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    v_root_party_id := coalesce(
        (p_params ->> 'party_id')::uuid,
        (select id from ores_refdata_parties_tbl
         where tenant_id = p_target_tenant_id
           and parent_party_id is null
           and party_category <> 'System'
           and valid_to = ores_utility_infinity_timestamp_fn()
         limit 1)
    );

    if v_root_party_id is null then
        return query select 'skipped_no_party'::text, 0::bigint;
        return;
    end if;

    if exists (
        select 1 from ores_refdata_portfolios_tbl
        where tenant_id = p_target_tenant_id
          and party_id = v_root_party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return query select 'skipped'::text, 0::bigint;
        return;
    end if;

    -- The sibling business_units dataset code is derived from this
    -- dataset's own code by convention (e.g. 'testdata.portfolios' ->
    -- 'testdata.business_units'; 'acme.acme_uk.portfolios' ->
    -- 'acme.acme_uk.business_units') rather than hardcoded to a single
    -- dataset -- any explicit override always wins.
    select code into v_dataset_code
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    v_bu_dataset_code := coalesce(
        p_params ->> 'business_units_dataset_code',
        replace(v_dataset_code, 'portfolios', 'business_units'));

    select id into v_bu_dataset_id
    from ores_dq_datasets_tbl
    where code = v_bu_dataset_code
      and valid_to = ores_utility_infinity_timestamp_fn();

    -- Explicit drop, not just ON COMMIT DROP: this function may be
    -- called more than once within a single enclosing transaction
    -- (e.g. a multi-party orchestrator), so the temp table from a
    -- prior call in the same transaction must not collide.
    drop table if exists bu_ref_map;
    create temp table bu_ref_map (
        artefact_id uuid primary key,
        published_id uuid not null
    ) on commit drop;

    if v_bu_dataset_id is not null then
        -- Scoped to the target party, not just (tenant, unit_name): two
        -- different parties can legitimately have identically-named
        -- business units (e.g. every legal entity in a holding group
        -- having its own "Global Markets" division), and an unscoped
        -- match would return multiple r rows for one artefact_id.
        insert into bu_ref_map (artefact_id, published_id)
        select a.id, r.id
        from ores_dq_business_units_artefact_tbl a
        join ores_refdata_business_units_tbl r
            on r.unit_name = a.unit_name
            and r.tenant_id = p_target_tenant_id
            and r.party_id = v_root_party_id
            and r.valid_to = ores_utility_infinity_timestamp_fn()
        where a.dataset_id = v_bu_dataset_id;
    end if;

    -- Explicit drop, not just ON COMMIT DROP: this function may be
    -- called more than once within a single enclosing transaction
    -- (e.g. a multi-party orchestrator), so the temp table from a
    -- prior call in the same transaction must not collide.
    drop table if exists portfolio_publish_map;
    create temp table portfolio_publish_map (
        artefact_id uuid primary key,
        new_id uuid not null default gen_random_uuid(),
        parent_artefact_id uuid,
        owner_unit_artefact_id uuid,
        depth int not null default 0,
        name text not null,
        purpose_type text,
        aggregation_ccy text,
        is_virtual boolean
    ) on commit drop;

    insert into portfolio_publish_map (
        artefact_id, parent_artefact_id, owner_unit_artefact_id, depth,
        name, purpose_type, aggregation_ccy, is_virtual
    )
    select id, parent_portfolio_id, owner_unit_id, 0,
           name, purpose_type, aggregation_ccy, is_virtual
    from ores_dq_portfolios_artefact_tbl
    where dataset_id = p_dataset_id
      and parent_portfolio_id is null;

    v_current_depth := 0;
    loop
        insert into portfolio_publish_map (
            artefact_id, parent_artefact_id, owner_unit_artefact_id, depth,
            name, purpose_type, aggregation_ccy, is_virtual
        )
        select a.id, a.parent_portfolio_id, a.owner_unit_id, v_current_depth + 1,
               a.name, a.purpose_type, a.aggregation_ccy, a.is_virtual
        from ores_dq_portfolios_artefact_tbl a
        join portfolio_publish_map m on m.artefact_id = a.parent_portfolio_id
        where a.dataset_id = p_dataset_id
          and m.depth = v_current_depth
          and not exists (select 1 from portfolio_publish_map x where x.artefact_id = a.id);

        if not found then exit; end if;
        v_current_depth := v_current_depth + 1;
    end loop;

    select max(depth) into v_max_depth from portfolio_publish_map;

    for v_current_depth in 0..coalesce(v_max_depth, 0) loop
        insert into ores_refdata_portfolios_tbl (
            tenant_id, id, version, party_id, name, parent_portfolio_id,
            owner_unit_id, purpose_type, aggregation_ccy, is_virtual,
            status, modified_by, performed_by, change_reason_code, change_commentary
        )
        select
            p_target_tenant_id,
            m.new_id, 0, v_root_party_id, m.name,
            parent_m.new_id,
            bu_map.published_id,
            m.purpose_type, m.aggregation_ccy, m.is_virtual,
            'Active',
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Published from organisation dataset'
        from portfolio_publish_map m
        left join portfolio_publish_map parent_m
            on parent_m.artefact_id = m.parent_artefact_id
        left join bu_ref_map bu_map
            on bu_map.artefact_id = m.owner_unit_artefact_id
        where m.depth = v_current_depth;

        get diagnostics v_level_count = row_count;
        v_inserted := v_inserted + v_level_count;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Books
-- =============================================================================

create or replace function ores_refdata_publish_books_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_dataset_code text;
    v_portfolio_dataset_code text;
    v_root_party_id uuid;
    v_portfolio_dataset_id uuid;
    v_inserted bigint := 0;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    v_root_party_id := coalesce(
        (p_params ->> 'party_id')::uuid,
        (select id from ores_refdata_parties_tbl
         where tenant_id = p_target_tenant_id
           and parent_party_id is null
           and party_category <> 'System'
           and valid_to = ores_utility_infinity_timestamp_fn()
         limit 1)
    );

    if v_root_party_id is null then
        return query select 'skipped_no_party'::text, 0::bigint;
        return;
    end if;

    if exists (
        select 1 from ores_refdata_books_tbl
        where tenant_id = p_target_tenant_id
          and party_id = v_root_party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return query select 'skipped'::text, 0::bigint;
        return;
    end if;

    -- See ores_refdata_publish_portfolios_from_dq_fn for why the
    -- sibling portfolios dataset code is derived from this dataset's
    -- own code rather than hardcoded to a single dataset.
    select code into v_dataset_code
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    v_portfolio_dataset_code := coalesce(
        p_params ->> 'portfolios_dataset_code',
        replace(v_dataset_code, 'books', 'portfolios'));

    select id into v_portfolio_dataset_id
    from ores_dq_datasets_tbl
    where code = v_portfolio_dataset_code
      and valid_to = ores_utility_infinity_timestamp_fn();

    -- Explicit drop, not just ON COMMIT DROP: this function may be
    -- called more than once within a single enclosing transaction
    -- (e.g. a multi-party orchestrator), so the temp table from a
    -- prior call in the same transaction must not collide.
    drop table if exists portfolio_ref_map;
    create temp table portfolio_ref_map (
        artefact_id uuid primary key,
        published_id uuid not null,
        published_owner_unit_id uuid
    ) on commit drop;

    if v_portfolio_dataset_id is not null then
        -- Scoped to the target party -- see the matching comment in
        -- ores_refdata_publish_portfolios_from_dq_fn's bu_ref_map join.
        insert into portfolio_ref_map (artefact_id, published_id, published_owner_unit_id)
        select a.id, r.id, r.owner_unit_id
        from ores_dq_portfolios_artefact_tbl a
        join ores_refdata_portfolios_tbl r
            on r.name = a.name
            and r.tenant_id = p_target_tenant_id
            and r.party_id = v_root_party_id
            and r.valid_to = ores_utility_infinity_timestamp_fn()
        where a.dataset_id = v_portfolio_dataset_id;
    end if;

    insert into ores_refdata_books_tbl (
        tenant_id, id, version, party_id, name,
        parent_portfolio_id, functional_currency, gl_account_ref, cost_center,
        book_status, regulatory_book_type, book_purpose_type, ledger_feed_type,
        is_sweepable, rates_centre_code, owner_unit_id,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        p_target_tenant_id,
        gen_random_uuid(), 0, v_root_party_id, a.name,
        pmap.published_id, a.functional_currency, a.gl_account_ref, a.cost_center,
        a.book_status, a.regulatory_book_type, a.book_purpose_type, a.ledger_feed_type,
        a.is_sweepable,
        -- The artefact template can't know which party it will be
        -- published to, so a region-agnostic book (e.g. a group-level
        -- regulatory capital book) leaves rates_centre_code null in the
        -- artefact, meaning "inherit the publishing party's own
        -- location" rather than a hardcoded region. Desk-level books
        -- keep their real, party-independent region code in the
        -- artefact (a GBP rates desk trades out of London regardless of
        -- which legal entity owns the book).
        coalesce(a.rates_centre_code, v_root_party.business_center_code),
        pmap.published_owner_unit_id,
        coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
        'Published from organisation dataset'
    from ores_dq_books_artefact_tbl a
    join portfolio_ref_map pmap on pmap.artefact_id = a.parent_portfolio_id
    cross join ores_refdata_parties_tbl v_root_party
    where a.dataset_id = p_dataset_id
      and v_root_party.id = v_root_party_id
      and v_root_party.tenant_id = p_target_tenant_id
      and v_root_party.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_inserted = row_count;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- CRM Topology Bundles: refdata.v1.ops.publish_crm_topology_bundles_from_dq
-- =============================================================================

/**
 * CRM Topology Bundles Publish-from-DQ Function
 *
 * SECURITY DEFINER function called by the refdata service's NATS
 * handler for the refdata.v1.ops.publish_crm_topology_bundles_from_dq
 * subject. Reads ores_dq_crm_topology_bundles_artefact_tbl (system
 * tenant) and writes crm_topology_config/crm_driver_pair/
 * crm_enabled_derived_pair rows for the target (tenant, party) --
 * one config per distinct crm_name in the artefact, its driver
 * pairs/enabled derived pairs from that crm_name's rows.
 *
 * Idempotent by natural key at every level (config: tenant/party/name;
 * pair: tenant/party/config_id/base/quote) -- re-publishing never
 * duplicates a config or a pair that already exists; new pairs added to
 * the artefact for an already-published crm_name are picked up on
 * re-publish, matching insert_only semantics (this function has no
 * upsert/replace_all modes: CRM topology is provisioning seed data, not
 * a live feed, so there is nothing to overwrite once a party has its
 * own configs).
 */

create or replace function ores_refdata_publish_crm_topology_bundles_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'insert_only',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_target_party_id uuid;
    v_dataset_name text;
    v_inserted bigint := 0;
    v_skipped bigint := 0;
    r_crm record;
    r_pair record;
    v_config_id uuid;
    v_actor text;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    v_target_party_id := (p_params ->> 'party_id')::uuid;
    if v_target_party_id is null then
        raise exception 'p_params.party_id is required to publish CRM topology bundles';
    end if;

    v_actor := coalesce(ores_iam_current_service_fn(), current_user);

    for r_crm in
        select distinct crm_name, pivot_currency_code
        from ores_dq_crm_topology_bundles_artefact_tbl
        where dataset_id = p_dataset_id
          and tenant_id = ores_utility_system_tenant_id_fn()
        order by crm_name
    loop
        select id into v_config_id
        from ores_refdata_crm_topology_configs_tbl
        where tenant_id = p_target_tenant_id
          and party_id = v_target_party_id
          and name = r_crm.crm_name
          and valid_to = ores_utility_infinity_timestamp_fn();

        if v_config_id is null then
            v_config_id := gen_random_uuid();
            insert into ores_refdata_crm_topology_configs_tbl (
                id, tenant_id, version, party_id, name, pivot_currency_code, enabled,
                modified_by, performed_by, change_reason_code, change_commentary,
                valid_from, valid_to
            ) values (
                v_config_id, p_target_tenant_id, 0, v_target_party_id,
                r_crm.crm_name, r_crm.pivot_currency_code, true,
                v_actor, current_user,
                'system.external_data_import', 'Published from DQ dataset: ' || v_dataset_name,
                current_timestamp, ores_utility_infinity_timestamp_fn()
            );
            v_inserted := v_inserted + 1;
        else
            v_skipped := v_skipped + 1;
        end if;

        for r_pair in
            select base_currency_code, quote_currency_code, row_kind
            from ores_dq_crm_topology_bundles_artefact_tbl
            where dataset_id = p_dataset_id
              and tenant_id = ores_utility_system_tenant_id_fn()
              and crm_name = r_crm.crm_name
            order by row_kind, base_currency_code, quote_currency_code
        loop
            if r_pair.row_kind = 'driver' then
                if not exists (
                    select 1 from ores_refdata_crm_driver_pairs_tbl
                    where tenant_id = p_target_tenant_id
                      and party_id = v_target_party_id
                      and config_id = v_config_id
                      and base_currency_code = r_pair.base_currency_code
                      and quote_currency_code = r_pair.quote_currency_code
                      and valid_to = ores_utility_infinity_timestamp_fn()
                ) then
                    insert into ores_refdata_crm_driver_pairs_tbl (
                        id, tenant_id, version, party_id, config_id,
                        base_currency_code, quote_currency_code, enabled,
                        modified_by, performed_by, change_reason_code, change_commentary,
                        valid_from, valid_to
                    ) values (
                        gen_random_uuid(), p_target_tenant_id, 0, v_target_party_id, v_config_id,
                        r_pair.base_currency_code, r_pair.quote_currency_code, true,
                        v_actor, current_user,
                        'system.external_data_import', 'Published from DQ dataset: ' || v_dataset_name,
                        current_timestamp, ores_utility_infinity_timestamp_fn()
                    );
                    v_inserted := v_inserted + 1;
                else
                    v_skipped := v_skipped + 1;
                end if;
            elsif r_pair.row_kind = 'derived' then
                if not exists (
                    select 1 from ores_refdata_crm_enabled_derived_pairs_tbl
                    where tenant_id = p_target_tenant_id
                      and party_id = v_target_party_id
                      and config_id = v_config_id
                      and base_currency_code = r_pair.base_currency_code
                      and quote_currency_code = r_pair.quote_currency_code
                      and valid_to = ores_utility_infinity_timestamp_fn()
                ) then
                    insert into ores_refdata_crm_enabled_derived_pairs_tbl (
                        id, tenant_id, version, party_id, config_id,
                        base_currency_code, quote_currency_code, enabled,
                        modified_by, performed_by, change_reason_code, change_commentary,
                        valid_from, valid_to
                    ) values (
                        gen_random_uuid(), p_target_tenant_id, 0, v_target_party_id, v_config_id,
                        r_pair.base_currency_code, r_pair.quote_currency_code, true,
                        v_actor, current_user,
                        'system.external_data_import', 'Published from DQ dataset: ' || v_dataset_name,
                        current_timestamp, ores_utility_infinity_timestamp_fn()
                    );
                    v_inserted := v_inserted + 1;
                else
                    v_skipped := v_skipped + 1;
                end if;
            else
                raise exception 'Unclassified row_kind: % - extend this function', r_pair.row_kind;
            end if;
        end loop;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Calendar Types
-- =============================================================================

create or replace function ores_refdata_publish_calendar_types_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_calendar_types_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.name,
            dq.description,
            dq.display_order
        from ores_dq_calendar_types_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_calendar_types_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.code = r.code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_calendar_types_tbl (
            tenant_id,
            code, version, name, description, display_order,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.name, r.description, r.display_order,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Currency Groups
-- =============================================================================

create or replace function ores_refdata_publish_currency_groups_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_currency_groups_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.name,
            dq.description,
            dq.display_order
        from ores_dq_currency_groups_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_currency_groups_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.code = r.code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_currency_groups_tbl (
            tenant_id,
            code, version, name, description, display_order,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.name, r.description, r.display_order,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Payment Frequencies
-- =============================================================================

create or replace function ores_refdata_publish_payment_frequencies_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_payment_frequencies_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.name,
            dq.description,
            dq.period_unit,
            dq.period_multiplier,
            dq.display_order
        from ores_dq_payment_frequencies_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_payment_frequencies_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.code = r.code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_payment_frequencies_tbl (
            tenant_id,
            code, version, name, description, period_unit, period_multiplier, display_order,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.name, r.description, r.period_unit, r.period_multiplier, r.display_order,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Calendars
-- =============================================================================

create or replace function ores_refdata_publish_calendars_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_calendars_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.name,
            dq.calendar_type,
            dq.country_code,
            dq.image_id
        from ores_dq_calendars_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_calendars_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.code = r.code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_calendars_tbl (
            tenant_id,
            code, version, name, calendar_type, country_code, image_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.name, r.calendar_type, r.country_code, r.image_id,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Calendar events
-- =============================================================================

-- A calendar event's natural key is (calendar_code, event_date,
-- diary_entry_type). Its id belongs to the tenant: an event the tenant
-- already has keeps its id, and a new one gets a fresh id, so the
-- artefact's own id never reaches a tenant. replace_all closes only the
-- events the dataset does not carry, after the upsert, so it keeps the
-- ids of the events it carries.
create or replace function ores_refdata_publish_calendar_events_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_existing_id uuid;
    v_new_version integer;
begin
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    for r in
        select
            dq.calendar_code,
            dq.event_date,
            dq.diary_entry_type,
            dq.name,
            dq.description,
            dq.source
        from ores_dq_calendar_events_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select existing.id into v_existing_id
        from ores_refdata_calendar_events_tbl existing
        where existing.tenant_id = p_target_tenant_id
          and existing.calendar_code = r.calendar_code
          and existing.event_date = r.event_date
          and existing.diary_entry_type = r.diary_entry_type
          and existing.valid_to = ores_utility_infinity_timestamp_fn();

        if p_mode = 'insert_only' and v_existing_id is not null then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_calendar_events_tbl (
            id, tenant_id, version,
            calendar_code, event_date, diary_entry_type, name, description, source,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            coalesce(v_existing_id, gen_random_uuid()), p_target_tenant_id, 0,
            r.calendar_code, r.event_date, r.diary_entry_type, r.name, r.description, r.source,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    if p_mode = 'replace_all' then
        update ores_refdata_calendar_events_tbl e
        set valid_to = clock_timestamp()
        where e.tenant_id = p_target_tenant_id
          and e.valid_to = ores_utility_infinity_timestamp_fn()
          and not exists (
              select 1 from ores_dq_calendar_events_artefact_tbl dq
              where dq.dataset_id = p_dataset_id
                and dq.tenant_id = ores_utility_system_tenant_id_fn()
                and dq.calendar_code = e.calendar_code
                and dq.event_date = e.event_date
                and dq.diary_entry_type = e.diary_entry_type
          );

        get diagnostics v_deleted = row_count;
    end if;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Tenor schedules
-- =============================================================================

-- Provisioning copies the tenor convention resolutions before any schedule
-- exists in the tenant, so it leaves their schedule_code and
-- schedule_step_count empty. Once the schedules are published, this
-- function sets those columns from the system tenant's rows, for every
-- resolution whose schedule the tenant now has. It does so in every mode,
-- insert_only included: the columns are derived from the schedules, and a
-- tenant has no other way to get them. replace_all closes only the
-- schedules the dataset does not carry, after the upsert, so no schedule a
-- resolution names is closed while the call runs.
create or replace function ores_refdata_publish_tenor_schedules_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_resolutions bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    for r in
        select
            dq.code,
            dq.name,
            dq.description,
            dq.display_order,
            dq.schedule_source,
            dq.calendar_code,
            dq.diary_entry_type
        from ores_dq_tenor_schedules_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_tenor_schedules_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.code = r.code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_tenor_schedules_tbl (
            tenant_id,
            code, version, name, description, display_order,
            schedule_source, calendar_code, diary_entry_type,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0, r.name, r.description, r.display_order,
            r.schedule_source, r.calendar_code, r.diary_entry_type,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    if p_mode = 'replace_all' then
        update ores_refdata_tenor_schedules_tbl t
        set valid_to = clock_timestamp()
        where t.tenant_id = p_target_tenant_id
          and t.valid_to = ores_utility_infinity_timestamp_fn()
          and not exists (
              select 1 from ores_dq_tenor_schedules_artefact_tbl dq
              where dq.dataset_id = p_dataset_id
                and dq.tenant_id = ores_utility_system_tenant_id_fn()
                and dq.code = t.code
          );

        get diagnostics v_deleted = row_count;
    end if;

    insert into ores_refdata_tenor_convention_resolutions_tbl (
        convention_code, tenant_id, tenor_code, version,
        anchor_override, offset_unit, offset_multiplier,
        schedule_code, schedule_step_count,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        t.convention_code, t.tenant_id, t.tenor_code, 0,
        t.anchor_override, t.offset_unit, t.offset_multiplier,
        s.schedule_code, s.schedule_step_count,
        coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
        'Schedule set from DQ dataset: ' || v_dataset_name
    from ores_refdata_tenor_convention_resolutions_tbl t
    join ores_refdata_tenor_convention_resolutions_tbl s
      on s.tenant_id = ores_utility_system_tenant_id_fn()
     and s.convention_code = t.convention_code
     and s.tenor_code = t.tenor_code
     and s.valid_to = ores_utility_infinity_timestamp_fn()
    where t.tenant_id = p_target_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn()
      and s.schedule_code is not null
      and (t.schedule_code is distinct from s.schedule_code
           or t.schedule_step_count is distinct from s.schedule_step_count)
      and exists (
          select 1 from ores_refdata_tenor_schedules_tbl ts
          where ts.tenant_id = p_target_tenant_id
            and ts.code = s.schedule_code
            and ts.valid_to = ores_utility_infinity_timestamp_fn()
      );

    get diagnostics v_resolutions = row_count;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated + v_resolutions
    where v_updated + v_resolutions > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace function ores_refdata_publish_currency_calendars_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_currency_calendars_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.currency_iso_code,
            dq.calendar_code
        from ores_dq_currency_calendars_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_currency_calendars_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.currency_iso_code = r.currency_iso_code
              and existing.calendar_code = r.calendar_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_currency_calendars_tbl (
            tenant_id,
            currency_iso_code, version, calendar_code,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.currency_iso_code, 0, r.calendar_code,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace function ores_refdata_publish_currency_countries_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_currency_countries_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.currency_iso_code,
            dq.country_alpha2_code
        from ores_dq_currency_countries_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_currency_countries_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.currency_iso_code = r.currency_iso_code
              and existing.country_alpha2_code = r.country_alpha2_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_currency_countries_tbl (
            tenant_id,
            currency_iso_code, version, country_alpha2_code,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.currency_iso_code, 0, r.country_alpha2_code,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace function ores_refdata_publish_currency_pair_convention_calendars_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_exists boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_refdata_currency_pair_convention_calendars_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.pair_code,
            dq.calendar_code
        from ores_dq_currency_pair_convention_calendars_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_refdata_currency_pair_convention_calendars_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.pair_code = r.pair_code
              and existing.calendar_code = r.calendar_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_refdata_currency_pair_convention_calendars_tbl (
            tenant_id,
            pair_code, version, calendar_code,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.pair_code, 0, r.calendar_code,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Instrument Conventions: refdata.v1.ops.publish_conventions_from_dq
-- =============================================================================

-- One dataset owns every convention kind: one artefact table per live
-- convention table, 24 of them. There is no per-kind publish body. The
-- function walks the kind list and dispatches by name transform,
-- ores_dq_<x>_conventions_artefact_tbl to ores_refdata_<x>_conventions_tbl,
-- so a new kind costs one array entry and one artefact table, not a function.
-- The copied column list is read from the live table itself, so there is no
-- second per-kind list to keep in step either.
--
-- Rows the target party already carries are skipped, so a republish converges
-- on the same set. Ids the engine does not ask for are inert: the engine
-- resolves a convention by id.
--
-- The oresmd_uri column is copied through as staged. It is null in the seed:
-- no offline tool turns an ORE index name into an oresmd URI today. See the
-- README under tools/ore_conventions/.
create or replace function ores_refdata_publish_conventions_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (action text, record_count bigint) as $$
declare
    v_dataset_name text;
    v_party_id uuid;
    v_kind text;
    v_artefact text;
    v_target text;
    v_has_party boolean;
    v_cols text;
    v_party_target text;
    v_party_expr text;
    v_party_cond text;
    v_commentary text;
    v_staged bigint;
    v_inserted bigint;
    v_total_inserted bigint := 0;
    v_total_staged bigint := 0;
    v_kinds text[] := array[
        'average_ois', 'bma_basis_swap', 'bond_yield', 'cds',
        'cms_spread_option', 'commodity_forward', 'commodity_future',
        'cross_currency_basis', 'cross_currency_fix_float', 'deposit', 'fra',
        'future', 'fx_option', 'ibor_index', 'inflation_swap',
        'intraday_power_load', 'ois', 'overnight_index', 'swap', 'swap_index',
        'tenor_basis_swap', 'tenor_basis_two_swap', 'zero',
        'zero_inflation_index'];
begin
    perform ores_utility_allow_version_replace_fn();

    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    v_party_id := ores_refdata_publish_target_party_fn(p_target_tenant_id, p_params);
    if v_party_id is null then
        return query select 'skipped_no_party'::text, 0::bigint;
        return;
    end if;

    v_commentary := 'Imported from DQ dataset: ' || v_dataset_name;

    foreach v_kind in array v_kinds loop
        v_artefact := format('ores_dq_%s_conventions_artefact_tbl', v_kind);
        v_target := format('ores_refdata_%s_conventions_tbl', v_kind);

        select exists (
            select 1 from information_schema.columns c
            where c.table_schema = 'public'
              and c.table_name = v_target
              and c.column_name = 'party_id') into v_has_party;

        -- The live table's own data columns, in table order, less the columns
        -- this function stamps. A nullable column, such as oresmd_uri, is
        -- copied as a value rather than dropped.
        select string_agg(format('%I', c.column_name), ', ' order by c.ordinal_position)
               || ','
        into v_cols
        from information_schema.columns c
        where c.table_schema = 'public'
          and c.table_name = v_target
          and c.column_name not in ('tenant_id', 'version', 'party_id',
              'modified_by', 'performed_by', 'change_reason_code',
              'change_commentary', 'valid_from', 'valid_to');

        if v_has_party then
            v_party_target := 'party_id,';
            v_party_expr := format('%L::uuid,', v_party_id);
            v_party_cond := format(' and o.party_id = %L::uuid', v_party_id);
        else
            v_party_target := '';
            v_party_expr := '';
            v_party_cond := '';
        end if;

        execute format('select count(*) from %I where dataset_id = $1', v_artefact)
            into v_staged using p_dataset_id;
        v_total_staged := v_total_staged + v_staged;

        -- The publish's own parameters cannot stay bare identifiers in an
        -- EXECUTE string: format() interpolates them as literals instead.
        execute format(
            'insert into %1$I (tenant_id, %2$s version, %3$s modified_by, '
                'performed_by, change_reason_code, change_commentary) '
            'select %8$L::uuid, %4$s 0, %3$s '
                'coalesce(ores_iam_current_service_fn(), current_user), current_user, '
                '''system.external_data_import'', %5$L '
            'from %6$I a '
            'where a.dataset_id = %9$L::uuid '
              'and not exists (select 1 from %1$I o '
                  'where o.tenant_id = %8$L::uuid%7$s '
                    'and o.id = a.id '
                    'and o.valid_to = ores_utility_infinity_timestamp_fn())',
            v_target, v_party_target, v_cols, v_party_expr, v_commentary,
            v_artefact, v_party_cond,
            p_target_tenant_id::text, p_dataset_id::text);

        get diagnostics v_inserted = row_count;
        v_total_inserted := v_total_inserted + v_inserted;
    end loop;

    return query
    select 'inserted'::text, v_total_inserted
    where v_total_inserted > 0
    union all select 'skipped'::text, v_total_staged - v_total_inserted
    where v_total_staged - v_total_inserted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
