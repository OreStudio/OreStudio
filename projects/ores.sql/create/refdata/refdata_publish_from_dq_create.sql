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
            select assets.image_id into v_resolved_image_id
            from ores_dq_images_artefact_tbl dq_img
            join ores_assets_images_tbl assets on assets.key = dq_img.key
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
            select assets.image_id into v_resolved_image_id
            from ores_dq_images_artefact_tbl dq_img
            join ores_assets_images_tbl assets on assets.key = dq_img.key
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
begin
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
            advance_calendar, business_day_convention, spot_relative, end_of_month,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.pair_code, 0, r.pip_factor, r.tick_size, r.decimal_places,
            r.advance_calendar, r.business_day_convention, r.spot_relative, r.end_of_month,
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
    v_dataset_size text;
    v_entity_dataset_id uuid;
    v_rel_dataset_id uuid;
    v_bic_dataset_id uuid;
begin
    select name, code into v_dataset_name, v_dataset_code
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    v_dataset_size := coalesce(
        substring(v_dataset_code from '[^.]+$'),
        p_params ->> 'lei_dataset_size',
        'small');

    select id into v_entity_dataset_id
    from ores_dq_datasets_tbl
    where code = 'gleif.lei_entities.' || v_dataset_size
      and valid_to = ores_utility_infinity_timestamp_fn();

    select id into v_rel_dataset_id
    from ores_dq_datasets_tbl
    where code = 'gleif.lei_relationships.' || v_dataset_size
      and valid_to = ores_utility_infinity_timestamp_fn();

    select id into v_bic_dataset_id
    from ores_dq_datasets_tbl
    where code = 'gleif.lei_bic'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_entity_dataset_id is null or v_rel_dataset_id is null then
        raise exception 'LEI dataset not found for size: %', v_dataset_size;
    end if;

    if exists (
        select 1
        from ores_refdata_counterparty_identifiers_tbl ci
        where ci.tenant_id = p_target_tenant_id
          and ci.id_scheme = 'LEI'
          and ci.valid_to = ores_utility_infinity_timestamp_fn()
        limit 1
    ) then
        raise notice 'Target tenant already has LEI counterparties, skipping.';
        return query select 'skipped'::text, 0::bigint;
        return;
    end if;

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

    update lei_counterparty_uuid_map m
    set parent_lei = r.relationship_end_node_node_id
    from ores_dq_lei_relationships_artefact_tbl r
    where r.relationship_start_node_node_id = m.lei
      and r.relationship_relationship_type = 'IS_DIRECTLY_CONSOLIDATED_BY'
      and r.relationship_relationship_status = 'ACTIVE'
      and r.dataset_id = v_rel_dataset_id
      and exists (select 1 from lei_counterparty_uuid_map p
                  where p.lei = r.relationship_end_node_node_id);

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
                parent_counterparty_id, business_center_code, status,
                modified_by, performed_by, change_reason_code, change_commentary
            )
            select
                p_target_tenant_id,
                m.counterparty_uuid, 0,
                m.entity_legal_name,
                m.short_code, m.entity_transliterated_name_1, 'Corporate',
                parent_map.counterparty_uuid,
                coalesce(bc_map.business_center_code, 'WRLD'),
                'Active',
                coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
                'Imported from GLEIF LEI dataset: ' || v_dataset_name
            from lei_counterparty_uuid_map m
            left join lei_counterparty_uuid_map parent_map on parent_map.lei = m.parent_lei
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
            v_inserted_counterparties := v_inserted_counterparties + v_level_count;
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
    v_dataset_size text;
    v_entity_dataset_id uuid;
    v_rel_dataset_id uuid;
    v_bic_dataset_id uuid;
begin
    select name, code into v_dataset_name, v_dataset_code
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    v_dataset_size := coalesce(
        substring(v_dataset_code from '[^.]+$'),
        p_params ->> 'lei_dataset_size',
        'small');

    select id into v_entity_dataset_id
    from ores_dq_datasets_tbl
    where code = 'gleif.lei_entities.' || v_dataset_size
      and valid_to = ores_utility_infinity_timestamp_fn();

    select id into v_rel_dataset_id
    from ores_dq_datasets_tbl
    where code = 'gleif.lei_relationships.' || v_dataset_size
      and valid_to = ores_utility_infinity_timestamp_fn();

    select id into v_bic_dataset_id
    from ores_dq_datasets_tbl
    where code = 'gleif.lei_bic'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_entity_dataset_id is null or v_rel_dataset_id is null then
        raise exception 'LEI dataset not found for size: %', v_dataset_size;
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
    v_root_party_id uuid;
    v_bu_dataset_id uuid;
    v_inserted bigint := 0;
    v_current_depth int;
    v_max_depth int;
    v_level_count bigint;
begin
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

    select id into v_bu_dataset_id
    from ores_dq_datasets_tbl
    where code = 'testdata.business_units'
      and valid_to = ores_utility_infinity_timestamp_fn();

    create temp table bu_ref_map (
        artefact_id uuid primary key,
        published_id uuid not null
    ) on commit drop;

    if v_bu_dataset_id is not null then
        insert into bu_ref_map (artefact_id, published_id)
        select a.id, r.id
        from ores_dq_business_units_artefact_tbl a
        join ores_refdata_business_units_tbl r
            on r.unit_name = a.unit_name
            and r.tenant_id = p_target_tenant_id
            and r.valid_to = ores_utility_infinity_timestamp_fn()
        where a.dataset_id = v_bu_dataset_id;
    end if;

    create temp table portfolio_publish_map (
        artefact_id uuid primary key,
        new_id uuid not null default gen_random_uuid(),
        parent_artefact_id uuid,
        owner_unit_artefact_id uuid,
        depth int not null default 0,
        name text not null,
        purpose_type text,
        aggregation_ccy text,
        is_virtual integer
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
            'active',
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
    v_root_party_id uuid;
    v_portfolio_dataset_id uuid;
    v_inserted bigint := 0;
begin
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

    select id into v_portfolio_dataset_id
    from ores_dq_datasets_tbl
    where code = 'testdata.portfolios'
      and valid_to = ores_utility_infinity_timestamp_fn();

    create temp table portfolio_ref_map (
        artefact_id uuid primary key,
        published_id uuid not null,
        published_owner_unit_id uuid
    ) on commit drop;

    if v_portfolio_dataset_id is not null then
        insert into portfolio_ref_map (artefact_id, published_id, published_owner_unit_id)
        select a.id, r.id, r.owner_unit_id
        from ores_dq_portfolios_artefact_tbl a
        join ores_refdata_portfolios_tbl r
            on r.name = a.name
            and r.tenant_id = p_target_tenant_id
            and r.valid_to = ores_utility_infinity_timestamp_fn()
        where a.dataset_id = v_portfolio_dataset_id;
    end if;

    insert into ores_refdata_books_tbl (
        tenant_id, id, version, party_id, name,
        parent_portfolio_id, functional_currency, gl_account_ref, cost_center,
        book_status, regulatory_book_type, is_sweepable, rates_centre_code, owner_unit_id,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        p_target_tenant_id,
        gen_random_uuid(), 0, v_root_party_id, a.name,
        pmap.published_id, a.functional_currency, a.gl_account_ref, a.cost_center,
        a.book_status, a.regulatory_book_type, a.is_sweepable,
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
-- CRM Topology Bundles: refdata.v1.crm-topology-bundles.publish-from-dq
-- =============================================================================

/**
 * CRM Topology Bundles Publish-from-DQ Function
 *
 * SECURITY DEFINER function called by the refdata service's NATS
 * handler for the refdata.v1.crm-topology-bundles.publish-from-dq
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
