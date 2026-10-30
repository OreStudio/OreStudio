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
 * Market Data Publish-from-DQ Functions
 *
 * SECURITY DEFINER functions called by the marketdata service's NATS
 * handler for the marketdata.v1.<entity>.publish-from-dq subjects. Each
 * function reads DQ artefact tables (system tenant) and writes only to
 * ores_marketdata_* tables.
 *
 * All functions use SECURITY DEFINER set search_path = public, pg_temp so
 * they execute with the definer's privileges without needing cross-service
 * DML grants.
 */

-- =============================================================================
-- Market Data Observations: marketdata.v1.ops.publish_market_data_observations_from_dq
--
-- Generic across series identity (FX spot, rates curves today; vol surfaces,
-- ... later, as more datasets are published under the same market_data_
-- observations artefact shape) - the asset class is read off the identity's
-- own authority, so a further class is a case arm here and nothing else.
-- =============================================================================

create or replace function ores_marketdata_publish_market_data_observations_from_dq_fn(
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
    v_target_party_id uuid;
    v_dataset_name text;
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    r record;
    v_series_id uuid;
    v_asset_class text;
    v_series_subclass text;
    v_exists boolean;
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

    v_target_party_id := (p_params ->> 'party_id')::uuid;
    -- The observations the system's generators start from belong to the system party, so a
    -- publish into the system tenant names none.
    if v_target_party_id is null and p_target_tenant_id = ores_utility_system_tenant_id_fn() then
        select id into v_target_party_id
        from ores_refdata_read_system_party_fn(p_target_tenant_id)
        order by id limit 1;
    end if;
    if v_target_party_id is null then
        raise exception 'p_params.party_id is required to publish market data observations';
    end if;

    -- replace_all: soft-delete existing observations for series this
    -- dataset publishes (not a blanket tenant/party-wide delete - other
    -- feeds/datasets may own series this dataset never touches).
    if p_mode = 'replace_all' then
        update ores_marketdata_market_observations_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and party_id = v_target_party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and series_id in (
              select ms.id
              from ores_marketdata_market_series_tbl ms
              join ores_dq_market_observations_artefact_tbl dq
                on dq.oresmd_uri = ms.oresmd_uri
              where dq.dataset_id = p_dataset_id
                and dq.tenant_id = ores_utility_system_tenant_id_fn()
                and ms.tenant_id = p_target_tenant_id
                and ms.party_id = v_target_party_id
                and ms.provider_code = 'ore'
          );

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.oresmd_uri, dq.datum_uri,
            dq.observation_date, dq.value, dq.source
        from ores_dq_market_observations_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
        order by dq.oresmd_uri, dq.datum_uri
    loop
        -- The asset class and the subclass a series joins are read off the identity's
        -- own authority, which is the one part of an oresmd URI every class shares:
        -- oresmd://fx/... is an FX series and oresmd://ir/... an interest-rates one.
        -- The two are one row here rather than two cases over the same token, so a
        -- class cannot gain an arm in one and not the other. 'fx' rather than the
        -- FpML 'ForeignExchange': asset_class is validated against
        -- ores_refdata_asset_class_codes_tbl (the taxonomy table itself -- see
        -- marketdata_market_series_create.sql's insert trigger), not the unrelated
        -- FpML Bond/Commodity/.../ForeignExchange/... taxonomy.
        v_asset_class := null;
        v_series_subclass := null;
        select c.asset_class, c.series_subclass
        into v_asset_class, v_series_subclass
        from (values ('fx', 'fx', 'spot'), ('ir', 'interest_rates', 'yield'))
             as c(authority, asset_class, series_subclass)
        where c.authority = split_part(replace(r.oresmd_uri, 'oresmd://', ''), '/', 1);

        if v_asset_class is null then
            raise exception 'Unclassified series identity: % - extend this function', r.oresmd_uri;
        end if;

        select id into v_series_id
        from ores_marketdata_market_series_tbl
        where tenant_id = p_target_tenant_id
          and party_id = v_target_party_id
          and oresmd_uri = r.oresmd_uri
          and provider_code = 'ore'
          and valid_to = ores_utility_infinity_timestamp_fn();

        if v_series_id is null then
            v_series_id := gen_random_uuid();

            -- derivation_kind 'OBSERVED' with the nil-uuid/0 sentinel
            -- pair: this function publishes raw DQ-sourced observations,
            -- never a derived series -- see the check constraint on
            -- ores_marketdata_market_series_tbl (OBSERVED <-> nil-uuid/0,
            -- else both required).
            --
            -- The series is named by the series URI the dataset's rows carry,
            -- which is the series of the datum their own ORE source key names: the
            -- deposit grid's MM/RATE/USD/2D/3M row names the MM/RATE/USD/2D series.
            -- The dataset states the URI because SQL cannot run the codec; the
            -- marketdata core tests check every URI the seeds hold. The URI is the
            -- only name the series row carries, and an observation's key is
            -- written from its datum URI rather than stored.
            insert into ores_marketdata_market_series_tbl (
                tenant_id, id, version, party_id,
                oresmd_uri, series_subclass,
                provider_code, derivation_kind, derivation_config_id, derivation_config_version,
                modified_by, performed_by, change_reason_code, change_commentary
            ) values (
                p_target_tenant_id, v_series_id, 0, v_target_party_id,
                r.oresmd_uri, v_series_subclass,
                'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
                coalesce(ores_iam_current_service_fn(), current_user), current_user,
                'system.external_data_import', 'Published from DQ dataset: ' || v_dataset_name
            );

            insert into ores_marketdata_market_series_asset_classes_tbl (
                tenant_id, market_series_id, asset_class_code, version,
                modified_by, performed_by, change_reason_code, change_commentary,
                valid_from, valid_to
            ) values (
                p_target_tenant_id, v_series_id, v_asset_class, 0,
                coalesce(ores_iam_current_service_fn(), current_user), current_user,
                'system.external_data_import', 'Published from DQ dataset: ' || v_dataset_name,
                clock_timestamp(), ores_utility_infinity_timestamp_fn()
            );
        end if;

        select exists (
            select 1 from ores_marketdata_market_observations_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.party_id = v_target_party_id
              and existing.series_id = v_series_id
              and existing.observation_datetime = (r.observation_date::timestamp at time zone 'UTC')
              and existing.oresmd_uri = r.datum_uri
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_marketdata_market_observations_tbl (
            id, tenant_id, party_id, series_id, observation_datetime, oresmd_uri, value,
            source, feed_id,
            valid_from, valid_to
        ) values (
            gen_random_uuid(), p_target_tenant_id, v_target_party_id, v_series_id,
            (r.observation_date::timestamp at time zone 'UTC'), r.datum_uri, r.value::text, r.source,
            -- The source ore-import of the provider ore, installed by
            -- marketdata_catalogue_populate.sql.
            '26772cc0-57d6-4fac-be36-2ae75eb56b6b'::uuid,
            current_timestamp, ores_utility_infinity_timestamp_fn()
        );

        if v_exists then
            v_updated := v_updated + 1;
        else
            v_inserted := v_inserted + 1;
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
-- Market Data Adapters: marketdata.v1.ops.publish_adapters_from_dq
--
-- The adapter catalogue is installation data: the system tenant owns the rows
-- and every tenant reads them, so a publish into another tenant is refused
-- rather than given its own copy. Each publish replaces the live row it finds,
-- through the entity's own version trigger, so the catalogue converges on the
-- staged set however many times the dataset is published.
-- =============================================================================

create or replace function ores_marketdata_publish_adapters_from_dq_fn(
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
    v_dataset_name text;
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    r record;
    v_exists boolean;
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

    if p_target_tenant_id <> ores_utility_system_tenant_id_fn() then
        raise exception 'The adapter catalogue belongs to the system tenant; cannot publish into %.', p_target_tenant_id;
    end if;

    if p_mode = 'replace_all' then
        update ores_marketdata_adapters_tbl existing
        set valid_to = clock_timestamp()
        where existing.tenant_id = p_target_tenant_id
          and existing.valid_to = ores_utility_infinity_timestamp_fn()
          and existing.code in (
              select dq.code
              from ores_dq_adapters_artefact_tbl dq
              where dq.dataset_id = p_dataset_id
                and dq.tenant_id = ores_utility_system_tenant_id_fn());

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select dq.code, dq.name, dq.module_version, dq.description, dq.process,
               dq.asset_classes, dq.services, dq.configuration_schema,
               dq.display_order
        from ores_dq_adapters_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
        order by dq.display_order, dq.code
    loop
        select exists (
            select 1 from ores_marketdata_adapters_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.code = r.code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_marketdata_adapters_tbl (
            tenant_id, code, version, name, module_version, description, process,
            asset_classes, services, configuration_schema, display_order,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id, r.code, 0, r.name, r.module_version, r.description,
            r.process, r.asset_classes, r.services, r.configuration_schema,
            r.display_order,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.initial_load', 'Published from DQ dataset: ' || v_dataset_name
        );

        if v_exists then
            v_updated := v_updated + 1;
        else
            v_inserted := v_inserted + 1;
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
-- Market Data Suppliers: marketdata.v1.ops.publish_suppliers_from_dq
--
-- Same installation-data rule as the adapter catalogue: the system tenant owns
-- the rows, every tenant reads them, and each publish replaces the live row it
-- finds.
-- =============================================================================

create or replace function ores_marketdata_publish_suppliers_from_dq_fn(
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
    v_dataset_name text;
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    r record;
    v_exists boolean;
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

    if p_target_tenant_id <> ores_utility_system_tenant_id_fn() then
        raise exception 'The supplier catalogue belongs to the system tenant; cannot publish into %.', p_target_tenant_id;
    end if;

    if p_mode = 'replace_all' then
        update ores_marketdata_suppliers_tbl existing
        set valid_to = clock_timestamp()
        where existing.tenant_id = p_target_tenant_id
          and existing.valid_to = ores_utility_infinity_timestamp_fn()
          and existing.code in (
              select dq.code
              from ores_dq_suppliers_artefact_tbl dq
              where dq.dataset_id = p_dataset_id
                and dq.tenant_id = ores_utility_system_tenant_id_fn());

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select dq.code, dq.name, dq.kind, dq.description, dq.contract_reference,
               dq.support, dq.display_order
        from ores_dq_suppliers_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
        order by dq.display_order, dq.code
    loop
        select exists (
            select 1 from ores_marketdata_suppliers_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.code = r.code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_marketdata_suppliers_tbl (
            tenant_id, code, version, name, kind, description, contract_reference,
            support, display_order,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id, r.code, 0, r.name, r.kind, r.description,
            r.contract_reference, r.support, r.display_order,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.initial_load', 'Published from DQ dataset: ' || v_dataset_name
        );

        if v_exists then
            v_updated := v_updated + 1;
        else
            v_inserted := v_inserted + 1;
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
