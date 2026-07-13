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
-- Market Data Observations: marketdata.v1.market-data-observations.publish-from-dq
--
-- Generic across series_type/metric (FX spot today; rates curves, vol
-- surfaces, ... later, as more datasets are published under the same
-- market_data_observations artefact shape) - classification below only
-- covers FX/RATE (this dataset's only content); extend the case when a
-- non-FX dataset is added.
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
    v_is_scalar boolean;
    v_exists boolean;
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

    v_target_party_id := (p_params ->> 'party_id')::uuid;
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
              join ores_dq_market_data_observations_artefact_tbl dq
                on dq.series_type = ms.series_type
               and dq.metric = ms.metric
               and dq.qualifier = ms.qualifier
              where dq.dataset_id = p_dataset_id
                and dq.tenant_id = ores_utility_system_tenant_id_fn()
                and ms.tenant_id = p_target_tenant_id
                and ms.party_id = v_target_party_id
          );

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.series_type, dq.metric, dq.qualifier, dq.point_id,
            dq.observation_date, dq.value, dq.source
        from ores_dq_market_data_observations_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
        order by dq.series_type, dq.metric, dq.qualifier
    loop
        if r.series_type = 'FX' and r.metric = 'RATE' then
            v_asset_class := 'fx';
            v_series_subclass := 'spot';
            v_is_scalar := true;
        else
            raise exception 'Unclassified series_type/metric: %/% - extend this function', r.series_type, r.metric;
        end if;

        select id into v_series_id
        from ores_marketdata_market_series_tbl
        where tenant_id = p_target_tenant_id
          and party_id = v_target_party_id
          and series_type = r.series_type
          and metric = r.metric
          and qualifier = r.qualifier
          and valid_to = ores_utility_infinity_timestamp_fn();

        if v_series_id is null then
            v_series_id := gen_random_uuid();

            insert into ores_marketdata_market_series_tbl (
                tenant_id, id, version, party_id,
                series_type, metric, qualifier, asset_class, series_subclass, is_scalar,
                modified_by, performed_by, change_reason_code, change_commentary
            ) values (
                p_target_tenant_id, v_series_id, 0, v_target_party_id,
                r.series_type, r.metric, r.qualifier, v_asset_class, v_series_subclass, v_is_scalar,
                coalesce(ores_iam_current_service_fn(), current_user), current_user,
                'system.external_data_import', 'Published from DQ dataset: ' || v_dataset_name
            );
        end if;

        select exists (
            select 1 from ores_marketdata_market_observations_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.party_id = v_target_party_id
              and existing.series_id = v_series_id
              and existing.observation_datetime = r.observation_date::timestamptz
              and coalesce(existing.point_id, '') = coalesce(r.point_id, '')
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_marketdata_market_observations_tbl (
            id, tenant_id, party_id, series_id, observation_datetime, point_id, value, source,
            valid_from, valid_to
        ) values (
            gen_random_uuid(), p_target_tenant_id, v_target_party_id, v_series_id,
            r.observation_date::timestamptz, r.point_id, r.value::text, r.source,
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
