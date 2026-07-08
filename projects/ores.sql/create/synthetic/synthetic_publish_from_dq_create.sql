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
 * Synthetic Publish-from-DQ Functions
 *
 * SECURITY DEFINER functions called by the synthetic service NATS handler for the
 * synthetic.v1.<entity>.publish-from-dq subjects. Each function reads DQ artefact
 * tables (system tenant) and writes only to ores_synthetic_* tables.
 *
 * All functions use SECURITY DEFINER set search_path = public, pg_temp so they
 * execute with the definer's privileges without needing cross-service DML grants.
 */

-- =============================================================================
-- FX Spot Configs: synthetic.v1.fx-spot-configs.publish-from-dq
--
-- Parent+child publish: each artefact row is a denormalized FX config
-- combining the market_data_generation_config container fields (name,
-- description, enabled) with the fx_spot_generation_config sub-config
-- fields (currency pair, initial price, tick cadence, process type). The
-- container is shared across all FX pairs for the same (tenant, party): the
-- first artefact row for a party creates it, subsequent rows reuse it.
-- =============================================================================

create or replace function ores_synthetic_publish_fx_spot_configs_from_dq_fn(
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
    v_party_id uuid;
    v_config_id uuid;
    v_fx_config_id uuid;
    v_dataset_name text;
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
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

    -- Party is not derivable from any "root party" convention here — synthetic
    -- configs are always scoped to an explicit party the caller names.
    v_party_id := (p_params ->> 'party_id')::uuid;
    if v_party_id is null then
        raise exception 'p_params.party_id is required to publish synthetic FX spot configs';
    end if;

    if p_mode = 'replace_all' then
        -- Capture the fx-config ids being replaced before voiding them, so the
        -- GMM components that reference them can be voided too — otherwise
        -- they're orphaned (still valid_to = infinity) pointing at a now-closed
        -- fx_spot_config_id, since the re-inserted rows below get fresh ids.
        create temp table replaced_fx_configs (id uuid) on commit drop;

        insert into replaced_fx_configs (id)
        select id from ores_synthetic_fx_spot_generation_configs_tbl
        where tenant_id = p_target_tenant_id
          and party_id = v_party_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        update ores_synthetic_fx_spot_generation_configs_tbl
        set valid_to = current_timestamp
        where id in (select id from replaced_fx_configs);

        get diagnostics v_deleted = row_count;

        update ores_synthetic_gmm_components_tbl
        set valid_to = current_timestamp
        where fx_spot_config_id in (select id from replaced_fx_configs)
          and valid_to = ores_utility_infinity_timestamp_fn();
    end if;

    -- Resolve (or create) the shared container for this (tenant, party).
    select id into v_config_id
    from ores_synthetic_market_data_generation_configs_tbl
    where tenant_id = p_target_tenant_id
      and party_id = v_party_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_config_id is null then
        select a.name, a.description, a.enabled
        into r
        from ores_dq_synthetic_fx_spot_configs_artefact_tbl a
        where a.dataset_id = p_dataset_id
        order by a.name
        limit 1;

        if found then
            v_config_id := gen_random_uuid();
            insert into ores_synthetic_market_data_generation_configs_tbl (
                tenant_id, id, version, party_id, name, description, enabled,
                modified_by, performed_by, change_reason_code, change_commentary
            ) values (
                p_target_tenant_id, v_config_id, 0, v_party_id,
                coalesce(r.name, 'synthetic'), coalesce(r.description, ''),
                coalesce(r.enabled, true),
                coalesce(ores_iam_current_service_fn(), current_user), current_user,
                'system.external_data_import', 'Published from DQ dataset: ' || v_dataset_name
            );
        end if;
    end if;

    if v_config_id is null then
        return query select 'skipped_no_config'::text, 0::bigint;
        return;
    end if;

    for r in
        select
            a.base_currency_code,
            a.quote_currency_code,
            a.gmm_initial_price,
            a.ticks_per_hour,
            a.process_type,
            a.enabled
        from ores_dq_synthetic_fx_spot_configs_artefact_tbl a
        where a.dataset_id = p_dataset_id
        order by a.base_currency_code, a.quote_currency_code
    loop
        select exists (
            select 1 from ores_synthetic_fx_spot_generation_configs_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.party_id = v_party_id
              and existing.config_id = v_config_id
              and existing.base_currency_code = r.base_currency_code
              and existing.quote_currency_code = r.quote_currency_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_synthetic_fx_spot_generation_configs_tbl (
            tenant_id, id, version, party_id, config_id,
            base_currency_code, quote_currency_code,
            source_name, ore_key, gmm_initial_price, ticks_per_hour, process_type,
            enabled, vintage_source, vintage_date,
            modified_by, performed_by, change_reason_code, change_commentary
        )
        select
            p_target_tenant_id,
            coalesce(existing.id, gen_random_uuid()),
            coalesce(existing.version, 0),
            v_party_id, v_config_id,
            r.base_currency_code, r.quote_currency_code,
            'synthetic.' || lower(r.base_currency_code) || lower(r.quote_currency_code),
            'FX/RATE/' || r.base_currency_code || '/' || r.quote_currency_code,
            r.gmm_initial_price, r.ticks_per_hour, r.process_type,
            coalesce(r.enabled, true),
            -- The DQ artefact has no per-row vintage; every synthetic FX config
            -- published from this dataset seeds from the same ORE reference
            -- vintage the story targets. Revisit if/when a dataset carries its
            -- own vintage (see "Seed Basic and Realistic dataset bundles").
            'ore.reference', '2016-02-05',
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Published from DQ dataset: ' || v_dataset_name
        from (select 1) as _dummy
        left join ores_synthetic_fx_spot_generation_configs_tbl existing
            on existing.tenant_id = p_target_tenant_id
           and existing.party_id = v_party_id
           and existing.config_id = v_config_id
           and existing.base_currency_code = r.base_currency_code
           and existing.quote_currency_code = r.quote_currency_code
           and existing.valid_to = ores_utility_infinity_timestamp_fn()
        returning id, version into v_fx_config_id, v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;

        -- Ensure every FX config has at least one GMM component; without
        -- one the mixture has nothing to sample from. This is a single
        -- placeholder component (no drift, plausible per-tick volatility),
        -- pending a proper calibration — see the deferred 30-pair library
        -- task for the real analysis.
        select exists (
            select 1 from ores_synthetic_gmm_components_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.fx_spot_config_id = v_fx_config_id
              and existing.component_index = 0
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if not (p_mode = 'insert_only' and v_exists) then
            insert into ores_synthetic_gmm_components_tbl (
                tenant_id, id, version, party_id, fx_spot_config_id, component_index,
                description, mean, stdev, weight,
                modified_by, performed_by, change_reason_code, change_commentary
            )
            select
                p_target_tenant_id,
                coalesce(existing_gmm.id, gen_random_uuid()),
                coalesce(existing_gmm.version, 0),
                v_party_id, v_fx_config_id, 0,
                'Default single-component GMM (placeholder, pending calibration)',
                0.0, 0.0005, 1.0,
                coalesce(ores_iam_current_service_fn(), current_user), current_user,
                'system.external_data_import', 'Published from DQ dataset: ' || v_dataset_name
            from (select 1) as _dummy2
            left join ores_synthetic_gmm_components_tbl existing_gmm
                on existing_gmm.tenant_id = p_target_tenant_id
               and existing_gmm.fx_spot_config_id = v_fx_config_id
               and existing_gmm.component_index = 0
               and existing_gmm.valid_to = ores_utility_infinity_timestamp_fn();
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
