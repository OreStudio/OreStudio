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
-- combining the fx_spot_generation_config sub-config fields (currency
-- pair, initial price, tick cadence, process type). The container
-- (market_data_generation_config) is shared across all FX pairs
-- published from the same dataset for the same (tenant, party): the
-- first call for a given (tenant, party, dataset) creates it, named
-- from the dataset's own display name; a second dataset published for
-- the same party (e.g. "Realistic" alongside "Basic") gets its own,
-- separate container rather than being merged into the first.
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
    v_config_name text;
    v_root_folder_id uuid;
    v_collection_folder_id uuid;
    v_asset_class_folder_id uuid;
    v_folder_id uuid;
    v_fx_config_id uuid;
    v_dataset_name text;
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    r record;
    gr record;
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

    -- Resolve (or create) the container for this (tenant, party, dataset).
    -- Keyed by dataset_id, not just (tenant, party): a party can run several
    -- distinct collections (e.g. "Basic", "Realistic") side by side, each
    -- published from its own dataset, and each must get its own container --
    -- not be silently merged into whichever one happened to publish first.
    select id into v_config_id
    from ores_synthetic_market_data_generation_configs_tbl
    where tenant_id = p_target_tenant_id
      and party_id = v_party_id
      and dataset_id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    -- The container's name is the dataset's own human-readable name (e.g.
    -- "Synthetic FX Spot Configs: Basic"), trimmed to its label after the
    -- last ": " when present ("Basic") -- not any one FX pair's artefact
    -- name, which would be a single arbitrary pair. Used both for the new
    -- container's name (below) and to namespace each pair's source_name.
    v_config_name := coalesce(nullif(split_part(v_dataset_name, ': ', 2), ''), v_dataset_name);

    if v_config_id is null then
        v_config_id := gen_random_uuid();
        insert into ores_synthetic_market_data_generation_configs_tbl (
            tenant_id, id, version, party_id, name, description, enabled, dataset_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id, v_config_id, 0, v_party_id,
            v_config_name,
            'Published from DQ dataset: ' || v_dataset_name,
            true, p_dataset_id,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Published from DQ dataset: ' || v_dataset_name
        );
    else
        -- Reusing an existing container: its name may have been renamed by
        -- the user since creation, so use the live value, not the dataset's.
        select name into v_config_name
        from ores_synthetic_market_data_generation_configs_tbl
        where tenant_id = p_target_tenant_id and id = v_config_id
          and valid_to = ores_utility_infinity_timestamp_fn();
    end if;

    if v_config_id is null then
        return query select 'skipped_no_config'::text, 0::bigint;
        return;
    end if;

    -- Resolve (or create) the folder chain this dataset's feeds live under:
    -- root ("Synthetic") > collection (linked to v_config_id via
    -- collection_id) > asset class > instrument type. Asset class/instrument
    -- type are hardcoded to FX/FX Rates for now since FX spot is the only
    -- instrument type modelled; generalize when a second one is added.
    -- Real, queryable rows (not a parsed source_name string) so any caller --
    -- Qt, ores.shell, or a wt workflow -- can resolve "everything under this
    -- folder" via the generated hierarchy function.
    select id into v_root_folder_id
    from ores_synthetic_folders_tbl
    where tenant_id = p_target_tenant_id and party_id = v_party_id
      and parent_id is null and kind = 'root'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_root_folder_id is null then
        v_root_folder_id := gen_random_uuid();
        insert into ores_synthetic_folders_tbl (
            tenant_id, id, version, party_id, parent_id, name, kind, collection_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id, v_root_folder_id, 0, v_party_id, null, 'Synthetic', 'root', null,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Root folder for synthetic feeds'
        );
    end if;

    select id into v_collection_folder_id
    from ores_synthetic_folders_tbl
    where tenant_id = p_target_tenant_id and party_id = v_party_id
      and kind = 'collection' and collection_id = v_config_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_collection_folder_id is null then
        v_collection_folder_id := gen_random_uuid();
        insert into ores_synthetic_folders_tbl (
            tenant_id, id, version, party_id, parent_id, name, kind, collection_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id, v_collection_folder_id, 0, v_party_id, v_root_folder_id,
            v_config_name, 'collection', v_config_id,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Collection folder for ' || v_config_name
        );
    else
        -- Keep the folder's display name in sync if the collection was renamed.
        update ores_synthetic_folders_tbl
        set name = v_config_name
        where tenant_id = p_target_tenant_id and id = v_collection_folder_id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and name <> v_config_name;
    end if;

    select id into v_asset_class_folder_id
    from ores_synthetic_folders_tbl
    where tenant_id = p_target_tenant_id and party_id = v_party_id
      and kind = 'asset_class' and parent_id = v_collection_folder_id and name = 'FX'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_asset_class_folder_id is null then
        v_asset_class_folder_id := gen_random_uuid();
        insert into ores_synthetic_folders_tbl (
            tenant_id, id, version, party_id, parent_id, name, kind, collection_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id, v_asset_class_folder_id, 0, v_party_id, v_collection_folder_id,
            'FX', 'asset_class', null,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Asset class folder'
        );
    end if;

    select id into v_folder_id
    from ores_synthetic_folders_tbl
    where tenant_id = p_target_tenant_id and party_id = v_party_id
      and kind = 'instrument_type' and parent_id = v_asset_class_folder_id and name = 'FX Rates'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_folder_id is null then
        v_folder_id := gen_random_uuid();
        insert into ores_synthetic_folders_tbl (
            tenant_id, id, version, party_id, parent_id, name, kind, collection_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id, v_folder_id, 0, v_party_id, v_asset_class_folder_id,
            'FX Rates', 'instrument_type', null,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Instrument type folder'
        );
    end if;

    for r in
        select
            a.base_currency_code,
            a.quote_currency_code,
            a.gmm_initial_price,
            a.ticks_per_hour,
            a.process_type,
            a.enabled,
            a.price_source,
            a.vintage_source,
            a.vintage_date
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
            tenant_id, id, version, party_id, config_id, folder_id,
            base_currency_code, quote_currency_code,
            source_name, ore_key, price_source, gmm_initial_price, ticks_per_hour, process_type,
            enabled, vintage_source, vintage_date,
            modified_by, performed_by, change_reason_code, change_commentary
        )
        select
            p_target_tenant_id,
            coalesce(existing.id, gen_random_uuid()),
            coalesce(existing.version, 0),
            v_party_id, v_config_id, v_folder_id,
            r.base_currency_code, r.quote_currency_code,
            -- source_name is now just a display/NATS-subject string, namespaced
            -- by collection only (to avoid two collections' same pair
            -- colliding) -- the actual hierarchy lives in folder_id above,
            -- not parsed out of this string.
            'synthetic.' || lower(replace(v_config_name, ' ', '')) || '.' ||
                lower(r.base_currency_code) || lower(r.quote_currency_code),
            -- ore_key uses the pair's natural base/quote order; the
            -- underlying market data is keyed the same way.
            'FX/RATE/' || r.base_currency_code || '/' || r.quote_currency_code,
            -- price_source/vintage_source/vintage_date are per-row artefact
            -- columns now — each dataset (ore_analytics, basic, realistic)
            -- carries its own vintage tag; 'fixed' rows fall back to the
            -- artefact's own gmm_initial_price instead of a vintage lookup.
            r.price_source,
            case when r.price_source = 'fixed' then r.gmm_initial_price else 0 end,
            r.ticks_per_hour, r.process_type,
            coalesce(r.enabled, true),
            coalesce(r.vintage_source, ''), coalesce(r.vintage_date, ''),
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
        -- one the mixture has nothing to sample from. Components are
        -- sourced from the dataset's own GMM component artefact rows,
        -- keyed by currency pair, so each dataset (basic, realistic, ...)
        -- carries its own calibrated mixture. Falls back to a single
        -- unrehearsed placeholder component if the dataset has none (e.g.
        -- ore_analytics before it was backfilled).
        if exists (
            select 1 from ores_dq_synthetic_gmm_components_artefact_tbl g
            where g.dataset_id = p_dataset_id
              and g.base_currency_code = r.base_currency_code
              and g.quote_currency_code = r.quote_currency_code
        ) then
            -- The mixture is a complete set per (pair, dataset), not a
            -- sparse list of independently-upserted indices: void any
            -- existing component whose index is no longer part of this
            -- dataset's mixture (e.g. a party switching from a 2-component
            -- Realistic mixture to a 1-component Basic one for the same
            -- pair must not keep Realistic's orphaned tail component).
            update ores_synthetic_gmm_components_tbl existing
            set valid_to = current_timestamp
            where existing.tenant_id = p_target_tenant_id
              and existing.fx_spot_config_id = v_fx_config_id
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
              and existing.component_index not in (
                  select g.component_index
                  from ores_dq_synthetic_gmm_components_artefact_tbl g
                  where g.dataset_id = p_dataset_id
                    and g.base_currency_code = r.base_currency_code
                    and g.quote_currency_code = r.quote_currency_code
              );

            for gr in
                select g.component_index, g.description, g.mean, g.stdev, g.weight
                from ores_dq_synthetic_gmm_components_artefact_tbl g
                where g.dataset_id = p_dataset_id
                  and g.base_currency_code = r.base_currency_code
                  and g.quote_currency_code = r.quote_currency_code
                order by g.component_index
            loop
                select exists (
                    select 1 from ores_synthetic_gmm_components_tbl existing
                    where existing.tenant_id = p_target_tenant_id
                      and existing.fx_spot_config_id = v_fx_config_id
                      and existing.component_index = gr.component_index
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
                        v_party_id, v_fx_config_id, gr.component_index,
                        gr.description, gr.mean, gr.stdev, gr.weight,
                        coalesce(ores_iam_current_service_fn(), current_user), current_user,
                        'system.external_data_import', 'Published from DQ dataset: ' || v_dataset_name
                    from (select 1) as _dummy2
                    left join ores_synthetic_gmm_components_tbl existing_gmm
                        on existing_gmm.tenant_id = p_target_tenant_id
                       and existing_gmm.fx_spot_config_id = v_fx_config_id
                       and existing_gmm.component_index = gr.component_index
                       and existing_gmm.valid_to = ores_utility_infinity_timestamp_fn();
                end if;
            end loop;
        else
            -- Same voiding rule as the branch above: this fallback publishes
            -- exactly one component (index 0), so any other pre-existing
            -- index (e.g. left over from a previously-published multi-
            -- component mixture for this fx_spot_config_id) must be voided
            -- too, not just left orphaned.
            update ores_synthetic_gmm_components_tbl existing
            set valid_to = current_timestamp
            where existing.tenant_id = p_target_tenant_id
              and existing.fx_spot_config_id = v_fx_config_id
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
              and existing.component_index <> 0;

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
-- IR Curve Configs: synthetic.v1.ir-curve-configs.publish-from-dq
--
-- Parent+child publish, same shape as FX spot configs above: each artefact
-- row is a denormalized IR curve config (currency/index, Vasicek
-- parameters, tick cadence), with its Curve Template entries carried in a
-- second artefact table keyed by (dataset, currency, index) instead of GMM
-- components. The container (market_data_generation_config) is shared
-- across all curves published from the same dataset for the same (tenant,
-- party), keyed by dataset_id exactly like FX's container resolution.
-- =============================================================================

create or replace function ores_synthetic_publish_ir_curve_configs_from_dq_fn(
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
    v_config_name text;
    v_root_folder_id uuid;
    v_collection_folder_id uuid;
    v_asset_class_folder_id uuid;
    v_folder_id uuid;
    v_ir_config_id uuid;
    v_dataset_name text;
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    r record;
    er record;
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

    v_party_id := (p_params ->> 'party_id')::uuid;
    if v_party_id is null then
        raise exception 'p_params.party_id is required to publish synthetic IR curve configs';
    end if;

    if p_mode = 'replace_all' then
        create temp table replaced_ir_configs (id uuid) on commit drop;

        insert into replaced_ir_configs (id)
        select id from ores_synthetic_ir_curve_generation_configs_tbl
        where tenant_id = p_target_tenant_id
          and party_id = v_party_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        update ores_synthetic_ir_curve_generation_configs_tbl
        set valid_to = current_timestamp
        where id in (select id from replaced_ir_configs);

        get diagnostics v_deleted = row_count;

        update ores_synthetic_ir_curve_template_entries_tbl
        set valid_to = current_timestamp
        where ir_curve_config_id in (select id from replaced_ir_configs)
          and valid_to = ores_utility_infinity_timestamp_fn();
    end if;

    -- Container resolution: identical to FX's, keyed by dataset_id so
    -- "Basic" and "Realistic" IR curve datasets each get their own
    -- container rather than merging.
    select id into v_config_id
    from ores_synthetic_market_data_generation_configs_tbl
    where tenant_id = p_target_tenant_id
      and party_id = v_party_id
      and dataset_id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    v_config_name := coalesce(nullif(split_part(v_dataset_name, ': ', 2), ''), v_dataset_name);

    if v_config_id is null then
        v_config_id := gen_random_uuid();
        insert into ores_synthetic_market_data_generation_configs_tbl (
            tenant_id, id, version, party_id, name, description, enabled, dataset_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id, v_config_id, 0, v_party_id,
            v_config_name,
            'Published from DQ dataset: ' || v_dataset_name,
            true, p_dataset_id,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Published from DQ dataset: ' || v_dataset_name
        );
    else
        select name into v_config_name
        from ores_synthetic_market_data_generation_configs_tbl
        where tenant_id = p_target_tenant_id and id = v_config_id
          and valid_to = ores_utility_infinity_timestamp_fn();
    end if;

    if v_config_id is null then
        return query select 'skipped_no_config'::text, 0::bigint;
        return;
    end if;

    -- Folder chain: root ("Synthetic") > collection > asset class ("Rates")
    -- > instrument type ("IR Curves") -- same shape as FX's FX/FX Rates
    -- chain, distinguished by asset class so both instrument families can
    -- coexist under one party's synthetic tree.
    select id into v_root_folder_id
    from ores_synthetic_folders_tbl
    where tenant_id = p_target_tenant_id and party_id = v_party_id
      and parent_id is null and kind = 'root'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_root_folder_id is null then
        v_root_folder_id := gen_random_uuid();
        insert into ores_synthetic_folders_tbl (
            tenant_id, id, version, party_id, parent_id, name, kind, collection_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id, v_root_folder_id, 0, v_party_id, null, 'Synthetic', 'root', null,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Root folder for synthetic feeds'
        );
    end if;

    select id into v_collection_folder_id
    from ores_synthetic_folders_tbl
    where tenant_id = p_target_tenant_id and party_id = v_party_id
      and kind = 'collection' and collection_id = v_config_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_collection_folder_id is null then
        v_collection_folder_id := gen_random_uuid();
        insert into ores_synthetic_folders_tbl (
            tenant_id, id, version, party_id, parent_id, name, kind, collection_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id, v_collection_folder_id, 0, v_party_id, v_root_folder_id,
            v_config_name, 'collection', v_config_id,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Collection folder for ' || v_config_name
        );
    else
        update ores_synthetic_folders_tbl
        set name = v_config_name
        where tenant_id = p_target_tenant_id and id = v_collection_folder_id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and name <> v_config_name;
    end if;

    select id into v_asset_class_folder_id
    from ores_synthetic_folders_tbl
    where tenant_id = p_target_tenant_id and party_id = v_party_id
      and kind = 'asset_class' and parent_id = v_collection_folder_id and name = 'Rates'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_asset_class_folder_id is null then
        v_asset_class_folder_id := gen_random_uuid();
        insert into ores_synthetic_folders_tbl (
            tenant_id, id, version, party_id, parent_id, name, kind, collection_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id, v_asset_class_folder_id, 0, v_party_id, v_collection_folder_id,
            'Rates', 'asset_class', null,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Asset class folder'
        );
    end if;

    select id into v_folder_id
    from ores_synthetic_folders_tbl
    where tenant_id = p_target_tenant_id and party_id = v_party_id
      and kind = 'instrument_type' and parent_id = v_asset_class_folder_id and name = 'IR Curves'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_folder_id is null then
        v_folder_id := gen_random_uuid();
        insert into ores_synthetic_folders_tbl (
            tenant_id, id, version, party_id, parent_id, name, kind, collection_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id, v_folder_id, 0, v_party_id, v_asset_class_folder_id,
            'IR Curves', 'instrument_type', null,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Instrument type folder'
        );
    end if;

    for r in
        select
            a.currency_code,
            a.index_name,
            a.process_type,
            a.kappa,
            a.theta,
            a.sigma,
            a.initial_rate,
            a.ticks_per_hour,
            a.fixed_leg_payment_frequency_code,
            a.enabled,
            a.auto_start,
            a.description
        from ores_dq_synthetic_ir_curve_configs_artefact_tbl a
        where a.dataset_id = p_dataset_id
        order by a.currency_code, a.index_name
    loop
        select exists (
            select 1 from ores_synthetic_ir_curve_generation_configs_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.party_id = v_party_id
              and existing.config_id = v_config_id
              and existing.currency_code = r.currency_code
              and existing.index_name = r.index_name
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_synthetic_ir_curve_generation_configs_tbl (
            tenant_id, id, version, party_id, config_id, folder_id,
            currency_code, index_name, process_type,
            kappa, theta, sigma, initial_rate, ticks_per_hour, enabled,
            auto_start, description,
            fixed_leg_payment_frequency_code, source_name,
            modified_by, performed_by, change_reason_code, change_commentary
        )
        select
            p_target_tenant_id,
            coalesce(existing.id, gen_random_uuid()),
            coalesce(existing.version, 0),
            v_party_id, v_config_id, v_folder_id,
            r.currency_code, r.index_name, r.process_type,
            r.kappa, r.theta, r.sigma, r.initial_rate, r.ticks_per_hour,
            coalesce(r.enabled, true),
            coalesce(r.auto_start, false),
            r.description,
            r.fixed_leg_payment_frequency_code,
            -- Same shape as fx_spot_generation_config.source_name: namespaced by collection only
            -- (so two collections' same currency+index never collide), not by asset class -- the
            -- "<CCY>-" prefix index_name already carries (see that column's own doc) is stripped
            -- back off first so the tail reads "usdsofr", not "usdusd-sofr".
            'synthetic.' || lower(replace(v_config_name, ' ', '')) || '.' ||
                lower(r.currency_code) ||
                lower(case when r.index_name like r.currency_code || '-%'
                           then substring(r.index_name from length(r.currency_code) + 2)
                           else r.index_name end),
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Published from DQ dataset: ' || v_dataset_name
        from (select 1) as _dummy
        left join ores_synthetic_ir_curve_generation_configs_tbl existing
            on existing.tenant_id = p_target_tenant_id
           and existing.party_id = v_party_id
           and existing.config_id = v_config_id
           and existing.currency_code = r.currency_code
           and existing.index_name = r.index_name
           and existing.valid_to = ores_utility_infinity_timestamp_fn()
        returning id, version into v_ir_config_id, v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;

        -- Curve Template entries are a complete set per (curve, dataset),
        -- not a sparse independently-upserted list: void any existing entry
        -- whose sequence_index is no longer part of this dataset's template
        -- (e.g. switching a curve from Realistic's 7-entry template to
        -- Basic's 3-entry one must not keep Realistic's orphaned tail
        -- entries), same rule as FX's GMM component sync above.
        update ores_synthetic_ir_curve_template_entries_tbl existing
        set valid_to = current_timestamp
        where existing.tenant_id = p_target_tenant_id
          and existing.ir_curve_config_id = v_ir_config_id
          and existing.valid_to = ores_utility_infinity_timestamp_fn()
          and existing.sequence_index not in (
              select e.sequence_index
              from ores_dq_synthetic_ir_curve_template_entries_artefact_tbl e
              where e.dataset_id = p_dataset_id
                and e.currency_code = r.currency_code
                and e.index_name = r.index_name
          );

        for er in
            select e.sequence_index, e.start_tenor_code, e.end_tenor_code, e.instrument_code
            from ores_dq_synthetic_ir_curve_template_entries_artefact_tbl e
            where e.dataset_id = p_dataset_id
              and e.currency_code = r.currency_code
              and e.index_name = r.index_name
            order by e.sequence_index
        loop
            select exists (
                select 1 from ores_synthetic_ir_curve_template_entries_tbl existing
                where existing.tenant_id = p_target_tenant_id
                  and existing.ir_curve_config_id = v_ir_config_id
                  and existing.sequence_index = er.sequence_index
                  and existing.valid_to = ores_utility_infinity_timestamp_fn()
            ) into v_exists;

            if not (p_mode = 'insert_only' and v_exists) then
                insert into ores_synthetic_ir_curve_template_entries_tbl (
                    tenant_id, id, version, party_id, ir_curve_config_id, sequence_index,
                    start_tenor_code, end_tenor_code, instrument_code,
                    modified_by, performed_by, change_reason_code, change_commentary
                )
                select
                    p_target_tenant_id,
                    coalesce(existing_e.id, gen_random_uuid()),
                    coalesce(existing_e.version, 0),
                    v_party_id, v_ir_config_id, er.sequence_index,
                    er.start_tenor_code, er.end_tenor_code, er.instrument_code,
                    coalesce(ores_iam_current_service_fn(), current_user), current_user,
                    'system.external_data_import', 'Published from DQ dataset: ' || v_dataset_name
                from (select 1) as _dummy2
                left join ores_synthetic_ir_curve_template_entries_tbl existing_e
                    on existing_e.tenant_id = p_target_tenant_id
                   and existing_e.ir_curve_config_id = v_ir_config_id
                   and existing_e.sequence_index = er.sequence_index
                   and existing_e.valid_to = ores_utility_infinity_timestamp_fn();
            end if;
        end loop;
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
