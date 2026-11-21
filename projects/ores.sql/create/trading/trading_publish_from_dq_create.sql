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
 * Publication of ORE's script library from its DQ dataset
 *
 * A script is the row of ores_trading_scripts_tbl and its rows in the six tables
 * beneath it. The library is published script by script: a script whose staged
 * rows equal the ones the target holds is skipped, and any other is written
 * whole as a new version, its rows that are no longer staged closed. So a
 * republished dataset versions only the scripts that changed.
 */

-- =============================================================================
-- The state of one script, staged or published, in one comparable form
-- =============================================================================

create or replace function ores_trading_staged_script_state_fn(
    p_dataset_id uuid,
    p_script_name text
) returns jsonb as $$
    select jsonb_build_object(
        'script', (select jsonb_build_object('product_tag', s.product_tag,
                                             'description', s.description)
                   from ores_dq_scripts_artefact_tbl s
                   where s.dataset_id = p_dataset_id and s.name = p_script_name),
        'variants', (select coalesce(jsonb_agg(jsonb_build_array(
                             v.purpose, v.code, v.npv_variable) order by v.purpose), '[]')
                     from ores_dq_script_variants_artefact_tbl v
                     where v.dataset_id = p_dataset_id and v.script_name = p_script_name),
        'results', (select coalesce(jsonb_agg(jsonb_build_array(
                            r.purpose, r.sequence_number, r.variable, r.rename)
                            order by r.purpose, r.sequence_number), '[]')
                    from ores_dq_script_results_artefact_tbl r
                    where r.dataset_id = p_dataset_id and r.script_name = p_script_name),
        'calibrations', (select coalesce(jsonb_agg(jsonb_build_array(
                                 c.purpose, c.sequence_number, c.index_variable)
                                 order by c.purpose, c.sequence_number), '[]')
                         from ores_dq_script_calibrations_artefact_tbl c
                         where c.dataset_id = p_dataset_id and c.script_name = p_script_name),
        'strikes', (select coalesce(jsonb_agg(jsonb_build_array(
                            k.purpose, k.calibration_number, k.sequence_number, k.strike_variable)
                            order by k.purpose, k.calibration_number, k.sequence_number), '[]')
                    from ores_dq_script_calibration_strikes_artefact_tbl k
                    where k.dataset_id = p_dataset_id and k.script_name = p_script_name),
        'coarsenings', (select coalesce(jsonb_agg(jsonb_build_array(
                                o.purpose, o.sequence_number, o.eligible_schedule)
                                order by o.purpose, o.sequence_number), '[]')
                        from ores_dq_script_coarsenings_artefact_tbl o
                        where o.dataset_id = p_dataset_id and o.script_name = p_script_name),
        'inputs', (select coalesce(jsonb_agg(jsonb_build_array(
                           i.sequence_number, i.input_name, i.data_type, i.is_array,
                           i.value_rule, i.description) order by i.sequence_number), '[]')
                   from ores_dq_script_inputs_artefact_tbl i
                   where i.dataset_id = p_dataset_id and i.script_name = p_script_name));
$$ language sql stable;

create or replace function ores_trading_published_script_state_fn(
    p_tenant_id uuid,
    p_script_name text
) returns jsonb as $$
    select jsonb_build_object(
        'script', (select jsonb_build_object('product_tag', s.product_tag,
                                             'description', s.description)
                   from ores_trading_scripts_tbl s
                   where s.tenant_id = p_tenant_id and s.name = p_script_name
                     and s.valid_to = ores_utility_infinity_timestamp_fn()),
        'variants', (select coalesce(jsonb_agg(jsonb_build_array(
                             v.purpose, v.code, v.npv_variable) order by v.purpose), '[]')
                     from ores_trading_script_variants_tbl v
                     where v.tenant_id = p_tenant_id and v.script_name = p_script_name
                       and v.valid_to = ores_utility_infinity_timestamp_fn()),
        'results', (select coalesce(jsonb_agg(jsonb_build_array(
                            r.purpose, r.sequence_number, r.variable, r.rename)
                            order by r.purpose, r.sequence_number), '[]')
                    from ores_trading_script_results_tbl r
                    where r.tenant_id = p_tenant_id and r.script_name = p_script_name
                      and r.valid_to = ores_utility_infinity_timestamp_fn()),
        'calibrations', (select coalesce(jsonb_agg(jsonb_build_array(
                                 c.purpose, c.sequence_number, c.index_variable)
                                 order by c.purpose, c.sequence_number), '[]')
                         from ores_trading_script_calibrations_tbl c
                         where c.tenant_id = p_tenant_id and c.script_name = p_script_name
                           and c.valid_to = ores_utility_infinity_timestamp_fn()),
        'strikes', (select coalesce(jsonb_agg(jsonb_build_array(
                            k.purpose, k.calibration_number, k.sequence_number, k.strike_variable)
                            order by k.purpose, k.calibration_number, k.sequence_number), '[]')
                    from ores_trading_script_calibration_strikes_tbl k
                    where k.tenant_id = p_tenant_id and k.script_name = p_script_name
                      and k.valid_to = ores_utility_infinity_timestamp_fn()),
        'coarsenings', (select coalesce(jsonb_agg(jsonb_build_array(
                                o.purpose, o.sequence_number, o.eligible_schedule)
                                order by o.purpose, o.sequence_number), '[]')
                        from ores_trading_script_coarsenings_tbl o
                        where o.tenant_id = p_tenant_id and o.script_name = p_script_name
                          and o.valid_to = ores_utility_infinity_timestamp_fn()),
        'inputs', (select coalesce(jsonb_agg(jsonb_build_array(
                           i.sequence_number, i.input_name, i.data_type, i.is_array,
                           i.value_rule, i.description) order by i.sequence_number), '[]')
                   from ores_trading_script_inputs_tbl i
                   where i.tenant_id = p_tenant_id and i.script_name = p_script_name
                     and i.valid_to = ores_utility_infinity_timestamp_fn()));
$$ language sql stable;

-- =============================================================================
-- Script library: trading.v1.ops.publish_script_library_from_dq
-- =============================================================================

-- compass:except(entity_writes/sql_writer) expires=2027-05-18 approver=marco reason=writes the script library's live tables from its staging tables, as every publish from DQ does; recorded in entity_table_writes_baseline.json
create or replace function ores_trading_publish_script_library_from_dq_fn(
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
    v_dataset_name text;
    v_modified_by text := coalesce(ores_iam_current_service_fn(), current_user);
    v_commentary text;
    v_exists boolean;
    v_name text;
begin
    -- A publish replaces the version it finds, so it asks for the version
    -- replace the store grants a bulk writer.
    perform ores_utility_allow_version_replace_fn();
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();
    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;
    if p_mode not in ('upsert', 'insert_only') then
        raise exception 'Invalid mode: %. Use upsert or insert_only', p_mode;
    end if;
    v_commentary := 'Imported from DQ dataset: ' || v_dataset_name;

    for v_name in
        select s.name from ores_dq_scripts_artefact_tbl s
        where s.dataset_id = p_dataset_id
          and s.tenant_id = ores_utility_system_tenant_id_fn()
        order by s.name
    loop
        select exists (
            select 1 from ores_trading_scripts_tbl t
            where t.tenant_id = p_target_tenant_id and t.name = v_name
              and t.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if v_exists and (p_mode = 'insert_only'
            or ores_trading_staged_script_state_fn(p_dataset_id, v_name)
               = ores_trading_published_script_state_fn(p_target_tenant_id, v_name)) then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- The script's rows that the dataset no longer stages are closed.
        update ores_trading_script_variants_tbl t set valid_to = current_timestamp
        where t.tenant_id = p_target_tenant_id and t.script_name = v_name
          and t.valid_to = ores_utility_infinity_timestamp_fn()
          and not exists (select 1 from ores_dq_script_variants_artefact_tbl a
                          where a.dataset_id = p_dataset_id and a.script_name = v_name
                            and a.purpose = t.purpose);
        update ores_trading_script_results_tbl t set valid_to = current_timestamp
        where t.tenant_id = p_target_tenant_id and t.script_name = v_name
          and t.valid_to = ores_utility_infinity_timestamp_fn()
          and not exists (select 1 from ores_dq_script_results_artefact_tbl a
                          where a.dataset_id = p_dataset_id and a.script_name = v_name
                            and a.purpose = t.purpose and a.sequence_number = t.sequence_number);
        update ores_trading_script_calibrations_tbl t set valid_to = current_timestamp
        where t.tenant_id = p_target_tenant_id and t.script_name = v_name
          and t.valid_to = ores_utility_infinity_timestamp_fn()
          and not exists (select 1 from ores_dq_script_calibrations_artefact_tbl a
                          where a.dataset_id = p_dataset_id and a.script_name = v_name
                            and a.purpose = t.purpose and a.sequence_number = t.sequence_number);
        update ores_trading_script_calibration_strikes_tbl t set valid_to = current_timestamp
        where t.tenant_id = p_target_tenant_id and t.script_name = v_name
          and t.valid_to = ores_utility_infinity_timestamp_fn()
          and not exists (select 1 from ores_dq_script_calibration_strikes_artefact_tbl a
                          where a.dataset_id = p_dataset_id and a.script_name = v_name
                            and a.purpose = t.purpose
                            and a.calibration_number = t.calibration_number
                            and a.sequence_number = t.sequence_number);
        update ores_trading_script_coarsenings_tbl t set valid_to = current_timestamp
        where t.tenant_id = p_target_tenant_id and t.script_name = v_name
          and t.valid_to = ores_utility_infinity_timestamp_fn()
          and not exists (select 1 from ores_dq_script_coarsenings_artefact_tbl a
                          where a.dataset_id = p_dataset_id and a.script_name = v_name
                            and a.purpose = t.purpose and a.sequence_number = t.sequence_number);
        update ores_trading_script_inputs_tbl t set valid_to = current_timestamp
        where t.tenant_id = p_target_tenant_id and t.script_name = v_name
          and t.valid_to = ores_utility_infinity_timestamp_fn()
          and not exists (select 1 from ores_dq_script_inputs_artefact_tbl a
                          where a.dataset_id = p_dataset_id and a.script_name = v_name
                            and a.sequence_number = t.sequence_number);

        insert into ores_trading_scripts_tbl (
            name, tenant_id, version, product_tag, description,
            modified_by, performed_by, change_reason_code, change_commentary)
        select s.name, p_target_tenant_id, 0, s.product_tag, s.description,
               v_modified_by, current_user, 'system.external_data_import', v_commentary
        from ores_dq_scripts_artefact_tbl s
        where s.dataset_id = p_dataset_id and s.name = v_name;

        insert into ores_trading_script_variants_tbl (
            script_name, purpose, tenant_id, version, code, npv_variable,
            modified_by, performed_by, change_reason_code, change_commentary)
        select a.script_name, a.purpose, p_target_tenant_id, 0, a.code, a.npv_variable,
               v_modified_by, current_user, 'system.external_data_import', v_commentary
        from ores_dq_script_variants_artefact_tbl a
        where a.dataset_id = p_dataset_id and a.script_name = v_name;

        insert into ores_trading_script_results_tbl (
            script_name, purpose, sequence_number, tenant_id, version, variable, rename,
            modified_by, performed_by, change_reason_code, change_commentary)
        select a.script_name, a.purpose, a.sequence_number, p_target_tenant_id, 0,
               a.variable, a.rename,
               v_modified_by, current_user, 'system.external_data_import', v_commentary
        from ores_dq_script_results_artefact_tbl a
        where a.dataset_id = p_dataset_id and a.script_name = v_name;

        insert into ores_trading_script_calibrations_tbl (
            script_name, purpose, sequence_number, tenant_id, version, index_variable,
            modified_by, performed_by, change_reason_code, change_commentary)
        select a.script_name, a.purpose, a.sequence_number, p_target_tenant_id, 0,
               a.index_variable,
               v_modified_by, current_user, 'system.external_data_import', v_commentary
        from ores_dq_script_calibrations_artefact_tbl a
        where a.dataset_id = p_dataset_id and a.script_name = v_name;

        insert into ores_trading_script_calibration_strikes_tbl (
            script_name, purpose, calibration_number, sequence_number, tenant_id, version,
            strike_variable,
            modified_by, performed_by, change_reason_code, change_commentary)
        select a.script_name, a.purpose, a.calibration_number, a.sequence_number,
               p_target_tenant_id, 0, a.strike_variable,
               v_modified_by, current_user, 'system.external_data_import', v_commentary
        from ores_dq_script_calibration_strikes_artefact_tbl a
        where a.dataset_id = p_dataset_id and a.script_name = v_name;

        insert into ores_trading_script_coarsenings_tbl (
            script_name, purpose, sequence_number, tenant_id, version, eligible_schedule,
            modified_by, performed_by, change_reason_code, change_commentary)
        select a.script_name, a.purpose, a.sequence_number, p_target_tenant_id, 0,
               a.eligible_schedule,
               v_modified_by, current_user, 'system.external_data_import', v_commentary
        from ores_dq_script_coarsenings_artefact_tbl a
        where a.dataset_id = p_dataset_id and a.script_name = v_name;

        insert into ores_trading_script_inputs_tbl (
            script_name, sequence_number, tenant_id, version, input_name, data_type, is_array,
            value_rule, description,
            modified_by, performed_by, change_reason_code, change_commentary)
        select a.script_name, a.sequence_number, p_target_tenant_id, 0, a.input_name,
               a.data_type, a.is_array, a.value_rule, a.description,
               v_modified_by, current_user, 'system.external_data_import', v_commentary
        from ores_dq_script_inputs_artefact_tbl a
        where a.dataset_id = p_dataset_id and a.script_name = v_name;

        if v_exists then
            v_updated := v_updated + 1;
        else
            v_inserted := v_inserted + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted where v_inserted > 0
    union all select 'updated'::text, v_updated where v_updated > 0
    union all select 'skipped'::text, v_skipped where v_skipped > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

comment on function ores_trading_publish_script_library_from_dq_fn(uuid, uuid, text, jsonb) is
'Publishes the script library a dataset stages into the target tenant, one script at a
 time: a script whose staged rows equal the published ones is skipped, any other is
 written whole as a new version and its rows no longer staged are closed. Counts are of
 scripts. In insert_only mode a script the target already holds is skipped whatever its
 staged rows. A published script the dataset no longer stages stays live: a booked
 trade may name it, so it is retired deliberately rather than by a republish.';
