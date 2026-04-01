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
 * Report Definitions Publication Function
 *
 * Copies report definition artefact data into the reporting table for a target
 * tenant. Each artefact row becomes a report definition scoped to the calling
 * party. The party is resolved from p_params->>'party_id' or by querying the
 * tenant's operational root party.
 *
 * Idempotent: skips if the party already has report definitions for this dataset.
 *
 * @param p_dataset_id       The DQ dataset containing report definition artefacts
 * @param p_target_tenant_id The tenant to publish data to
 * @param p_mode             Population mode (only 'upsert' supported)
 * @param p_params           Optional: {"party_id": "<uuid>"} to override root party
 */

-- =============================================================================
-- Report Definitions Publication
-- =============================================================================

create or replace function ores_dq_report_definitions_publish_fn(
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
begin
    -- Resolve root party: explicit param or query tenant's operational root.
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

    -- Idempotency: skip if this party already has report definitions
    if exists (
        select 1 from ores_reporting_report_definitions_tbl
        where tenant_id = p_target_tenant_id
          and party_id = v_root_party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return query select 'skipped'::text, 0::bigint;
        return;
    end if;

    -- Bulk insert all report definitions for this party
    insert into ores_reporting_report_definitions_tbl (
        tenant_id, id, version, party_id, name,
        description, report_type, schedule_expression, concurrency_policy,
        fsm_state_id, scheduler_job_id,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        p_target_tenant_id,
        gen_random_uuid(), 0, v_root_party_id, a.name,
        coalesce(a.description, ''), a.report_type, a.schedule_expression, a.concurrency_policy,
        null, null,
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.external_data_import', 'Published from ore_analytics dataset'
    from ores_dq_report_definitions_artefact_tbl a
    where a.dataset_id = p_dataset_id
    order by a.display_order, a.name;

    get diagnostics v_inserted = row_count;

    -- Return summary
    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0;
end;
$$ language plpgsql security definer;
