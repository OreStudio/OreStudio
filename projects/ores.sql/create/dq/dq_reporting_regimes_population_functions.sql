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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_populate_function_refdata.mustache
 * To modify, update the template and regenerate.
 *
 * FPML reporting_regimes Population Functions
 *
 * Functions to publish reporting_regimes from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores_dq_preview_reporting_regime_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores_dq_populate_reporting_regimes(dataset_id, 'upsert');
 */


-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what reporting_regimes would be copied from a DQ dataset.
 */
create or replace function ores_dq_preview_reporting_regime_population(p_dataset_id uuid)
returns table (
    action text,
    code text,
    coding_scheme_code text,
    description text,
    reason text
) as $$
begin
    return query
    select
        case
            when existing.code is not null then 'update'
            else 'insert'
        end as action,
        dq.code,
        dq.coding_scheme_code,
        dq.description,
        case
            when existing.code is not null then 'Record already exists'
            else 'New record'
        end as reason
    from ores_dq_reporting_regimes_artefact_tbl dq
    left join ores_refdata_reporting_regimes_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores_utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate refdata_reporting_regimes_tbl from a DQ reporting_regimes dataset.
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Population mode: 'upsert', 'insert_only', or 'replace_all'.
 */
create or replace function ores_dq_populate_reporting_regimes(
    p_dataset_id uuid,
    p_mode text default 'upsert'
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
    -- Validate dataset exists
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores_refdata_reporting_regimes_tbl
        set valid_to = current_timestamp
        where valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores_dq_reporting_regimes_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores_refdata_reporting_regimes_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores_refdata_reporting_regimes_tbl (
            code, version, coding_scheme_code, source, description,
            modified_by, change_reason_code, change_commentary
        ) values (
            r.code, 0, r.coding_scheme_code, r.source, r.description,
            'data_importer', 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        -- Track whether this was an insert (version=1) or update (version>1)
        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    -- Return summary
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
$$ language plpgsql;
