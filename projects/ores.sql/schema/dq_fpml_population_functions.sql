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
 * FPML account_types Population Functions
 *
 * Functions to publish account_types from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_account_type_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores.dq_populate_account_types(dataset_id, 'upsert');
 */

set schema 'ores';

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what account_types would be copied from a DQ dataset.
 */
create or replace function ores.dq_preview_account_type_population(p_dataset_id uuid)
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
    from ores.dq_account_types_artefact_tbl dq
    left join ores.refdata_account_types_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate refdata_account_types_tbl from a DQ account_types dataset.
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Population mode: 'upsert', 'insert_only', or 'replace_all'.
 */
create or replace function ores.dq_populate_account_types(
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
    from ores.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores.refdata_account_types_tbl
        set valid_to = current_timestamp
        where valid_to = ores.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores.dq_account_types_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores.refdata_account_types_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores.refdata_account_types_tbl (
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
 * FPML asset_classes Population Functions
 *
 * Functions to publish asset_classes from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_asset_class_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores.dq_populate_asset_classes(dataset_id, 'upsert');
 */

set schema 'ores';

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what asset_classes would be copied from a DQ dataset.
 */
create or replace function ores.dq_preview_asset_class_population(p_dataset_id uuid)
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
    from ores.dq_asset_classes_artefact_tbl dq
    left join ores.refdata_asset_classes_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate refdata_asset_classes_tbl from a DQ asset_classes dataset.
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Population mode: 'upsert', 'insert_only', or 'replace_all'.
 */
create or replace function ores.dq_populate_asset_classes(
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
    from ores.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores.refdata_asset_classes_tbl
        set valid_to = current_timestamp
        where valid_to = ores.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores.dq_asset_classes_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores.refdata_asset_classes_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores.refdata_asset_classes_tbl (
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
 * FPML asset_measures Population Functions
 *
 * Functions to publish asset_measures from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_asset_measure_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores.dq_populate_asset_measures(dataset_id, 'upsert');
 */

set schema 'ores';

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what asset_measures would be copied from a DQ dataset.
 */
create or replace function ores.dq_preview_asset_measure_population(p_dataset_id uuid)
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
    from ores.dq_asset_measures_artefact_tbl dq
    left join ores.refdata_asset_measures_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate refdata_asset_measures_tbl from a DQ asset_measures dataset.
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Population mode: 'upsert', 'insert_only', or 'replace_all'.
 */
create or replace function ores.dq_populate_asset_measures(
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
    from ores.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores.refdata_asset_measures_tbl
        set valid_to = current_timestamp
        where valid_to = ores.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores.dq_asset_measures_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores.refdata_asset_measures_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores.refdata_asset_measures_tbl (
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
 * FPML benchmark_rates Population Functions
 *
 * Functions to publish benchmark_rates from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_benchmark_rate_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores.dq_populate_benchmark_rates(dataset_id, 'upsert');
 */

set schema 'ores';

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what benchmark_rates would be copied from a DQ dataset.
 */
create or replace function ores.dq_preview_benchmark_rate_population(p_dataset_id uuid)
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
    from ores.dq_benchmark_rates_artefact_tbl dq
    left join ores.refdata_benchmark_rates_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate refdata_benchmark_rates_tbl from a DQ benchmark_rates dataset.
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Population mode: 'upsert', 'insert_only', or 'replace_all'.
 */
create or replace function ores.dq_populate_benchmark_rates(
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
    from ores.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores.refdata_benchmark_rates_tbl
        set valid_to = current_timestamp
        where valid_to = ores.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores.dq_benchmark_rates_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores.refdata_benchmark_rates_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores.refdata_benchmark_rates_tbl (
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
 * FPML business_centres Population Functions
 *
 * Functions to publish business_centres from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_business_centre_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores.dq_populate_business_centres(dataset_id, 'upsert');
 */

set schema 'ores';

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what business_centres would be copied from a DQ dataset.
 */
create or replace function ores.dq_preview_business_centre_population(p_dataset_id uuid)
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
    from ores.dq_business_centres_artefact_tbl dq
    left join ores.refdata_business_centres_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate refdata_business_centres_tbl from a DQ business_centres dataset.
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Population mode: 'upsert', 'insert_only', or 'replace_all'.
 */
create or replace function ores.dq_populate_business_centres(
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
    from ores.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores.refdata_business_centres_tbl
        set valid_to = current_timestamp
        where valid_to = ores.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores.dq_business_centres_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores.refdata_business_centres_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores.refdata_business_centres_tbl (
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
 * FPML business_processes Population Functions
 *
 * Functions to publish business_processes from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_business_process_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores.dq_populate_business_processes(dataset_id, 'upsert');
 */

set schema 'ores';

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what business_processes would be copied from a DQ dataset.
 */
create or replace function ores.dq_preview_business_process_population(p_dataset_id uuid)
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
    from ores.dq_business_processes_artefact_tbl dq
    left join ores.refdata_business_processes_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate refdata_business_processes_tbl from a DQ business_processes dataset.
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Population mode: 'upsert', 'insert_only', or 'replace_all'.
 */
create or replace function ores.dq_populate_business_processes(
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
    from ores.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores.refdata_business_processes_tbl
        set valid_to = current_timestamp
        where valid_to = ores.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores.dq_business_processes_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores.refdata_business_processes_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores.refdata_business_processes_tbl (
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
 * FPML cashflow_types Population Functions
 *
 * Functions to publish cashflow_types from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_cashflow_type_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores.dq_populate_cashflow_types(dataset_id, 'upsert');
 */

set schema 'ores';

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what cashflow_types would be copied from a DQ dataset.
 */
create or replace function ores.dq_preview_cashflow_type_population(p_dataset_id uuid)
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
    from ores.dq_cashflow_types_artefact_tbl dq
    left join ores.refdata_cashflow_types_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate refdata_cashflow_types_tbl from a DQ cashflow_types dataset.
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Population mode: 'upsert', 'insert_only', or 'replace_all'.
 */
create or replace function ores.dq_populate_cashflow_types(
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
    from ores.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores.refdata_cashflow_types_tbl
        set valid_to = current_timestamp
        where valid_to = ores.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores.dq_cashflow_types_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores.refdata_cashflow_types_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores.refdata_cashflow_types_tbl (
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
 * FPML entity_classifications Population Functions
 *
 * Functions to publish entity_classifications from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_entity_classification_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores.dq_populate_entity_classifications(dataset_id, 'upsert');
 */

set schema 'ores';

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what entity_classifications would be copied from a DQ dataset.
 */
create or replace function ores.dq_preview_entity_classification_population(p_dataset_id uuid)
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
    from ores.dq_entity_classifications_artefact_tbl dq
    left join ores.refdata_entity_classifications_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate refdata_entity_classifications_tbl from a DQ entity_classifications dataset.
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Population mode: 'upsert', 'insert_only', or 'replace_all'.
 */
create or replace function ores.dq_populate_entity_classifications(
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
    from ores.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores.refdata_entity_classifications_tbl
        set valid_to = current_timestamp
        where valid_to = ores.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores.dq_entity_classifications_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores.refdata_entity_classifications_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores.refdata_entity_classifications_tbl (
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
 * FPML local_jurisdictions Population Functions
 *
 * Functions to publish local_jurisdictions from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_local_jurisdiction_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores.dq_populate_local_jurisdictions(dataset_id, 'upsert');
 */

set schema 'ores';

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what local_jurisdictions would be copied from a DQ dataset.
 */
create or replace function ores.dq_preview_local_jurisdiction_population(p_dataset_id uuid)
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
    from ores.dq_local_jurisdictions_artefact_tbl dq
    left join ores.refdata_local_jurisdictions_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate refdata_local_jurisdictions_tbl from a DQ local_jurisdictions dataset.
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Population mode: 'upsert', 'insert_only', or 'replace_all'.
 */
create or replace function ores.dq_populate_local_jurisdictions(
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
    from ores.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores.refdata_local_jurisdictions_tbl
        set valid_to = current_timestamp
        where valid_to = ores.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores.dq_local_jurisdictions_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores.refdata_local_jurisdictions_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores.refdata_local_jurisdictions_tbl (
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
 * FPML party_relationships Population Functions
 *
 * Functions to publish party_relationships from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_party_relationship_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores.dq_populate_party_relationships(dataset_id, 'upsert');
 */

set schema 'ores';

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what party_relationships would be copied from a DQ dataset.
 */
create or replace function ores.dq_preview_party_relationship_population(p_dataset_id uuid)
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
    from ores.dq_party_relationships_artefact_tbl dq
    left join ores.refdata_party_relationships_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate refdata_party_relationships_tbl from a DQ party_relationships dataset.
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Population mode: 'upsert', 'insert_only', or 'replace_all'.
 */
create or replace function ores.dq_populate_party_relationships(
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
    from ores.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores.refdata_party_relationships_tbl
        set valid_to = current_timestamp
        where valid_to = ores.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores.dq_party_relationships_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores.refdata_party_relationships_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores.refdata_party_relationships_tbl (
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
 * FPML party_roles Population Functions
 *
 * Functions to publish party_roles from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_party_role_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores.dq_populate_party_roles(dataset_id, 'upsert');
 */

set schema 'ores';

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what party_roles would be copied from a DQ dataset.
 */
create or replace function ores.dq_preview_party_role_population(p_dataset_id uuid)
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
    from ores.dq_party_roles_artefact_tbl dq
    left join ores.refdata_party_roles_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate refdata_party_roles_tbl from a DQ party_roles dataset.
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Population mode: 'upsert', 'insert_only', or 'replace_all'.
 */
create or replace function ores.dq_populate_party_roles(
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
    from ores.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores.refdata_party_roles_tbl
        set valid_to = current_timestamp
        where valid_to = ores.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores.dq_party_roles_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores.refdata_party_roles_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores.refdata_party_roles_tbl (
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
 * FPML person_roles Population Functions
 *
 * Functions to publish person_roles from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_person_role_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores.dq_populate_person_roles(dataset_id, 'upsert');
 */

set schema 'ores';

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what person_roles would be copied from a DQ dataset.
 */
create or replace function ores.dq_preview_person_role_population(p_dataset_id uuid)
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
    from ores.dq_person_roles_artefact_tbl dq
    left join ores.refdata_person_roles_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate refdata_person_roles_tbl from a DQ person_roles dataset.
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Population mode: 'upsert', 'insert_only', or 'replace_all'.
 */
create or replace function ores.dq_populate_person_roles(
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
    from ores.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores.refdata_person_roles_tbl
        set valid_to = current_timestamp
        where valid_to = ores.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores.dq_person_roles_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores.refdata_person_roles_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores.refdata_person_roles_tbl (
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
 * FPML regulatory_corporate_sectors Population Functions
 *
 * Functions to publish regulatory_corporate_sectors from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_regulatory_corporate_sector_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores.dq_populate_regulatory_corporate_sectors(dataset_id, 'upsert');
 */

set schema 'ores';

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what regulatory_corporate_sectors would be copied from a DQ dataset.
 */
create or replace function ores.dq_preview_regulatory_corporate_sector_population(p_dataset_id uuid)
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
    from ores.dq_regulatory_corporate_sectors_artefact_tbl dq
    left join ores.refdata_regulatory_corporate_sectors_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate refdata_regulatory_corporate_sectors_tbl from a DQ regulatory_corporate_sectors dataset.
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Population mode: 'upsert', 'insert_only', or 'replace_all'.
 */
create or replace function ores.dq_populate_regulatory_corporate_sectors(
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
    from ores.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores.refdata_regulatory_corporate_sectors_tbl
        set valid_to = current_timestamp
        where valid_to = ores.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores.dq_regulatory_corporate_sectors_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores.refdata_regulatory_corporate_sectors_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores.refdata_regulatory_corporate_sectors_tbl (
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
 * FPML reporting_regimes Population Functions
 *
 * Functions to publish reporting_regimes from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_reporting_regime_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores.dq_populate_reporting_regimes(dataset_id, 'upsert');
 */

set schema 'ores';

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what reporting_regimes would be copied from a DQ dataset.
 */
create or replace function ores.dq_preview_reporting_regime_population(p_dataset_id uuid)
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
    from ores.dq_reporting_regimes_artefact_tbl dq
    left join ores.refdata_reporting_regimes_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores.utility_infinity_timestamp_fn()
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
create or replace function ores.dq_populate_reporting_regimes(
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
    from ores.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores.refdata_reporting_regimes_tbl
        set valid_to = current_timestamp
        where valid_to = ores.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores.dq_reporting_regimes_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores.refdata_reporting_regimes_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores.refdata_reporting_regimes_tbl (
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
 * FPML supervisory_bodies Population Functions
 *
 * Functions to publish supervisory_bodies from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_supervisory_body_population(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores.dq_populate_supervisory_bodies(dataset_id, 'upsert');
 */

set schema 'ores';

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what supervisory_bodies would be copied from a DQ dataset.
 */
create or replace function ores.dq_preview_supervisory_body_population(p_dataset_id uuid)
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
    from ores.dq_supervisory_bodies_artefact_tbl dq
    left join ores.refdata_supervisory_bodies_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate refdata_supervisory_bodies_tbl from a DQ supervisory_bodies dataset.
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Population mode: 'upsert', 'insert_only', or 'replace_all'.
 */
create or replace function ores.dq_populate_supervisory_bodies(
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
    from ores.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update ores.refdata_supervisory_bodies_tbl
        set valid_to = current_timestamp
        where valid_to = ores.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each record from DQ dataset
    for r in
        select
            dq.code,
            dq.coding_scheme_code,
            dq.source,
            dq.description
        from ores.dq_supervisory_bodies_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists
        select exists (
            select 1 from ores.refdata_supervisory_bodies_tbl existing
            where existing.code = r.code
              and existing.coding_scheme_code = r.coding_scheme_code
              and existing.valid_to = ores.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert record - trigger handles versioning automatically
        insert into ores.refdata_supervisory_bodies_tbl (
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
