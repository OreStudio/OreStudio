/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 */

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate ores_dq_badge_severities_tbl from a DQ badge severities dataset.
 *
 * Uses SECURITY DEFINER to bypass RLS: it reads artefacts staged from the
 * system tenant and writes into the tenant specified by p_target_tenant_id.
 *
 * @param p_dataset_id       The DQ dataset to populate from
 * @param p_target_tenant_id The tenant to publish data to
 * @param p_mode             Population mode: 'upsert', 'insert_only', or 'replace_all'
 */
create or replace function ores_dq_badge_severities_publish_fn(
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
        update ores_dq_badge_severities_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and change_commentary = 'Imported from DQ dataset: ' || v_dataset_name
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.name,
            dq.description,
            dq.display_order
        from ores_dq_badge_severities_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_dq_badge_severities_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.code = r.code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_dq_badge_severities_tbl (
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
$$ language plpgsql security definer;
