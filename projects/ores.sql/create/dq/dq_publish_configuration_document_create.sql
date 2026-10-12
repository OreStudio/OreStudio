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
 * Publication of a report configuration document
 *
 * A configuration document is a header row and the child rows that reference it
 * by id: a pricing engines document, a today's market document, a curve
 * configuration. A dataset stages the document in one artefact table per live
 * table. These functions copy it into the live tables for the party a publish
 * names.
 *
 * Ids are re-keyed per party. Every party gets the same staged document, and a
 * live id is unique within a tenant, so a staged id is mapped to a live id that
 * depends on the party. The map is a function of the staged id and the party and
 * nothing else, so every row of the document maps consistently, a foreign key
 * between two rows of the document needs no lookup, and a republish finds the
 * rows it wrote and skips them.
 */

-- The live id of a staged id for one party.
create or replace function ores_dq_party_uuid_fn(
    p_party_id uuid,
    p_staged_id uuid
)
returns uuid as $$
    select md5(p_party_id::text || ':' || p_staged_id::text)::uuid;
$$ language sql immutable strict;

comment on function ores_dq_party_uuid_fn(uuid, uuid) is
'The live id that a staged id has for one party. The same staged id gives the same
 live id for a party and a different one for another party.';

/**
 * Publishes a configuration document from its staging tables.
 *
 * The first table of p_tables is the header. Its staged rows carry the id of the
 * reporting configuration they stand for in configuration_id, and the function
 * writes that reporting configuration, named after the header, before it writes
 * any row of the document. Every uuid column of every table is re-keyed per
 * party. A row whose live id exists for the party is skipped, so a republish
 * converges on the same set.
 *
 * @param p_dataset_id    the dataset that stages the document.
 * @param p_tenant_id     the tenant that receives it.
 * @param p_party_id      the party that receives it.
 * @param p_type_code     the configuration type the document is, for example
 *                        pricing_engines.
 * @param p_component     the component that owns the live tables.
 * @param p_live_prefix   the prefix of the live table names, for example
 *                        ores_analytics_.
 * @param p_tables        the base table names, header first, children after in
 *                        the order they are written.
 * @param p_external      the uuid columns that refer to something outside the
 *                        document. They are copied as staged and not re-keyed,
 *                        because a re-keyed value would point at nothing. Every
 *                        other uuid column of every table is a key of the document.
 *
 * A publish only inserts. The mode a caller passes has no effect, and a staged row
 * whose live id exists is left as it is, so a changed document needs new ids.
 */
create or replace function ores_dq_publish_configuration_document_fn(
    p_dataset_id uuid,
    p_tenant_id uuid,
    p_party_id uuid,
    p_type_code text,
    p_component text,
    p_live_prefix text,
    p_tables text[],
    p_external text[] default array[]::text[]
)
returns table (action text, record_count bigint) as $$
declare
    v_dataset_name text;
    v_commentary text;
    v_header text := p_tables[1];
    v_header_staging text;
    v_table text;
    v_staging text;
    v_live text;
    v_cols text;
    v_exprs text;
    v_staged bigint := 0;
    v_n bigint;
    v_inserted bigint := 0;
begin
    perform ores_utility_allow_version_replace_fn();

    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    v_commentary := 'Imported from DQ dataset: ' || v_dataset_name;
    v_header_staging := format('ores_dq_%s_artefact_tbl', v_header);

    -- A header with no configuration would publish a document that no definition can
    -- bind, so a dataset that stages one is refused.
    execute format('select count(*) from %I where dataset_id = $1 and configuration_id is null',
                   v_header_staging)
        into v_n using p_dataset_id;
    if v_n > 0 then
        raise exception 'Dataset % stages % header(s) of % with no configuration_id.',
            v_dataset_name, v_n, p_type_code;
    end if;

    -- The reporting configuration each staged header stands for.
    execute format(
        'insert into ores_reporting_configurations_tbl (tenant_id, id, version, party_id, '
            'name, configuration_type_code, owning_component, modified_by, performed_by, '
            'change_reason_code, change_commentary) '
        'select %1$L::uuid, ores_dq_party_uuid_fn(%2$L::uuid, a.configuration_id), 0, %2$L::uuid, '
            'a.name, %3$L, %4$L, coalesce(ores_iam_current_service_fn(), current_user), '
            'current_user, ''system.external_data_import'', %5$L '
        'from %6$I a '
        'where a.dataset_id = %7$L::uuid and a.configuration_id is not null '
          'and not exists (select 1 from ores_reporting_configurations_tbl o '
              'where o.tenant_id = %1$L::uuid and o.party_id = %2$L::uuid '
                'and o.id = ores_dq_party_uuid_fn(%2$L::uuid, a.configuration_id) '
                'and o.valid_to = ores_utility_infinity_timestamp_fn())',
        p_tenant_id::text, p_party_id::text, p_type_code, p_component, v_commentary,
        v_header_staging, p_dataset_id::text);

    foreach v_table in array p_tables loop
        v_staging := format('ores_dq_%s_artefact_tbl', v_table);
        v_live := format('%s%s_tbl', p_live_prefix, v_table);

        execute format('select count(*) from %I where dataset_id = $1', v_staging)
            into v_n using p_dataset_id;
        v_staged := v_staged + v_n;

        -- The live table's own data columns, in table order, less the columns
        -- this function stamps. A uuid column is re-keyed, and any other column
        -- is copied as staged.
        select string_agg(format('%I', c.column_name), ', ' order by c.ordinal_position),
               string_agg(case when c.data_type = 'uuid' and c.column_name <> all (p_external)
                    then format('ores_dq_party_uuid_fn(%L::uuid, a.%I)', p_party_id::text,
                                c.column_name)
                    else format('a.%I', c.column_name) end,
                    ', ' order by c.ordinal_position)
        into v_cols, v_exprs
        from information_schema.columns c
        where c.table_schema = 'public'
          and c.table_name = v_live
          and c.column_name not in ('tenant_id', 'version', 'party_id', 'modified_by',
              'performed_by', 'change_reason_code', 'change_commentary',
              'valid_from', 'valid_to');

        if v_cols is null then
            raise exception 'Live table not found: %', v_live;
        end if;

        execute format(
            'insert into %1$I (tenant_id, version, party_id, %2$s, modified_by, performed_by, '
                'change_reason_code, change_commentary) '
            'select %3$L::uuid, 0, %4$L::uuid, %5$s, '
                'coalesce(ores_iam_current_service_fn(), current_user), current_user, '
                '''system.external_data_import'', %6$L '
            'from %7$I a '
            'where a.dataset_id = %8$L::uuid '
              'and not exists (select 1 from %1$I o '
                  'where o.tenant_id = %3$L::uuid and o.party_id = %4$L::uuid '
                    'and o.id = ores_dq_party_uuid_fn(%4$L::uuid, a.id) '
                    'and o.valid_to = ores_utility_infinity_timestamp_fn())',
            v_live, v_cols, p_tenant_id::text, p_party_id::text, v_exprs, v_commentary,
            v_staging, p_dataset_id::text);

        get diagnostics v_n = row_count;
        v_inserted := v_inserted + v_n;
    end loop;

    return query
    select 'inserted'::text, v_inserted where v_inserted > 0
    union all select 'skipped'::text, v_staged - v_inserted where v_staged - v_inserted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

comment on function ores_dq_publish_configuration_document_fn(uuid, uuid, uuid, text, text, text, text[], text[]) is
'Copies a configuration document from its staging tables into the live tables of
 its owning component for one party, with its reporting configuration, re-keying
 every uuid per party. Idempotent per row.';
