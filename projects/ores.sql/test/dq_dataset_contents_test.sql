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
 * pgTAP tests for reading what a dataset has staged.
 *
 * A dataset names one artefact type, and the type names a table or a family of
 * them, so the answer is the family's tables and never a single row. The facts
 * asserted here are:
 *
 * - the tables read lists every table its artefact type names, and no other;
 * - each table states its own columns, without the two columns every family
 *   shares;
 * - a page states the columns it carries, and no served row carries the dataset,
 *   the tenant or the version;
 * - the page's order is the table's own, so a known row is the row a person sees,
 *   and paging cannot repeat or skip one;
 * - a table outside the dataset's own family is refused, and so is a dataset no
 *   artefact type names.
 *
 * Run with: pg_prove -d <database> test/dq_dataset_contents_test.sql
 */

begin;

select plan(10);

create temporary view t_contents as
select * from ores_dq_dataset_tables_fn('refdata.conventions');

create temporary view t_family_size as
select array_length(string_to_array(at.artefact_table, ','), 1) as tables
from ores_dq_datasets_tbl d
join ores_dq_artefact_types_tbl at on at.code = d.artefact_type
where d.code = 'refdata.conventions'
  and d.valid_to = ores_utility_infinity_timestamp_fn();

select ok(
    (select count(*) from t_contents) > 0,
    'the tables read answers the family a dataset stages into');

select is(
    (select count(*) from t_contents),
    (select tables::bigint from t_family_size),
    'every table its artefact type names is listed, and no other');

select is(
    (select count(*) from t_contents where table_name not like 'ores\_%'),
    0::bigint,
    'each table is named by the name the store states');

select is(
    (select count(*) from t_contents
     where columns like '%dataset_id%' or columns like '%tenant_id%'
        or columns like '%version%'),
    0::bigint,
    'the columns every family shares are not stated');

select ok(
    ores_dq_dataset_rows_fn(
        'refdata.conventions', (select min(table_name) from t_contents), 5, 0) ? 'columns',
    'a page states the columns its rows carry');

select is(
    (select count(*)
     from jsonb_array_elements(
              ores_dq_dataset_rows_fn(
                  'refdata.conventions', (select min(table_name) from t_contents), 5, 0)
              -> 'rows') as r
     where r ? 'dataset_id' or r ? 'tenant_id'),
    0::bigint,
    'a served row carries neither the dataset nor the tenant');

select throws_ok(
    $$select ores_dq_dataset_rows_fn(
        'refdata.conventions', 'ores_dq_currencies_artefact_tbl', 5, 0)$$,
    'P0001',
    NULL,
    'a table outside the dataset family is refused');

select is(
    (select count(*)
     from jsonb_array_elements(
              ores_dq_dataset_rows_fn(
                  'refdata.conventions', (select min(table_name) from t_contents), 1000, 0)
              -> 'rows') as r
     where r ? 'version'),
    0::bigint,
    'a served row carries no version');

-- The page's first row is the table's first row: ctid is the order, and without
-- one LIMIT and OFFSET could repeat a row on one page and skip it on the next.
select is(
    (ores_dq_dataset_rows_fn(
         'refdata.conventions',
         'ores_dq_conventions_artefact_tbl', 1, 0) -> 'rows' -> 0 ->> 'kind_code'),
    (select kind_code from ores_dq_conventions_artefact_tbl order by ctid limit 1),
    'the first row of the page is the first row of the table');

select is(
    (select count(*) from ores_dq_dataset_tables_fn('no.such.dataset')),
    0::bigint,
    'a dataset no artefact type names stages into no table');

select * from finish();

rollback;
