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
 * Retained Stores Artefact Population Script
 *
 * Stages the stores the installation keeps, and the bound each one ships with.
 * The default periods follow the data: a quarter for the run output, a quarter
 * for quotes, a month for delivery counters, a week for job inputs, a day for
 * scratch. Two stores take no default. The engine package cache is bounded by
 * its content keying, so a policy would bound nothing. The calendar dates are
 * bounded by nothing a period can express: a policy bounds by age, and a day
 * behind the horizon is out of range rather than old, so that store needs a
 * delete by range before it can be bounded at all.
 */

\echo '--- Retained Stores Artefacts ---'

select id as v_dataset_id from ores_dq_datasets_tbl
where code = 'scheduler.retained_stores'
  and valid_to = ores_utility_infinity_timestamp_fn() \gset

delete from ores_dq_retained_stores_artefact_tbl
where dataset_id = :'v_dataset_id';

insert into ores_dq_retained_stores_artefact_tbl (
    tenant_id, dataset_id, code, version, display_name, description,
    owner_component, scope, target_kind, target, time_column, measure_kind,
    bound_kind, default_action, default_retention_days, default_cadence_hours,
    default_reason
)
values
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'refdata.calendar_dates', 0,
     'Calendar dates',
     'The business days of every calendar, one row per calendar per day, derived from the calendar''s rules and its exceptions. The installation publishes them once and every tenant reads the same copy.',
     'refdata', 'platform', 'chunked_table', 'public.ores_refdata_calendar_dates_tbl',
     'valid_from', 'relation_rows', 'none', null, null, 24, null),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'reporting.report_cashflows', 0,
     'Report run cashflows',
     'The cashflow rows each report run writes: the projected flows of every trade the run priced, one row per flow per measure. Kept so a run can be read again without pricing it again.',
     'reporting', 'platform', 'chunked_table', 'public.ores_reporting_report_cashflows_tbl',
     'valid_from', 'relation_rows', 'policy', 'drop_chunks', 90, 24,
     'A run''s cashflows, kept for the quarter its figures are compared over'),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'reporting.report_metrics', 0,
     'Report run metrics',
     'The summary figures each report run writes beside its cashflows, one row per metric per run. Kept under the same bound as the cashflows, because the two are read together.',
     'reporting', 'platform', 'chunked_table', 'public.ores_reporting_report_metrics_tbl',
     'valid_from', 'relation_rows', 'policy', 'drop_chunks', 90, 24,
     'A run''s metrics, kept for the quarter its figures are compared over'),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'reporting.report_text_values', 0,
     'Report run text values',
     'The text each report run writes: the headings and the labels its figures carry, one row per value per run.',
     'reporting', 'platform', 'chunked_table', 'public.ores_reporting_report_text_values_tbl',
     'valid_from', 'relation_rows', 'policy', 'drop_chunks', 90, 24,
     'A run''s text values, kept for the quarter its figures are compared over'),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'marketdata.ticks', 0,
     'Market data ticks',
     'The quotes every feed delivered, as they arrived, compressed by the store. The raw record a curve is built from, kept longer than anything derived from it.',
     'marketdata', 'platform', 'chunked_table', 'public.ores_marketdata_ticks_tbl',
     'ts', 'relation_rows', 'policy', 'drop_chunks', 90, 24,
     'A curve is rebuilt from its quotes within a quarter; an older quote is superseded'),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'marketdata.delivery_counts', 0,
     'Market data delivery counts',
     'How many items each feed delivered for each series in each period: one row per series per period, carried forward on every flush rather than appended.',
     'marketdata', 'platform', 'table', 'public.ores_marketdata_delivery_counts_tbl',
     'period_start', 'relation_rows', 'policy', 'delete_rows', 30, 24,
     'A delivery counter is read for a month; an older row is read only during an incident'),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'compute.job_inputs', 0,
     'Compute job inputs',
     'The input archives a report run hands to the grid, one per work unit: the trade set, the configuration and the market data the engine reads.',
     'compute', 'platform', 'object_prefix', 'compute/input/',
     null, 'objects', 'policy', 'expire_prefix', 7, 24,
     'An input archive is the run''s own copy of what it priced, and the run keeps its output, so a week is enough'),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'compute.job_outputs', 0,
     'Compute job outputs',
     'The result archives the grid writes back, one per work unit: the engine''s own output files and its log, packed together.',
     'compute', 'platform', 'object_prefix', 'compute/output/',
     null, 'objects', 'policy', 'expire_prefix', 90, 24,
     'Run output is kept as long as the database rows of the same run, which is a quarter'),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'compute.node_scratch', 0,
     'Compute node scratch',
     'The working folder each grid node keeps while it runs a job: the unpacked input, the engine''s log and the output, until the job''s result is uploaded.',
     'compute', 'platform', 'directory', 'publish/run/wrappers',
     null, 'files', 'policy', 'release_directory', 1, 24,
     'A job''s folder is scratch once its output is uploaded; a day covers a job that is still running'),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'compute.package_cache', 0,
     'Engine package cache',
     'The unpacked engine packages a host shares between its nodes, named by the package''s digest so two versions holding the same bytes are one copy. Bounded by that keying rather than by a policy.',
     'compute', 'platform', 'directory', 'publish/run/packages',
     null, 'files', 'keying', null, null, 24, null);
