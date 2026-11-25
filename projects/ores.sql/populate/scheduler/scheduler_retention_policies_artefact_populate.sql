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
 * Retention Policies Artefact Population Script
 *
 * Stages the policies of the run output tables. Each states the period its
 * model declares, so the sweep and the engine's own retention job drop the same
 * chunks: ninety days, the span of the comparisons a run's figures serve. A
 * run's document and the cut it priced from are what outlive them.
 */

\echo '--- Retention Policies Artefacts ---'

select id as v_dataset_id from ores_dq_datasets_tbl
where code = 'scheduler.retention_policies'
  and valid_to = ores_utility_infinity_timestamp_fn() \gset

delete from ores_dq_retention_policies_artefact_tbl
where dataset_id = :'v_dataset_id';

insert into ores_dq_retention_policies_artefact_tbl (
    tenant_id, dataset_id, code, version, target_kind, target, action,
    retention_days, cadence_hours, enabled, description
)
values
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'report_cashflows_90d', 0,
     'chunked_table', 'public.ores_reporting_report_cashflows_tbl', 'drop_chunks',
     90, 24, true,
     'A run''s cashflows, kept for the quarter its figures are compared over'),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'report_metrics_90d', 0,
     'chunked_table', 'public.ores_reporting_report_metrics_tbl', 'drop_chunks',
     90, 24, true,
     'A run''s metrics, kept for the quarter its figures are compared over'),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'report_text_values_90d', 0,
     'chunked_table', 'public.ores_reporting_report_text_values_tbl', 'drop_chunks',
     90, 24, true,
     'A run''s text values, kept for the quarter its figures are compared over');
