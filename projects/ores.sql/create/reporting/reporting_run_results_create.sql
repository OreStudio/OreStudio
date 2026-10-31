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
 * Template: sql_schema_domain_entity_view_create.mustache
 * To modify, update the template and regenerate.
 *
 * Run Result View
 *
 * The read model the result screen reads: one figure of a run, at the trade it is
 * about, named by the measure the screen shows. It is a view over
 * ores_reporting_report_metrics_tbl, so the figures a run produced are stored
 * once, in the shape the readers write, and the screen's shape is a projection of
 * them.
 *
 * The view is where the engine's headings meet the screen's words, and it is the
 * only place that knows either. The engine names the figure NPV, NPV(Base) and
 * Notional; the screen asks for npv, npv_base and notional. A screen that
 * learned the headings would break the day a configuration renamed a column, and a
 * screen that joined the tables itself would state the same rollup twice.
 */

create or replace view "ores_reporting_run_results_vw"
with (security_invoker = true) as
select m.tenant_id,
       m.id,
       m.party_id,
       m.report_instance_id,
       m.report_definition_id,
       m.as_of,
       m.subject_type,
       m.subject_id,
       case m.measure
           when 'NPV' then 'npv'
           when 'NPV(Base)' then 'npv_base'
           when 'Notional' then 'notional'
           else m.measure
       end as measure,
       m.currency,
       case
           when (m.qualifiers::jsonb ->> 'CounterParty') ~
                '^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$'
           then (m.qualifiers::jsonb ->> 'CounterParty')::uuid
           else null
       end as counterparty_id,
       m.value as figure,
       m.source
  from ores_reporting_report_metrics_tbl m
 where m.family = 'npv'
   and m.valid_to = ores_utility_infinity_timestamp_fn();
