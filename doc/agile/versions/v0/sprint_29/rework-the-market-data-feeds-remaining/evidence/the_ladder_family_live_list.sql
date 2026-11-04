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
 * The ladder family as the environment holds it, written to
 * the_ladder_family_live_list.txt beside this file.
 *
 * Read-only: every statement here selects. Run from the repository root:
 *
 *   ./compass.sh sql -- -v ON_ERROR_STOP=1 \
 *       -f doc/agile/versions/v0/sprint_29/rework-the-market-data-feeds-remaining/evidence/the_ladder_family_live_list.sql
 */

\set ON_ERROR_STOP on

\o doc/agile/versions/v0/sprint_29/rework-the-market-data-feeds-remaining/evidence/the_ladder_family_live_list.txt

\echo '--- The ladder family, by party and cut ---'

select p.short_code as party, d.cut_kind_code as cut, count(*) as definitions
from ores_reporting_report_definitions_tbl d
join ores_refdata_parties_tbl p
  on p.id = d.party_id and p.valid_to = ores_utility_infinity_timestamp_fn()
where d.valid_to = ores_utility_infinity_timestamp_fn()
  and d.name in ('Bump and Reprice Rho Ladder', 'Bump and Reprice Rho Ladder (EOD)',
                 'Bump and Reprice Gamma Ladder', 'Bump and Reprice Theta Ladder',
                 'Bump and Reprice Cross-gamma Ladder',
                 'Bump and Reprice ATM Vega Ladder', 'Bump and Reprice ATM Vega Ladder (EOD)',
                 'Bump and Reprice Surface Vega Ladder', 'Bump and Reprice Surface Vega Ladder (EOD)',
                 'Bump and Reprice Risk-reversal Vega Ladder',
                 'Bump and Reprice Risk-reversal Vega Ladder (EOD)',
                 'Bump and Reprice Butterfly Vega Ladder',
                 'Bump and Reprice Butterfly Vega Ladder (EOD)',
                 'Bump and Reprice DV01 Ladder', 'Bump and Reprice Delta Ladder')
group by 1, 2
order by 1, 2 desc;

\echo ''
\echo '--- The live list: every ladder definition, its cut and its run settings ---'

select p.short_code as party, d.name, d.cut_kind_code as cut,
       d.schedule_expression as schedule, cfg.name as run_settings,
       cfg.market_data_type as market_data
from ores_reporting_report_definitions_tbl d
join ores_refdata_parties_tbl p
  on p.id = d.party_id and p.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_reporting_report_configurations_tbl b
  on b.report_definition_id = d.id
 and b.configuration_type_code = 'risk_report_config'
 and b.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_reporting_configurations_tbl h
  on h.id = b.configuration_id and h.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_reporting_risk_report_configs_tbl cfg
  on cfg.configuration_id = h.id and cfg.valid_to = ores_utility_infinity_timestamp_fn()
where d.valid_to = ores_utility_infinity_timestamp_fn()
  and d.name in ('Bump and Reprice Rho Ladder', 'Bump and Reprice Rho Ladder (EOD)',
                 'Bump and Reprice Gamma Ladder', 'Bump and Reprice Theta Ladder',
                 'Bump and Reprice Cross-gamma Ladder',
                 'Bump and Reprice ATM Vega Ladder', 'Bump and Reprice ATM Vega Ladder (EOD)',
                 'Bump and Reprice Surface Vega Ladder', 'Bump and Reprice Surface Vega Ladder (EOD)',
                 'Bump and Reprice Risk-reversal Vega Ladder',
                 'Bump and Reprice Risk-reversal Vega Ladder (EOD)',
                 'Bump and Reprice Butterfly Vega Ladder',
                 'Bump and Reprice Butterfly Vega Ladder (EOD)',
                 'Bump and Reprice DV01 Ladder', 'Bump and Reprice Delta Ladder')
order by p.short_code, d.name;

\echo ''
\echo '--- The six configuration types each ladder definition binds, one party ---'

select b.configuration_type_code as type, count(*) as bindings
from ores_reporting_report_configurations_tbl b
join ores_reporting_report_definitions_tbl d
  on d.id = b.report_definition_id and d.valid_to = ores_utility_infinity_timestamp_fn()
join ores_refdata_parties_tbl p
  on p.id = d.party_id and p.short_code = 'ACCOUK'
 and p.valid_to = ores_utility_infinity_timestamp_fn()
where b.valid_to = ores_utility_infinity_timestamp_fn()
  and d.name like 'Bump and Reprice %'
  and d.name in ('Bump and Reprice Rho Ladder', 'Bump and Reprice Rho Ladder (EOD)',
                 'Bump and Reprice Gamma Ladder', 'Bump and Reprice Theta Ladder',
                 'Bump and Reprice Cross-gamma Ladder',
                 'Bump and Reprice ATM Vega Ladder', 'Bump and Reprice ATM Vega Ladder (EOD)',
                 'Bump and Reprice Surface Vega Ladder', 'Bump and Reprice Surface Vega Ladder (EOD)',
                 'Bump and Reprice Risk-reversal Vega Ladder',
                 'Bump and Reprice Risk-reversal Vega Ladder (EOD)',
                 'Bump and Reprice Butterfly Vega Ladder',
                 'Bump and Reprice Butterfly Vega Ladder (EOD)',
                 'Bump and Reprice DV01 Ladder', 'Bump and Reprice Delta Ladder')
group by 1
order by 1;

\echo ''
\echo '--- The sensitivity analytic of every run settings row a ladder binds ---'

select distinct p.short_code as party, cfg.name as run_settings,
       op.name as parameter, v.value
from ores_reporting_report_definitions_tbl d
join ores_refdata_parties_tbl p
  on p.id = d.party_id and p.valid_to = ores_utility_infinity_timestamp_fn()
join ores_reporting_report_configurations_tbl b
  on b.report_definition_id = d.id
 and b.configuration_type_code = 'risk_report_config'
 and b.valid_to = ores_utility_infinity_timestamp_fn()
join ores_reporting_configurations_tbl h
  on h.id = b.configuration_id and h.valid_to = ores_utility_infinity_timestamp_fn()
join ores_reporting_risk_report_configs_tbl cfg
  on cfg.configuration_id = h.id and cfg.valid_to = ores_utility_infinity_timestamp_fn()
join ores_reporting_configuration_analytics_tbl a
  on a.configuration_id = cfg.configuration_id
 and a.valid_to = ores_utility_infinity_timestamp_fn()
 and a.analytic_type_code = 'sensitivity'
join ores_reporting_configuration_analytic_parameters_tbl v
  on v.configuration_analytic_id = a.id and v.valid_to = ores_utility_infinity_timestamp_fn()
join ores_reporting_ore_parameters_tbl op
  on op.id = v.ore_parameter_id
 and op.name in ('sensitivityConfigFile', 'marketConfigFile')
where d.valid_to = ores_utility_infinity_timestamp_fn()
  and d.name in ('Bump and Reprice Rho Ladder', 'Bump and Reprice Rho Ladder (EOD)',
                 'Bump and Reprice Gamma Ladder', 'Bump and Reprice Theta Ladder',
                 'Bump and Reprice Cross-gamma Ladder',
                 'Bump and Reprice ATM Vega Ladder', 'Bump and Reprice ATM Vega Ladder (EOD)',
                 'Bump and Reprice Surface Vega Ladder', 'Bump and Reprice Surface Vega Ladder (EOD)',
                 'Bump and Reprice Risk-reversal Vega Ladder',
                 'Bump and Reprice Risk-reversal Vega Ladder (EOD)',
                 'Bump and Reprice Butterfly Vega Ladder',
                 'Bump and Reprice Butterfly Vega Ladder (EOD)',
                 'Bump and Reprice DV01 Ladder', 'Bump and Reprice Delta Ladder')
order by p.short_code, cfg.name, op.name;

\echo ''
\echo '--- Northwind: staged, waiting for its tenant ---'

select s.code as dataset, a.name, a.cut_kind_code as cut, a.schedule_expression as schedule
from ores_dq_report_definitions_artefact_tbl a
join ores_dq_datasets_tbl s
  on s.id = a.dataset_id and s.valid_to = ores_utility_infinity_timestamp_fn()
where s.code like 'northwind%'
  and a.name in ('Bump and Reprice Rho Ladder', 'Bump and Reprice Rho Ladder (EOD)',
                 'Bump and Reprice Gamma Ladder', 'Bump and Reprice Theta Ladder',
                 'Bump and Reprice Cross-gamma Ladder',
                 'Bump and Reprice ATM Vega Ladder', 'Bump and Reprice ATM Vega Ladder (EOD)',
                 'Bump and Reprice Surface Vega Ladder', 'Bump and Reprice Surface Vega Ladder (EOD)',
                 'Bump and Reprice Risk-reversal Vega Ladder',
                 'Bump and Reprice Risk-reversal Vega Ladder (EOD)',
                 'Bump and Reprice Butterfly Vega Ladder',
                 'Bump and Reprice Butterfly Vega Ladder (EOD)',
                 'Bump and Reprice DV01 Ladder', 'Bump and Reprice Delta Ladder')
order by a.name;

\o
