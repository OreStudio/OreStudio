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

-- =============================================================================
-- The analytics each seeded report turns on
-- =============================================================================
--
-- A report definition is a name and a schedule. The risk report config the run
-- resolves by definition id carries the analytics it turns on, and this map is
-- where those flags are authored: the risk_report_configs artefact seed reads
-- this function to fill its enable columns, and
-- ores_reporting_publish_risk_report_configs_from_dq_fn copies them into the
-- config. The rows are staged as a DQ dataset like every other ACME datum, so
-- the map and the artefact table stay the only two places the flags live.
--
-- The names are the ones seeded for ore.report_definitions; a name the map
-- does not know turns nothing on, which is the right default for a report that
-- inspects data rather than prices it.
create or replace function ores_reporting_analytic_flags_fn(
    p_report_name text
)
returns jsonb as $$
    select to_jsonb(f) - 'name'
    from (values
        ('Data Quality Completeness',              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('System Health',                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Audit Trail Summary',                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('SA-CCR Exposure',                        0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
        ('Leverage Ratio',                         0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
        ('FRTB SA Capital',                        0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Liquidity Coverage',                     0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Large Exposures',                        0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
        ('MIFID II Transaction Reporting',         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('CFTC Swap Data Reporting',               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('HKMA Trade Repository',                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Group Consolidated Exposure',            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
        ('Board Risk Dashboard',                   1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0),
        ('Cross-Entity Counterparty Concentration',0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
        ('Intercompany Exposure Matrix',           0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
        ('Model Calibration',                      0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Yield Curves',                           0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('FX Spot Rates',                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Volatility Surfaces',                    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Credit Curves',                          0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('NPV',                                    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Cashflows',                              0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Delta and Gamma',                        0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Vega',                                   0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Bucketed DV01',                          0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Exposure',                               0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0),
        ('CVA/DVA/FVA',                            0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1),
        ('Stressed VaR',                           0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0),
        ('P&L Attribution',                        1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Intraday Risk Monitor',                  1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0),
        ('Headline Position',                      1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    ) as f(name, npv, cashflow, curves, sensitivity, simulation,
           xva, stress, pvar, initial_margin, pfe, cva, dva, fva)
    where f.name = p_report_name;
$$ language sql immutable;

comment on function ores_reporting_analytic_flags_fn(text) is
'Returns the ORE analytics a seeded report turns on, as a JSON object of the
 risk report config enable flags. A name the map does not know returns null.';
