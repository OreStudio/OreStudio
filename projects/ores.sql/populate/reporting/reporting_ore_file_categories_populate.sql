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
 * ORE File Categories Population Script
 *
 * GENERATED FILE - DO NOT EDIT. Regenerate with
 * projects/ores.sql/utility/generate_file_types_populate.py.
 *
 * Seeds the 14 groups the files of an ORE run fall in, from
 * projects/ores.sql/utility/ore_files.py. This script is idempotent.
 */

\echo '--- ORE File Categories ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
    r record;
begin
    for r in
        select * from (values
        ('master_input', 'Master Input', 'supplied', 'The master input of a run. It is the one file the ORE engine is started with, and every other file a run reads is named in it. A person opens it to see which analytics a run asked for, the as-of date and the base currency, and which configuration, trade and market data files it used.', 1),
        ('schemas', 'Schemas', 'supplied', 'The XML schemas ORE ships with the engine. Each declares the elements, the order and the allowed values of one kind of XML document, so a document can be checked before a run rather than failing during it. A schema belongs to one engine version: the schema of the version that runs a document is the one that says whether the engine will read it. Some schemas include others and are complete only with them.', 2),
        ('configuration', 'Configuration', 'supplied', 'The configuration documents of a run. Each is an XML document that ORE validates against its own schema, and each describes one part of how the run works: how the curves are built, which market objects exist, how each product is priced, how the market is simulated, which risk factors are shifted. ORE Studio holds their content as configuration entities and writes the documents when it packages a run, so the documents of a run show exactly which configuration produced its figures.', 3),
        ('trade_data', 'Trade Data', 'supplied', 'The trade data of a run: the portfolio of trades it values, the netting sets the trades belong to with the terms of their collateral agreements, and the facts about the counterparties and collateral that the exposure and capital methods need. Every analytic runs over these trades, so they decide which rows the reports contain.', 4),
        ('market_data', 'Market Data', 'supplied', 'The market data of a run: the quotes the curves and surfaces are built from, the historical fixings of the indices the trades reference, and the historical scenarios, covariances and stress shifts that some risk analytics are given. These are plain data files, not configuration: they change every day while the configuration stays the same.', 5),
        ('calibration_report', 'Calibration Reports', 'produced', 'The reports of the first step of a run: the term structures ORE built from the market data and the models it calibrated. A person reads them to check that the curves and the model parameters the rest of the run relies on are sensible, before trusting any valuation or risk figure.', 6),
        ('valuation_report', 'Valuation Reports', 'produced', 'The valuation reports: the value of each trade today, its cashflows and the extra results its pricing engine reported. These are the basic figures of a run. Most other reports are checked against them.', 7),
        ('risk_report', 'Risk Reports', 'produced', 'The market risk reports: how the value of the portfolio changes when the market moves. They hold the sensitivities to each risk factor, the values under stress scenarios, the value at risk and the profit and loss between two dates with its explanation.', 8),
        ('capital_report', 'Capital and Margin Reports', 'produced', 'The regulatory capital and initial margin reports: the standardised methods regulators prescribe, such as SA-CCR, SA-CVA, BA-CVA and the standardised market risk charge, and the initial margin under ISDA SIMM or the regulatory schedule, with the CRIF sensitivities the margin is computed from.', 9),
        ('xva_report', 'XVA and Exposure Reports', 'produced', 'The XVA and exposure reports: the third step of a run, which post-processes the simulated values of the trades. They hold the exposure profiles of each trade and netting set through time, the valuation adjustments such as CVA, DVA, FVA, COLVA and MVA, the dynamic initial margin, and the explanation and sensitivities of the adjustments.', 10),
        ('portfolio_details', 'Portfolio Details', 'produced', 'Lists of what a portfolio contains and depends on: its counterparties, netting sets and trade types with how many trades each has, and the indices, market objects and risk factors its trades need. A person reads them to see what a run must be given before it can value the portfolio.', 11),
        ('intermediate_data', 'Intermediate Data', 'produced', 'The data files one analytic of a run writes for another to read: the simulated value cubes, the simulated market scenarios, the conversion matrices between zero and par rates. They are large and meant for machines rather than people, but they are kept so that a later run can reuse them and so that a figure in a report can be traced back to the paths it came from.', 12),
        ('run_diagnostics', 'Run Diagnostics', 'produced', 'What the engine reports about the run itself: its log, how long each part took, how often each trade was priced, and the market data, fixings and dividends it actually loaded. A person reads them when a run fails or when a figure needs to be traced to the input it was computed from.', 13),
        ('packaging', 'ORE Studio Packaging', 'packaged', 'The archives ORE Studio makes to carry a run to the compute grid and back. They are not part of ORE: the input package holds every file the engine is given, the output archive holds every file the engine wrote, and the manifest lists the outputs one by one so that each can be read without unpacking the archive. They are the complete record of what a run was given and what it produced.', 14)
        ) as t (code, name, origin, description, display_order)
    loop
        if not exists (
            select 1 from ores_reporting_ore_file_categories_tbl
            where tenant_id = v_sys_tenant and code = r.code
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            insert into ores_reporting_ore_file_categories_tbl (
                code, name, origin, description, display_order,
                tenant_id, version, modified_by, change_reason_code, change_commentary
            ) values (
                r.code, r.name, r.origin, r.description, r.display_order,
                v_sys_tenant, 0, current_user, 'system.initial_load', 'Seed ORE file category: ' || r.code
            );
        end if;
    end loop;
end $$;
