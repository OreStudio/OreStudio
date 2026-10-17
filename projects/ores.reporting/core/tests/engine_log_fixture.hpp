/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
// The engine's own account of a report run that failed, as a real run wrote it:
// the output archive of the Acme bond report of 2026-10-16, in which all eight
// bonds would not build. Every message the engine emitted for that run is here,
// in the order it emitted them, so a test reads the vocabulary the engine
// actually uses rather than a sample of it.
//
// Kept as text rather than as the archive it came from: the archive is the
// engine's output and is megabytes of CSV, and the part under test is the
// structured log inside it.

#ifndef ORES_REPORTING_CORE_TESTS_ENGINE_LOG_FIXTURE_HPP
#define ORES_REPORTING_CORE_TESTS_ENGINE_LOG_FIXTURE_HPP

#include <cstddef>
#include <string_view>

namespace ores::reporting::tests {

using namespace std::string_view_literals;

/// The name the engine gives its structured log inside a run's output.
constexpr std::string_view structured_log_name = "log_structured.json";

/// Every message the engine emitted for a run whose eight bonds would not build.
constexpr std::string_view
    failing_run_log = R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/PRICING'"}, {"name": "tradeId", "value": "033b4053-fe1d-5901-bc2f-7d95afe91f03"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/PRICING'"}, {"name": "tradeId", "value": "073c6cda-19e9-5a19-868a-1cfe40f1100e"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/PRICING'"}, {"name": "tradeId", "value": "1277dfc4-6097-5488-86a8-98047090a043"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/PRICING'"}, {"name": "tradeId", "value": "5585a86b-1962-53c4-ae50-0c7ace4ce605"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/PRICING'"}, {"name": "tradeId", "value": "5e4c8789-ba0f-54c5-b27e-d0f8954b0470"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/PRICING'"}, {"name": "tradeId", "value": "a25dc282-5363-5298-95ba-5db45a28fd4e"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/PRICING'"}, {"name": "tradeId", "value": "c673b558-d5a3-5aee-a449-b868f77f1013"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/PRICING'"}, {"name": "tradeId", "value": "e141952c-6645-5aa1-a276-86378990fecd"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Analytics", "message": "Portfolio does not contain any built trades, context is 'analytic/PRICING'", "sub_fields": [{"name": "analyticType", "value": "PRICING"}, {"name": "exceptionType", "value": "Failed Analytic"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/SA_CCR'"}, {"name": "tradeId", "value": "033b4053-fe1d-5901-bc2f-7d95afe91f03"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/SA_CCR'"}, {"name": "tradeId", "value": "073c6cda-19e9-5a19-868a-1cfe40f1100e"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/SA_CCR'"}, {"name": "tradeId", "value": "1277dfc4-6097-5488-86a8-98047090a043"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/SA_CCR'"}, {"name": "tradeId", "value": "5585a86b-1962-53c4-ae50-0c7ace4ce605"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/SA_CCR'"}, {"name": "tradeId", "value": "5e4c8789-ba0f-54c5-b27e-d0f8954b0470"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/SA_CCR'"}, {"name": "tradeId", "value": "a25dc282-5363-5298-95ba-5db45a28fd4e"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/SA_CCR'"}, {"name": "tradeId", "value": "c673b558-d5a3-5aee-a449-b868f77f1013"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Trade", "message": "did not find object  of type yield curve / ibor index under configuration 'default' or 'default' in YieldCurves", "sub_fields": [{"name": "exceptionType", "value": "Error building trade for context 'analytic/SA_CCR'"}, {"name": "tradeId", "value": "e141952c-6645-5aa1-a276-86378990fecd"}, {"name": "tradeType", "value": "Bond"}]})JSON"
                      "\n"
                      R"JSON({"category": "Error", "group": "Analytics", "message": "Portfolio does not contain any built trades, context is 'analytic/SA_CCR'", "sub_fields": [{"name": "analyticType", "value": "SA_CCR"}, {"name": "exceptionType", "value": "Failed Analytic"}]})JSON"
                      "\n"
                      R"JSON({"category": "Warning", "group": "Analytics", "message": "Error in ORE analytics: Failed to run analytics PRICING,SA_CCR", "sub_fields": [{"name": "analyticType", "value": "OREApp::run()"}, {"name": "warningType", "value": "Error"}]})JSON"
                      "\n"
                      // The engine terminates every line, the last one too, and a reader that
                      // splits on the terminator would otherwise drop it.
                      "";


/// How many messages of each scope that run produced: one per bond that would
/// not build, one per analytic that then had nothing to price, and one warning
/// the engine wrote when it gave up on the pair.
constexpr std::size_t failing_run_trade_error_count = 16;
constexpr std::size_t failing_run_analytic_error_count = 3;

}

#endif
