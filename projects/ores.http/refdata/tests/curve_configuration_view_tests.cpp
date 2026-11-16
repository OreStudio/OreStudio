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
#include "ores.http.core/routes/ore_value_sets.hpp"
#include "ores.http/routes/refdata/curve_configuration_view.hpp"
#include "ores.http/routes/refdata/curve_document_cells.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using ores::http::routes::refdata::document::cells;
using ores::http::routes::refdata::curve_view::contract;
using ores::http::routes::refdata::curve_view::view_json_of;
using ores::http::routes::refdata::curve_view::vocabulary;
using ores::http::routes::value_set_reader;
using ores::http::routes::value_set_tables;
using namespace ores::logging;

namespace {

namespace refdata_domain = ores::refdata::domain;
namespace refdata_messaging = ores::refdata::messaging;
namespace reporting_domain = ores::reporting::domain;

const auto definition_id = boost::uuids::string_generator()("00000000-0000-0000-0000-00000000000a");
const auto segment_id = boost::uuids::string_generator()("00000000-0000-0000-0000-0000000000c1");

/// The document one yield curve needs: its definition, its row, and its segment.
refdata_messaging::curve_configuration_document yield_curve_document(bool with_segment) {
    refdata_messaging::curve_configuration_document document;
    document.config.name = "ACME curves";
    document.config.version = 3;

    refdata_domain::curve_definition stated;
    stated.id = definition_id;
    stated.curve_id = "EUR-ESTR";
    stated.description = "ESTR discounting";
    stated.section_code = "YieldCurves";
    stated.position = 4;
    stated.version = 2;
    document.definitions.push_back(stated);

    refdata_domain::yield_curve_config curve;
    curve.curve_definition_id = definition_id;
    curve.currency = "EUR";
    curve.day_counter = "ACT/360";
    document.yield_curves.push_back(curve);

    if (!with_segment)
        return document;

    refdata_domain::curve_segment segment;
    segment.id = segment_id;
    segment.curve_definition_id = definition_id;
    segment.segment_type = "Deposit";
    segment.position = 1;
    document.segments.push_back(segment);

    refdata_domain::curve_segment_convention convention;
    convention.curve_segment_id = segment_id;
    convention.conventions = "EUR-ESTR-3M";
    document.segment_conventions.push_back(convention);

    refdata_domain::curve_quote quote;
    quote.id = boost::uuids::string_generator()("00000000-0000-0000-0000-0000000000d1");
    quote.curve_segment_id = segment_id;
    quote.quote_list = "Deposit";
    quote.quote_text = "EUR-ESTR-3M";
    quote.optional_flag = "true";
    document.quotes.push_back(quote);
    return document;
}

/// The contract entries one drawn section is checked against.
std::vector<reporting_domain::ore_parameter> contract_rows() {
    reporting_domain::ore_parameter currency;
    currency.schema_name = "curveconfig.xsd";
    currency.xpath = "/CurveConfiguration/YieldCurves/YieldCurve/Currency";
    currency.scope = "GlobalParameters";
    currency.name = "Currency";
    currency.label = "Currency";
    currency.is_required = true;

    reporting_domain::ore_parameter curve_id;
    curve_id.schema_name = "curveconfig.xsd";
    curve_id.xpath = "/CurveConfiguration/YieldCurves/YieldCurve/CurveId";
    curve_id.scope = "GlobalParameters";
    curve_id.name = "CurveId";
    curve_id.label = "Curve identifier";
    return {currency, curve_id};
}

/// The value sets, which one drawn section names no set of.
value_set_reader reader_with_no_sets() {
    return value_set_reader(value_set_tables{},
                            [](std::string_view) -> std::vector<ores::http::routes::value_choice> {
                                throw std::runtime_error("no catalogue relation is read here");
                            });
}

refdata_domain::curve_section section_row(std::string code, std::string entry, std::string words) {
    refdata_domain::curve_section row;
    row.code = std::move(code);
    row.entry_element = std::move(entry);
    row.description = std::move(words);
    return row;
}

}

TEST_CASE("a_curve_configuration_draws_its_sections_entries_and_leaves", "[curve_view]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing one curve configuration as its screen reads it";

    const auto document = cells(yield_curve_document(false));
    const auto rows = contract_rows();
    contract parameters;
    for (const auto& row : rows)
        parameters.emplace(row.xpath, &row);
    const auto values = reader_with_no_sets();
    const std::map<std::string, std::string> kinds;

    const auto answer = view_json_of(
        document, "00000000-0000-0000-0000-000000000001",
        {section_row("YieldCurves", "", "The yield curves")}, {parameters, values, kinds});

    CHECK(answer.find("\"name\":\"ACME curves\"") != std::string::npos);
    CHECK(answer.find("\"configurationId\":\"00000000-0000-0000-0000-000000000001\"") !=
          std::string::npos);
    CHECK(answer.find("\"code\":\"YieldCurves\"") != std::string::npos);
    CHECK(answer.find("\"entryElement\":\"YieldCurve\"") != std::string::npos);
    CHECK(answer.find("\"mapped\":true") != std::string::npos);
    CHECK(answer.find("\"curveId\":\"EUR-ESTR\"") != std::string::npos);
    CHECK(answer.find("\"position\":4") != std::string::npos);
    CHECK(answer.find("\"version\":2") != std::string::npos);
    CHECK(answer.find("\"xpath\":\"/CurveConfiguration/YieldCurves/YieldCurve/Currency\"") !=
          std::string::npos);
    CHECK(answer.find("\"value\":\"EUR\"") != std::string::npos);
    CHECK(answer.find("\"valueSet\":\"\"") != std::string::npos);
    CHECK(answer.find("\"documented\":2") != std::string::npos);
    CHECK(answer.find("\"total\":12") != std::string::npos);
    CHECK(answer.find("\"collections\":[]") != std::string::npos);
}

TEST_CASE("a_curve_configurations_segments_draw_their_kind_and_quotes", "[curve_view]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the segments of one curve configuration";

    const auto document = cells(yield_curve_document(true));
    const auto rows = contract_rows();
    contract parameters;
    for (const auto& row : rows)
        parameters.emplace(row.xpath, &row);
    const auto values = reader_with_no_sets();
    const std::map<std::string, std::string> kinds{{"Deposit", "Simple"}};

    const auto answer = view_json_of(
        document, "00000000-0000-0000-0000-000000000001",
        {section_row("YieldCurves", "", "The yield curves")}, {parameters, values, kinds});

    CHECK(answer.find("\"element\":\"Segments\"") != std::string::npos);
    CHECK(answer.find("\"kind\":\"Simple\"") != std::string::npos);
    CHECK(answer.find("\"value\":\"EUR-ESTR-3M\"") != std::string::npos);
    CHECK(answer.find("\"list\":\"Deposit\"") != std::string::npos);
    CHECK(answer.find("\"optional\":true") != std::string::npos);
    // Twelve leaves of the curve, and the four a Simple segment reads.
    CHECK(answer.find("\"total\":16") != std::string::npos);
}

TEST_CASE("a_curve_configuration_lists_a_section_no_projection_maps", "[curve_view]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a curve section the projection does not map";

    const auto document = cells(yield_curve_document(false));
    const auto rows = contract_rows();
    contract parameters;
    for (const auto& row : rows)
        parameters.emplace(row.xpath, &row);
    const auto values = reader_with_no_sets();
    const std::map<std::string, std::string> kinds;

    const auto answer = view_json_of(
        document, "00000000-0000-0000-0000-000000000001",
        {section_row("BondCurves", "BondCurve", "The bond curves")}, {parameters, values, kinds});

    CHECK(answer.find("\"code\":\"BondCurves\"") != std::string::npos);
    CHECK(answer.find("\"entryElement\":\"BondCurve\"") != std::string::npos);
    CHECK(answer.find("\"mapped\":false") != std::string::npos);
    CHECK(answer.find("\"total\":0") != std::string::npos);
}
