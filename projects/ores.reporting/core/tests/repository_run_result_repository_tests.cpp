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
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.core/repository/report_metric_repository.hpp"
#include "ores.reporting.core/repository/run_result_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "test_suite.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

namespace {

std::string run_result_new_uuid() {
    return boost::uuids::to_string(boost::uuids::random_generator()());
}

ores::reporting::domain::report_metric make_metric(const std::string& instance_id,
                                                   const std::string& definition_id,
                                                   const std::string& measure,
                                                   const std::string& currency,
                                                   const std::string& qualifiers,
                                                   double value) {
    return ores::reporting::domain::report_metric{
        .id = boost::uuids::random_generator()(),
        .party_id = boost::uuids::random_generator()(),
        .report_instance_id = boost::uuids::string_generator()(instance_id),
        .family = "npv",
        .subject_type = "trade",
        .subject_id = "Swap_20y",
        .as_of = std::chrono::sys_days{std::chrono::year{2026} / 10 / 31},
        .measure = measure,
        .report_definition_id = boost::uuids::string_generator()(definition_id),
        .currency = currency,
        .qualifiers = qualifiers,
        .value = value,
        .source = "npv"};
}

}

using namespace ores::logging;
using ores::reporting::repository::report_metric_repository;
using ores::reporting::repository::run_result_repository;
using ores::testing::database_helper;

TEST_CASE("the screen reads a run's figures through the run result view", "[repository]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto instance_id = run_result_new_uuid();
    const auto definition_id = run_result_new_uuid();
    const auto counterparty = run_result_new_uuid();

    // The post-processor stores the engine's own headings, with the counterparty
    // among the qualifiers.
    const auto qualifiers = std::string(R"({"CounterParty":")") + counterparty + R"("})";
    report_metric_repository metrics;
    metrics.insert(
        h.context(),
        {make_metric(instance_id, definition_id, "NPV", "USD", qualifiers, 1234.5),
         make_metric(instance_id, definition_id, "NPV(Base)", "EUR", qualifiers, 1100.0),
         make_metric(instance_id, definition_id, "Notional", "USD", qualifiers, 1000.0)});

    run_result_repository results;
    const auto rows = results.read_latest(h.context(), 0, 100);

    auto found = [&rows](const std::string& measure) {
        return std::ranges::find_if(rows,
                                    [&measure](const auto& one) { return one.measure == measure; });
    };
    REQUIRE(found("npv") != rows.end());
    CHECK(found("npv")->figure == 1234.5);
    CHECK(found("npv")->currency == "USD");
    CHECK(found("npv")->subject_id == "Swap_20y");
    CHECK(found("npv")->counterparty_id.has_value());
    CHECK(boost::uuids::to_string(*found("npv")->counterparty_id) == counterparty);

    // The engine's heading becomes the word the screen knows the figure by.
    REQUIRE(found("npv_base") != rows.end());
    CHECK(found("npv_base")->currency == "EUR");
    REQUIRE(found("notional") != rows.end());
    CHECK(found("notional")->figure == 1000.0);

    // A family that is not a result row does not reach the screen through it.
    const auto metrics_of_another_family =
        std::ranges::find_if(rows, [](const auto& one) { return one.source != "npv"; });
    CHECK(metrics_of_another_family == rows.end());
}
