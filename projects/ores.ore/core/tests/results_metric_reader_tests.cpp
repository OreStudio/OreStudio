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
#include "ores.ore.core/results/metric_reader.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.testing/test_files.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <filesystem>
#include <string>

namespace {

const std::string metric_tags("[ore][results][metric_reader]");

using ores::ore::results::family_named;
using ores::ore::results::metric;
using ores::ore::results::output_families;
using ores::ore::results::read_metrics;
using ores::ore::results::read_metrics_for_file;

const metric* metric_named(const std::vector<metric>& metrics, const std::string& measure) {
    const auto it = std::ranges::find_if(
        metrics, [&measure](const metric& one) { return one.measure == measure; });
    return it == metrics.end() ? nullptr : &*it;
}

std::string npv_header() {
    return "#TradeId,TradeType,Maturity,MaturityTime,NPV,NpvCurrency,NPV(Base),BaseCurrency,"
           "Notional,NotionalCurrency,Notional(Base),NettingSet,CounterParty\n";
}

std::string npv_row(const std::string& npv) {
    return "T1,Swap,2018-03-01,2.0," + npv + ",USD,1.0,EUR,1000,USD,900,CPTY_A,CPTY_A\n";
}

std::string sample_text(const std::string& sample_file) {
    const auto relative = std::filesystem::path(sample_file).lexically_relative("external/ore");
    return ores::testing::read_file(ores::testing::ore_path(relative.string()));
}

}

TEST_CASE("npv states one metric for each figure the row carries", metric_tags) {
    const auto* family = family_named("npv");
    REQUIRE(family != nullptr);

    const auto outcome = read_metrics(sample_text(family->sample_file), *family);

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.metrics.size() >= 4);

    const auto& first = outcome.metrics.front();
    CHECK(first.subject_type == "trade");
    CHECK(first.subject == "AverageOIS_USD");
    CHECK(first.as_of.empty());

    const auto* npv = metric_named(outcome.metrics, "NPV");
    REQUIRE(npv != nullptr);
    CHECK(npv->value == -257052.667564);

    const auto* notional = metric_named(outcome.metrics, "Notional");
    REQUIRE(notional != nullptr);
    CHECK(notional->value == 10000000.0);
}

TEST_CASE("a configuration-driven family names each column as a measure", metric_tags) {
    const auto* family = family_named("curves");
    REQUIRE(family != nullptr);

    const auto outcome = read_metrics(sample_text(family->sample_file), *family);

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.metrics.size() >= 2);

    // A family with no record key reads one row per pillar: the tenor is the
    // subject and the date is the observation.
    const auto& first = outcome.metrics.front();
    CHECK(first.subject_type == "row");
    CHECK(first.subject == "1M");
    CHECK(first.as_of == "2022-04-01");

    const auto* usd = metric_named(outcome.metrics, "USD");
    REQUIRE(usd != nullptr);
    CHECK(usd->value > 0.0);
}

TEST_CASE("a key that is not the subject travels with the metric", metric_tags) {
    const auto* family = family_named("exposure-trade");
    REQUIRE(family != nullptr);

    const auto outcome = read_metrics(sample_text(family->sample_file), *family);

    REQUIRE(outcome.refusals.empty());
    REQUIRE_FALSE(outcome.metrics.empty());

    const auto& first = outcome.metrics.front();
    CHECK(first.subject_type == "trade");
    const auto has_time =
        std::ranges::any_of(first.qualifiers, [](const auto& one) { return one.first == "Time"; });
    CHECK(has_time);
    CHECK(metric_named(outcome.metrics, "EPE") != nullptr);
}

TEST_CASE("a row whose key is empty keeps its figure and its other keys", metric_tags) {
    const auto* family = family_named("xva");
    REQUIRE(family != nullptr);

    const auto outcome = read_metrics(sample_text(family->sample_file), *family);
    REQUIRE(outcome.refusals.empty());

    // The corpus states a netting-set total with no trade id, so the subject is
    // the netting set and the figure is not lost.
    const auto total = std::ranges::find_if(
        outcome.metrics, [](const metric& one) { return one.subject_type == "netting_set"; });
    REQUIRE(total != outcome.metrics.end());
    CHECK(total->subject == "CPTY_A");
    CHECK(total->measure == "CVA");
}

TEST_CASE("a figure that is not a number is refused, and names its heading", metric_tags) {
    const auto* family = family_named("npv");
    REQUIRE(family != nullptr);

    const auto csv = npv_header() + npv_row("not-a-figure");
    const auto outcome = read_metrics(csv, *family);

    REQUIRE_FALSE(outcome.refusals.empty());
    CHECK(outcome.refusals.front().find("NPV") != std::string::npos);
}

TEST_CASE("a file that omits a required column is refused", metric_tags) {
    const auto* family = family_named("npv");
    REQUIRE(family != nullptr);

    const auto csv = "#TradeId,TradeType,Maturity,MaturityTime,NPV,NpvCurrency,NPV(Base),"
                     "BaseCurrency,Notional,NotionalCurrency,Notional(Base),NettingSet\n"
                     "T1,Swap,2018-03-01,2.0,1.0,USD,1.0,EUR,1000,USD,900,CPTY_A\n";
    const auto outcome = read_metrics(csv, *family);

    REQUIRE_FALSE(outcome.refusals.empty());
    // The keys are checked in the order the catalogue states them, so the
    // refusal names whichever required key this file leaves out.
    CHECK(outcome.refusals.front().find("states no") != std::string::npos);
}

TEST_CASE("a figure the engine has no value for is not a metric", metric_tags) {
    const auto* family = family_named("npv");
    REQUIRE(family != nullptr);

    const auto csv = npv_header() + npv_row("1.0") + npv_row("#N/A");
    const auto outcome = read_metrics(csv, *family);

    // A null figure removes that measure from its row and nothing else: the
    // first row states four figures and the second states three.
    CHECK(outcome.refusals.empty());
    REQUIRE(outcome.metrics.size() == 7);
    const auto null_npv = std::ranges::count_if(outcome.metrics, [](const metric& one) {
        return one.subject == "T1" && one.measure == "NPV";
    });
    CHECK(null_npv == 1);
    const auto second_npv = std::ranges::count_if(outcome.metrics, [](const metric& one) {
        return one.subject == "T1" && one.measure == "NPV" && one.value == 1.0;
    });
    CHECK(second_npv == 1);
}

TEST_CASE("an open family reads a measure the samples do not state", metric_tags) {
    const auto* family = family_named("curves");
    REQUIRE(family != nullptr);

    const auto* csv = "#Tenor,Date,USD,SOME.NEW.CURVE\n1M,2023-02-28,0.99,0.98\n";
    const auto outcome = read_metrics(csv, *family);

    CHECK(outcome.refusals.empty());
    REQUIRE(outcome.metrics.size() == 2);
    CHECK(metric_named(outcome.metrics, "SOME.NEW.CURVE") != nullptr);
}

TEST_CASE("a closed family refuses a measure it does not state", metric_tags) {
    const auto* family = family_named("npv");
    REQUIRE(family != nullptr);

    const auto csv = npv_header() + npv_row("1.0") +
                     "T2,Swap,2018-03-01,2.0,2.0,USD,2.0,EUR,"
                     "1000,USD,900,CPTY_A,CPTY_A,2.0\n";
    const auto header = csv.substr(0, csv.find('\n'));
    std::string with_extra = header + ",SURPRISE\n" + npv_row("1.0") + "\n";
    // The extra heading must be on every row for the file to be well formed.
    std::string row = "T2,Swap,2018-03-01,2.0,2.0,USD,2.0,EUR,1000,USD,900,CPTY_A,CPTY_A,2.0\n";
    const auto outcome = read_metrics(with_extra + row, *family);

    REQUIRE_FALSE(outcome.refusals.empty());
    CHECK(outcome.refusals.front().find("SURPRISE") != std::string::npos);
}

TEST_CASE("a file no family covers is refused", metric_tags) {
    const auto outcome = read_metrics_for_file("#A,B\n1,2\n", "no_such_family.csv");

    REQUIRE_FALSE(outcome.refusals.empty());
    CHECK(outcome.refusals.front().find("no family") != std::string::npos);
}

TEST_CASE("every metric family's checked-in sample reads", metric_tags) {
    // The three child-row families have a reader of their own, and the wide
    // market state is stored as a payload rather than read as metrics.
    const std::vector<std::string> other_shapes = {"flows",
                                                   "stresstest_flows",
                                                   "todaysmarketcalibration_cashflows",
                                                   "stress_scenarios",
                                                   "scenariostatistics"};

    std::size_t read = 0;
    for (const auto& family : output_families()) {
        if (std::ranges::find(other_shapes, family.code) != other_shapes.end())
            continue;
        // A family of no figures is not a metric family: the calibration
        // report states text beside a key, and it is market state, not a result.
        if (family.measure_columns.empty())
            continue;
        INFO("the family is " << family.code);

        const auto outcome = read_metrics(sample_text(family.sample_file), family);
        INFO("the first refusal is "
             << (outcome.refusals.empty() ? std::string("none") : outcome.refusals.front()));
        CHECK(outcome.refusals.empty());
        CHECK_FALSE(outcome.metrics.empty());
        ++read;
    }
    CHECK(read > 50);
}
