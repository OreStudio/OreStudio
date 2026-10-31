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
#include "ores.ore.core/results/cashflow_reader.hpp"
#include "ores.ore.core/results/metric_reader.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.testing/test_files.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>
#include <vector>

namespace {

const std::string gate_tags("[ore][results][corpus_gate]");

using ores::ore::results::family_of_file;
using ores::ore::results::output_families;
using ores::ore::results::output_family;
using ores::ore::results::is_cashflow_family;
using ores::ore::results::read_cashflows;
using ores::ore::results::read_metrics;

enum class shape { metric, cashflow, payload };

// The families whose shape is a market state rather than a result. They are
// stored as a payload by the marketdata task, so no reader in this component
// claims them, and the gate says so instead of skipping them silently.
const std::vector<std::string> payload_families = {"rawcube",
                                                   "netcube",
                                                   "stress_scenarios",
                                                   "scenariostatistics",
                                                   "todaysmarketcalibration",
                                                   "scenario",
                                                   "scenario_fullSurface"};

shape shape_of(const output_family& family) {
    if (std::ranges::find(payload_families, family.code) != payload_families.end())
        return shape::payload;
    // A family whose key names a payment states a cashflow, and every other
    // family states figures about a subject.
    if (is_cashflow_family(family))
        return shape::cashflow;
    return shape::metric;
}

bool reads_whole(const output_family& family, const std::string& csv) {
    switch (shape_of(family)) {
        case shape::cashflow:
            return read_cashflows(csv, family).refusals.empty();
        case shape::metric:
            return read_metrics(csv, family).refusals.empty();
        case shape::payload:
            break;
    }
    return true;
}


}

TEST_CASE("every family states the shape that reads it", gate_tags) {
    std::size_t metrics = 0;
    std::size_t cashflows = 0;
    std::size_t payloads = 0;

    for (const auto& family : output_families()) {
        INFO("the family is " << family.code);
        switch (shape_of(family)) {
            case shape::metric:
                // A metric family states figures; one without any is misclassified
                // rather than unread.
                CHECK_FALSE(family.measure_columns.empty());
                ++metrics;
                break;
            case shape::payload: {
                const auto is_payload =
                    std::ranges::find(payload_families, family.code) != payload_families.end();
                CHECK(is_payload);
                ++payloads;
                break;
            }
            case shape::cashflow:
                // A cashflow family states the numbering of a payment, or its
                // reader could not tell one payment from another.
                CHECK(std::ranges::find(family.key_columns, "CashflowNo") !=
                      family.key_columns.end());
                ++cashflows;
                break;
        }
    }

    CHECK(metrics == 50);
    CHECK(cashflows == 3);
    CHECK(payloads == payload_families.size());
}

TEST_CASE("the checked-in corpus reads whole with the reader each family states", gate_tags) {
    const auto root = ores::testing::ore_path("examples");
    REQUIRE(std::filesystem::exists(root));

    std::size_t files = 0;
    std::size_t read = 0;
    for (const auto& suite : std::filesystem::directory_iterator(root)) {
        const auto expected = suite.path() / "ExpectedOutput";
        if (!std::filesystem::is_directory(expected))
            continue;
        for (const auto& entry : std::filesystem::recursive_directory_iterator(expected)) {
            if (!entry.is_regular_file() || entry.path().extension() != ".csv")
                continue;
            ++files;

            const auto* family = family_of_file(entry.path().filename().string());
            INFO("the file is " << entry.path().string());
            REQUIRE(family != nullptr);
            if (shape_of(*family) == shape::payload)
                continue;

            INFO("the family is " << family->code);
            const auto csv = ores::testing::read_file(entry.path());
            CHECK(reads_whole(*family, csv));
            ++read;
        }
    }

    CHECK(files > 400);
    CHECK(read > 400);
}
