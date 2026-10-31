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
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.testing/test_files.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <map>
#include <string>

namespace {

const std::string cashflow_tags("[ore][results][cashflow_reader]");

using ores::ore::results::cashflow_row;
using ores::ore::results::column_named;
using ores::ore::results::output_column_kind;
using ores::ore::results::family_named;
using ores::ore::results::is_cashflow_family;
using ores::ore::results::read_cashflows;

const cashflow_row* figure_named(const std::vector<cashflow_row>& rows,
                                 const std::string& measure,
                                 const std::string& part) {
    const auto found = std::ranges::find_if(
        rows, [&](const cashflow_row& one) { return one.measure == measure && one.part == part; });
    return found == rows.end() ? nullptr : &*found;
}

std::string cashflow_sample_text(const std::string& sample_file) {
    const auto relative = std::filesystem::path(sample_file).lexically_relative("external/ore");
    return ores::testing::read_file(ores::testing::ore_path(relative.string()));
}

/**
 * @brief A one-row file of a family, stating every column the family requires.
 *
 * A synthetic file must carry the family's required columns or the reader
 * refuses it as the wrong shape, and the point of these cases is what the reader
 * does with a row it can read. Each column takes the value the case states, or a
 * value of its kind.
 */
std::string cashflow_csv(const std::string& family_code,
                         const std::map<std::string, std::string>& stated) {
    const auto* family = family_named(family_code);
    REQUIRE(family != nullptr);

    const auto value_of = [&](const std::string& name) {
        const auto it = stated.find(name);
        if (it != stated.end())
            return it->second;
        const auto* column = column_named(*family, name);
        REQUIRE(column != nullptr);
        switch (column->kind) {
            case output_column_kind::decimal:
                return std::string("1.0");
            case output_column_kind::date:
                return std::string("2026-05-11");
            case output_column_kind::integer:
                return std::string("1");
            default:
                return std::string("x");
        }
    };

    std::string csv = "#";
    std::string row;
    for (const auto& name : family->required_columns) {
        if (csv.size() > 1) {
            csv += ",";
            row += ",";
        }
        csv += name;
        row += value_of(name);
    }
    return csv + "\n" + row + "\n";
}

}

TEST_CASE("a family is a cashflow family when its key names a payment", cashflow_tags) {
    const auto* flows = family_named("flows");
    REQUIRE(flows != nullptr);
    CHECK(is_cashflow_family(*flows));

    const auto* npv = family_named("npv");
    REQUIRE(npv != nullptr);
    CHECK_FALSE(is_cashflow_family(*npv));
}

TEST_CASE("flows states one row for each figure of each payment", cashflow_tags) {
    const auto* family = family_named("flows");
    REQUIRE(family != nullptr);

    const auto outcome = read_cashflows(cashflow_sample_text(family->sample_file), *family);

    REQUIRE(outcome.refusals.empty());
    REQUIRE_FALSE(outcome.rows.empty());

    const auto& first = outcome.rows.front();
    CHECK(first.subject_type == "trade");
    CHECK_FALSE(first.subject.empty());
    CHECK_FALSE(first.cashflow_no.empty());
    CHECK_FALSE(first.leg_no.empty());
    CHECK_FALSE(first.measure.empty());
    CHECK(first.part.empty());

    const auto* amount = figure_named(outcome.rows, "Amount", "");
    REQUIRE(amount != nullptr);
    CHECK(amount->currency.size() == 3);
    CHECK(amount->value != 0.0);
}

TEST_CASE("a stressed payment states each figure as a base, a scenario and a difference",
          cashflow_tags) {
    const auto* family = family_named("stresstest_flows");
    REQUIRE(family != nullptr);

    const auto csv = cashflow_csv("stresstest_flows",
                                  {{"TradeId", "T1"},
                                   {"ScenarioLabel", "BASE"},
                                   {"CashflowNo", "1"},
                                   {"LegNo", "0"},
                                   {"PayDate", "2026-05-11"},
                                   {"FlowType", "Interest"},
                                   {"Amount_Base", "100.0"},
                                   {"Amount_Scen", "90.0"},
                                   {"Amount_Diff", "10.0"},
                                   {"Currency", "USD"}});
    const auto outcome = read_cashflows(csv, *family);

    REQUIRE(outcome.refusals.empty());

    // The family states many figures for each payment, and this case states only
    // the three parts of Amount; what it proves is that the three are told apart.
    const auto* base = figure_named(outcome.rows, "Amount", "base");
    REQUIRE(base != nullptr);
    CHECK(base->value == 100.0);
    CHECK(base->scenario == "BASE");
    CHECK(base->subject == "T1");
    CHECK(base->pay_date == "2026-05-11");
    CHECK(base->flow_type == "Interest");

    const auto* scenario = figure_named(outcome.rows, "Amount", "scen");
    REQUIRE(scenario != nullptr);
    CHECK(scenario->value == 90.0);

    const auto* difference = figure_named(outcome.rows, "Amount", "diff");
    REQUIRE(difference != nullptr);
    CHECK(difference->value == 10.0);
}

TEST_CASE("the qualifiers a payment states travel beside its figure", cashflow_tags) {
    const auto* family = family_named("flows");
    REQUIRE(family != nullptr);

    const auto csv = cashflow_csv("flows",
                                  {{"TradeId", "T1"},
                                   {"CashflowNo", "1"},
                                   {"LegNo", "0"},
                                   {"PayDate", "2026-05-11"},
                                   {"FlowType", "Interest"},
                                   {"Amount", "100.0"},
                                   {"Currency", "USD"}});
    const auto outcome = read_cashflows(csv, *family);

    REQUIRE(outcome.refusals.empty());
    REQUIRE_FALSE(outcome.rows.empty());
    // The keys the row carries in its own columns are not repeated as qualifiers,
    // and the ones it does not carry are.
    for (const auto& row : outcome.rows) {
        CHECK(std::ranges::none_of(row.qualifiers, [](const auto& qualifier) {
            return qualifier.first == "TradeId" || qualifier.first == "CashflowNo" ||
                   qualifier.first == "LegNo" || qualifier.first == "PayDate";
        }));
    }
}

TEST_CASE("a payment that states no cashflow number is refused", cashflow_tags) {
    const auto* family = family_named("flows");
    REQUIRE(family != nullptr);

    const auto csv = cashflow_csv("flows", {{"TradeId", "T1"}, {"CashflowNo", ""}});
    const auto outcome = read_cashflows(csv, *family);

    REQUIRE_FALSE(outcome.refusals.empty());
    CHECK(outcome.refusals.front().find("states no CashflowNo") != std::string::npos);
    CHECK(outcome.rows.empty());
}

TEST_CASE("a figure that is not a number is refused and the heading is named", cashflow_tags) {
    const auto* family = family_named("flows");
    REQUIRE(family != nullptr);

    const auto csv =
        cashflow_csv("flows", {{"TradeId", "T1"}, {"CashflowNo", "1"}, {"Amount", "not-a-figure"}});
    const auto outcome = read_cashflows(csv, *family);

    REQUIRE_FALSE(outcome.refusals.empty());
    CHECK(outcome.refusals.front().find("Amount") != std::string::npos);
}

TEST_CASE("a null figure states nothing and is not a refusal", cashflow_tags) {
    const auto* family = family_named("flows");
    REQUIRE(family != nullptr);

    const auto csv =
        cashflow_csv("flows", {{"TradeId", "T1"}, {"CashflowNo", "1"}, {"Amount", "#N/A"}});
    const auto outcome = read_cashflows(csv, *family);

    CHECK(outcome.refusals.empty());
    // The payment still states the other figures of its row, so the file holds
    // rows; what it must not hold is a figure the engine stated as null.
    CHECK(figure_named(outcome.rows, "Amount", "") == nullptr);
}
