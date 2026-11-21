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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "library_files.hpp"
#include "ores.trading.scripting/inputs.hpp"
#include "ores.trading.scripting/parse.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <map>
#include <regex>
#include <set>
#include <sstream>
#include <string>
#include <vector>

namespace {

using namespace ores::trading::scripting;
using ores::trading::scripting::tests::file_text;
using ores::trading::scripting::tests::project_root;
namespace fs = std::filesystem;

const std::string inputs_tags = "[trading][scripting][inputs]";


std::string unquoted(const std::string& sql) {
    if (sql == "null")
        return {};
    std::string out;
    for (std::size_t i = 1; i + 1 < sql.size(); ++i) {
        out += sql[i];
        if (sql[i] == '\'' && sql[i + 1] == '\'')
            ++i;
    }
    return out;
}

/// The library the store is seeded with: each script's variants, in seed order.
struct seeded_library {
    std::map<std::string, std::vector<std::pair<std::string, std::string>>> variants;
    std::map<std::string, std::vector<script_input>> inputs;
};

seeded_library seeded() {
    const auto sql = file_text(
        project_root() / "projects/ores.sql/populate/trading/trading_script_library_populate.sql");
    seeded_library out;
    const std::regex variant(
        R"re(\n    \('(\w+)', ores_utility_system_tenant_id_fn\(\), 0, '(\w+)', \$code\$([\s\S]*?)\$code\$)re");
    for (auto m = std::sregex_iterator(sql.begin(), sql.end(), variant);
         m != std::sregex_iterator();
         ++m)
        out.variants[(*m)[1].str()].emplace_back((*m)[2].str(), (*m)[3].str());
    const std::regex input(
        R"re(\n    \('(\w+)', ores_utility_system_tenant_id_fn\(\), 0, \d+, '(\w+)', '(\w+)', (true|false), (null|'(?:[^']|'')*'), )re");
    const auto block = sql.substr(sql.find("insert into ores_trading_script_inputs_tbl"));
    for (auto m = std::sregex_iterator(block.begin(), block.end(), input);
         m != std::sregex_iterator();
         ++m) {
        const auto rule = unquoted((*m)[5].str());
        out.inputs[(*m)[1].str()].push_back(
            {.name = (*m)[2].str(),
             .type = (*m)[3].str(),
             .is_array = (*m)[4].str() == "true",
             .rule = rule.empty() ? std::nullopt : std::optional<std::string>(rule)});
    }
    return out;
}

/*
 * The store corrects WorstOfBasketSwap after the seed, since ORE cannot parse it
 * as it ships (trading_script_library_corrections_populate.sql); the same
 * correction is applied here, and it changes no input.
 */
std::string corrected(const std::string& name, std::string code) {
    const std::string text = "couponAccumulation = couponAccumulation + (1-allAssetsTriggered)\n";
    if (name == "WorstOfBasketSwap")
        if (const auto at = code.find(text); at != std::string::npos)
            code.insert(at + text.size() - 1, ";");
    return code;
}

std::string described(const script_input& in) {
    return in.name + " " + in.type + (in.is_array ? "[]" : "") +
           (in.rule ? " {" + *in.rule + "}" : "");
}

}

TEST_CASE("the inputs the parser derives are the inputs the store holds", inputs_tags) {
    const auto library = seeded();
    REQUIRE(library.variants.size() > 50u);
    REQUIRE(library.inputs.size() > 50u);

    std::size_t compared = 0;
    for (const auto& [name, variants] : library.variants) {
        INFO("script " << name);
        // The default variant first, as the trade states what it reads first.
        auto ordered = variants;
        std::stable_partition(
            ordered.begin(), ordered.end(), [](const auto& v) { return v.first == "default"; });
        std::vector<std::vector<script_input>> per_variant;
        for (const auto& [purpose, code] : ordered) {
            const auto text = corrected(name, code);
            const auto result = parse(text);
            INFO("variant " << purpose);
            REQUIRE(result.parsed());
            per_variant.push_back(inputs_of(result.tree, text));
        }
        const auto derived = merge_inputs(per_variant);
        const auto stored =
            library.inputs.contains(name) ? library.inputs.at(name) : std::vector<script_input>{};
        std::string report;
        for (std::size_t i = 0; i < std::max(derived.size(), stored.size()); ++i)
            report += "\n  " + (i < derived.size() ? described(derived[i]) : "-") + "  |  " +
                      (i < stored.size() ? described(stored[i]) : "-");
        INFO("derived  |  stored:" << report);
        CHECK(derived == stored);
        compared += stored.size();
    }
    CHECK(compared > 700u);
}

TEST_CASE("an input takes its type from how the script uses it", inputs_tags) {
    const std::string text = "NUMBER payoff, i;\n"
                             "REQUIRE PutCall == 1 OR PutCall == -1;\n"
                             "FOR i IN (1, SIZE(Dates), 1) DO\n"
                             "  payoff = payoff + Underlying(Dates[i]);\n"
                             "END;\n"
                             "value = PAY(PutCall * (payoff - Strike), Expiry, Settlement, PayCcy)"
                             " * dcf(DayCounter, TODAY, Expiry);\n";
    const auto result = parse(text);
    REQUIRE(result.parsed());
    const auto inputs = inputs_of(result.tree, text);
    std::vector<std::string> seen;
    for (const auto& in : inputs)
        seen.push_back(described(in));
    // payoff and i are declared and value is assigned, so none is an input; TODAY
    // is the run's date.
    CHECK(seen == std::vector<std::string>{"PutCall Number {PutCall == 1 OR PutCall == -1}",
                                           "Dates Event[]",
                                           "Underlying Index",
                                           "Expiry Event",
                                           "Settlement Event",
                                           "PayCcy Currency",
                                           "Strike Number",
                                           "DayCounter Daycounter"});
}

TEST_CASE("a script with several variants has every input any variant reads", inputs_tags) {
    const std::vector<std::vector<script_input>> variants = {
        {{.name = "Strike", .type = "Number"}, {.name = "Expiry", .type = "Event"}},
        {{.name = "Expiry", .type = "Event"},
         {.name = "Strike", .type = "Number", .rule = std::string("Strike > 0")},
         {.name = "ExerciseDates", .type = "Event", .is_array = true}}};
    const auto merged = merge_inputs(variants);
    REQUIRE(merged.size() == 3u);
    CHECK(merged[0].name == "Strike");
    CHECK(merged[0].rule == std::optional<std::string>("Strike > 0"));
    CHECK(merged[1].name == "Expiry");
    CHECK(merged[2].name == "ExerciseDates");
}
