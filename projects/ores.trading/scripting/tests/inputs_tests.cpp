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

/// The library the store is published from: each script's variants, in the
/// order the dataset stages them.
struct staged_library {
    std::map<std::string, std::vector<std::pair<std::string, std::string>>> variants;
    std::map<std::string, std::vector<script_input>> inputs;
};

staged_library staged() {
    const auto sql = file_text(
        project_root() / "projects/ores.sql/populate/ore/ore_script_library_artefact_populate.sql");
    staged_library out;
    const std::string row_head =
        R"re(\n        \(v_dataset_id, ores_utility_system_tenant_id_fn\(\), 0, )re";
    const std::regex variant(row_head + R"re('(\w+)', '(\w+)', \$code\$([\s\S]*?)\$code\$)re");
    for (auto m = std::sregex_iterator(sql.begin(), sql.end(), variant);
         m != std::sregex_iterator();
         ++m)
        out.variants[(*m)[1].str()].emplace_back((*m)[2].str(), (*m)[3].str());
    const std::regex input(
        row_head + R"re('(\w+)', \d+, '(\w+)', '(\w+)', (true|false), (null|'(?:[^']|'')*'), )re");
    const auto block = sql.substr(sql.find("insert into ores_dq_script_inputs_artefact_tbl"));
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

std::string described(const script_input& in) {
    return in.name + " " + in.type + (in.is_array ? "[]" : "") +
           (in.rule ? " {" + *in.rule + "}" : "");
}

}

TEST_CASE("the inputs the parser derives are the inputs the dataset stages", inputs_tags) {
    const auto library = staged();
    REQUIRE(library.variants.size() > 50u);
    REQUIRE(library.inputs.size() > 50u);

    std::set<std::string> differing;
    std::size_t compared = 0;
    for (const auto& [name, variants] : library.variants) {
        INFO("script " << name);
        // The default variant first, as the trade states what it reads first.
        auto ordered = variants;
        std::stable_partition(
            ordered.begin(), ordered.end(), [](const auto& v) { return v.first == "default"; });
        std::vector<std::vector<script_input>> per_variant;
        for (const auto& [purpose, code] : ordered) {
            const auto& text = code;
            const auto result = parse(text);
            INFO("variant " << purpose);
            REQUIRE(result.parsed());
            per_variant.push_back(inputs_of(result.tree, text));
        }
        const auto derived = merge_inputs(per_variant);
        const auto stored =
            library.inputs.contains(name) ? library.inputs.at(name) : std::vector<script_input>{};
        // The names and their order must agree, and every input's type, shape
        // and rule; a difference within an input is reported by name.
        std::vector<std::string> derived_names, stored_names;
        for (const auto& in : derived)
            derived_names.push_back(in.name);
        for (const auto& in : stored)
            stored_names.push_back(in.name);
        CHECK(derived_names == stored_names);
        for (std::size_t i = 0; i < std::min(derived.size(), stored.size()); ++i)
            if (derived[i] != stored[i])
                differing.insert(name + ": " + described(derived[i]) + " | stored " +
                                 described(stored[i]));
        compared += stored.size();
    }
    CHECK(compared > 700u);
    std::string report;
    for (const auto& d : differing)
        report += "\n  " + d;
    INFO("differing inputs:" << report);
    CHECK(differing.empty());
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

TEST_CASE("a variant that types or indexes an input says more of it than one that reads it",
          inputs_tags) {
    const std::vector<std::vector<script_input>> variants = {
        {{.name = "Fixings", .type = "Number"}},
        {{.name = "Fixings", .type = "Event", .is_array = true}}};
    const auto merged = merge_inputs(variants);
    REQUIRE(merged.size() == 1u);
    CHECK(merged[0].type == "Event");
    CHECK(merged[0].is_array);
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
