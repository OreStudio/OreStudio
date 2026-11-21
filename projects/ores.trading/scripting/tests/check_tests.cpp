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
#include "ores.trading.scripting/check.hpp"
#include "ores.trading.scripting/parse.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

using namespace ores::trading::scripting;

const std::string check_tags = "[trading][scripting][check]";

/// The findings of a script against the variables a trade states.
std::vector<diagnostic> findings(const std::string& text,
                                 const std::vector<script_variable>& variables) {
    const auto parsed = parse(text);
    REQUIRE(parsed.parsed());
    const auto inputs = inputs_of(parsed.tree, text);
    return check(parsed.tree, text, inputs, variables);
}

/// A finding as one line: its code, then its line and column or its subject.
std::string summary(const diagnostic& d) {
    if (d.start)
        return d.code + " " + std::to_string(d.start->line) + ":" +
               std::to_string(d.start->column.value_or(0));
    return d.code + " " + d.subject.value_or("");
}

const std::string script = "NUMBER unused, total;\n"
                           "REQUIRE PayoffType == 0 OR PayoffType == 1;\n"
                           "REQUIRE SIZE(Levels) == SIZE(Types);\n"
                           "total = Strike * Quantity + Levels[1] + Types[1];\n"
                           "value = PAY(total, Expiry, Settlement, PayCcy);\n";

}

TEST_CASE("every finding is reported together, located ones first", check_tags) {
    const std::vector<script_variable> variables = {
        {.name = "PayoffType", .type = "Number", .values = {"2"}},
        {.name = "Levels", .type = "Number", .is_array = true, .values = {"1", "2"}},
        {.name = "Types", .type = "Number", .is_array = true, .values = {"1", "2", "3"}},
        {.name = "Quantity", .type = "Event", .values = {"2027-01-01"}},
        {.name = "Expiry", .type = "Event", .is_array = true, .values = {"2027-01-01"}},
        {.name = "Settlement", .type = "Event", .values = {"2027-01-05"}},
        {.name = "PayCcy", .type = "Currency", .values = {"USD"}},
        {.name = "Notional", .type = "Number", .values = {"100"}}};
    const auto result = findings(script, variables);
    std::vector<std::string> seen;
    for (const auto& d : result)
        seen.push_back(summary(d));
    // unused is declared and never used; PayoffType 2 and the lists' sizes break
    // the REQUIREs; Expiry is a list, Quantity the wrong type, Strike missing, and
    // Notional never read.
    CHECK(seen == std::vector<std::string>{"SCR4001 1:1",
                                           "SCR3001 2:1",
                                           "SCR3001 3:1",
                                           "SCR2004 Expiry",
                                           "SCR2002 Notional",
                                           "SCR2003 Quantity",
                                           "SCR2001 Strike"});
    for (const auto& d : result) {
        INFO(summary(d));
        CHECK(d.severity == code_of(d.code)->severity);
        CHECK_FALSE(d.message.empty());
    }
}

TEST_CASE("a trade that states every input as the script reads it has no finding", check_tags) {
    const std::vector<script_variable> variables = {
        {.name = "PayoffType", .type = "Number", .values = {"1"}},
        {.name = "Levels", .type = "Number", .is_array = true, .values = {"1", "2"}},
        {.name = "Types", .type = "Number", .is_array = true, .values = {"3", "4"}},
        {.name = "Strike", .type = "Number", .values = {"100"}},
        {.name = "Quantity", .type = "Number", .values = {"10"}},
        {.name = "Expiry", .type = "Event", .values = {"2027-01-01"}},
        {.name = "Settlement", .type = "Event", .values = {"2027-01-05"}},
        {.name = "PayCcy", .type = "Currency", .values = {"USD"}}};
    const auto with_used = std::string(script).replace(script.find("unused, "), 8, "");
    CHECK(findings(with_used, variables).empty());
}

TEST_CASE("a REQUIRE that reads a date is left to ORE", check_tags) {
    const std::string text =
        "REQUIRE Expiry >= Start AND Quantity > 0;\n"
        "value = PAY(Quantity, Expiry, Expiry, PayCcy) + PAY(0, Start, Start, PayCcy);\n";
    const std::vector<script_variable> fine = {
        {.name = "Expiry", .type = "Event", .values = {"2027-01-01"}},
        {.name = "Start", .type = "Event", .values = {"2028-01-01"}},
        {.name = "Quantity", .type = "Number", .values = {"10"}},
        {.name = "PayCcy", .type = "Currency", .values = {"USD"}}};
    // The dates are not compared here, so a start after the expiry passes.
    CHECK(findings(text, fine).empty());
    // A known side settles AND: a quantity of zero breaks it whatever the dates.
    auto broken = fine;
    broken[2].values = {"0"};
    const auto result = findings(text, broken);
    REQUIRE(result.size() == 1u);
    CHECK(summary(result[0]) == "SCR3001 1:1");
}

TEST_CASE("a REQUIRE inside an IF is left to ORE, which knows whether it runs", check_tags) {
    const std::string text = "IF Flag == 1 THEN\n"
                             "  REQUIRE Strike > 100;\n"
                             "END;\n"
                             "value = PAY(Strike * Flag, Expiry, Expiry, PayCcy);\n";
    const std::vector<script_variable> variables = {
        {.name = "Flag", .type = "Number", .values = {"0"}},
        {.name = "Strike", .type = "Number", .values = {"50"}},
        {.name = "Expiry", .type = "Event", .values = {"2027-01-01"}},
        {.name = "PayCcy", .type = "Currency", .values = {"USD"}}};
    CHECK(findings(text, variables).empty());
}

TEST_CASE("a REQUIRE over a name the script assigns is left to ORE", check_tags) {
    const std::string text = "Level = Level * 2;\n"
                             "REQUIRE Level < 100;\n"
                             "value = PAY(Level, Expiry, Expiry, PayCcy);\n";
    const std::vector<script_variable> variables = {
        {.name = "Level", .type = "Number", .values = {"80"}},
        {.name = "Expiry", .type = "Event", .values = {"2027-01-01"}},
        {.name = "PayCcy", .type = "Currency", .values = {"USD"}}};
    // The script writes Level, so it is not an input, and the REQUIRE reads the
    // script's value, not the trade's.
    std::vector<std::string> seen;
    for (const auto& d : findings(text, variables))
        seen.push_back(summary(d));
    CHECK(seen == std::vector<std::string>{"SCR2002 Level"});
}

TEST_CASE("a REQUIRE is evaluated as ORE evaluates it", check_tags) {
    const std::string text =
        "REQUIRE Weight1 + Weight2 == 0.3;\n"
        "REQUIRE Notional / Units > 0;\n"
        "value = PAY(Notional * Units * Weight1 * Weight2, Expiry, Expiry, PayCcy);\n";
    const std::vector<script_variable> variables = {
        {.name = "Weight1", .type = "Number", .values = {"0.1"}},
        {.name = "Weight2", .type = "Number", .values = {"0.2"}},
        {.name = "Notional", .type = "Number", .values = {"0"}},
        {.name = "Units", .type = "Number", .values = {"0"}},
        {.name = "Expiry", .type = "Event", .values = {"2027-01-01"}},
        {.name = "PayCcy", .type = "Currency", .values = {"USD"}}};
    // 0.1 + 0.2 equals 0.3 within QuantLib's close_enough, as ORE compares, and a
    // zero divisor leaves the second statement to ORE.
    CHECK(findings(text, variables).empty());
}

TEST_CASE("a variable of the wrong type and shape has both findings", check_tags) {
    const std::string text = "value = PAY(1, Expiry, Expiry, PayCcy);\n";
    const std::vector<script_variable> variables = {
        {.name = "Expiry", .type = "Number", .is_array = true, .values = {"1", "2"}},
        {.name = "PayCcy", .type = "Currency", .values = {"USD"}}};
    std::vector<std::string> seen;
    for (const auto& d : findings(text, variables))
        seen.push_back(summary(d));
    CHECK(seen == std::vector<std::string>{"SCR2003 Expiry", "SCR2004 Expiry"});
}
