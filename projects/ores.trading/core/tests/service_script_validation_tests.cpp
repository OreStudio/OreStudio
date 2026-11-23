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
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.trading.core/service/script_validation_service.hpp"
#include "ores.trading.scripting/inputs.hpp"
#include "ores.trading.scripting/parse.hpp"
#include <catch2/catch_test_macros.hpp>
#include <algorithm>
#include <string>
#include <vector>

using namespace ores::trading;

namespace {

const std::string payoff = "Option = PAY(Notional * 5, Expiry, Expiry, PayCcy);";

/// A value of the type ORE reads, so the variable passes the type check.
std::string value_of(const std::string& type) {
    if (type == "Event")
        return "2027-06-30";
    if (type == "Currency")
        return "EUR";
    return "1000";
}

/// The variables a trade states for every input of the payoff, each well typed.
std::vector<messaging::script_variable_value> stated_inputs() {
    const auto parsed = scripting::parse(payoff);
    std::vector<messaging::script_variable_value> out;
    for (const auto& i : scripting::inputs_of(parsed.tree, payoff))
        out.push_back({.name = i.name,
                       .type = i.type,
                       .is_array = i.is_array,
                       .values = {value_of(i.type)}});
    return out;
}

std::vector<service::script_text> one(const std::string& code) {
    return {{.purpose = "default", .code = code}};
}

long count_of(const messaging::validate_script_response& r, const std::string& severity) {
    return std::ranges::count(r.diagnostics, severity, &messaging::script_diagnostic::severity);
}

}

TEST_CASE("a script that does not parse is invalid, located where ORE stopped",
          "[script_validation]") {
    const auto out = service::validate_script(one("Option = PAY(Notional *, Expiry);"), {});

    REQUIRE(out.success);
    CHECK_FALSE(out.is_valid);
    REQUIRE(out.diagnostics.size() == 1);
    CHECK(out.diagnostics[0].severity == "error");
    CHECK(out.diagnostics[0].line == 1);
    CHECK(out.diagnostics[0].purpose == "default");
    CHECK(out.diagnostics[0].subject.empty());
}

TEST_CASE("an input the trade does not state is an error about it, reported once for every variant",
          "[script_validation]") {
    const std::vector<service::script_text> variants{{.purpose = "default", .code = payoff},
                                                     {.purpose = "AD", .code = payoff}};
    auto variables = stated_inputs();
    std::erase_if(variables, [](const auto& v) { return v.name == "Notional"; });

    const auto out = service::validate_script(variants, variables);

    REQUIRE(out.success);
    CHECK_FALSE(out.is_valid);
    const auto about_notional = std::ranges::count_if(
        out.diagnostics, [](const auto& d) { return d.subject == "Notional"; });
    CHECK(about_notional == 1);
}

TEST_CASE("a script whose every input is stated and well typed is valid", "[script_validation]") {
    const auto out = service::validate_script(one(payoff), stated_inputs());

    REQUIRE(out.success);
    CHECK(out.is_valid);
    CHECK(count_of(out, "error") == 0);
}

TEST_CASE("a variable the script never reads is a warning, and warnings alone are valid",
          "[script_validation]") {
    auto variables = stated_inputs();
    variables.push_back({.name = "Unused", .type = "Number", .values = {"1"}});

    const auto out = service::validate_script(one(payoff), variables);

    REQUIRE(out.success);
    CHECK(out.is_valid);
    CHECK(count_of(out, "warning") >= 1);
    CHECK(std::ranges::any_of(out.diagnostics, [](const auto& d) {
        return d.subject == "Unused" && d.severity == "warning";
    }));
}

TEST_CASE("a library script resolves by name and version, and is checked against the trade",
          "[script_validation][database]") {
    ores::testing::scoped_database_helper h;

    const auto current = service::library_variants(h.context(), "Accumulator01", 0);
    REQUIRE(current.has_value());
    REQUIRE_FALSE(current->empty());
    CHECK_FALSE(service::library_variants(h.context(), "Accumulator01", 999999).has_value());
    CHECK_FALSE(service::library_variants(h.context(), "NoSuchScript", 0).has_value());

    const auto out = service::validate_script(*current, {});
    REQUIRE(out.success);
    CHECK_FALSE(out.is_valid);
    CHECK(std::ranges::any_of(out.diagnostics, [](const auto& d) { return !d.subject.empty(); }));
}
