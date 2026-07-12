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
#include "ores.analytics.quant/domain/topology_build_error.hpp"
#include "ores.analytics.quant/service/topology_builder.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>

using ores::analytics::quant::domain::ccy_pair_input;
using ores::analytics::quant::domain::topology_build_error;
using ores::analytics::quant::domain::topology_error_kind;
using ores::analytics::quant::service::topology_builder;

namespace {

std::vector<ccy_pair_input> majors_spanning_tree() {
    return {
        {"EUR", "USD", true},
        {"USD", "JPY", true},
        {"GBP", "USD", true},
        {"USD", "CHF", true},
    };
}

} // namespace

TEST_CASE("builds a valid spanning tree from majors", "[topology_builder]") {
    const auto topology =
        topology_builder::build(majors_spanning_tree(), "USD", {"EUR", "GBP", "JPY", "CHF"});

    CHECK(topology.vertex_count() == 5);
    CHECK(topology.currency_id_for("EUR").has_value());
    CHECK(topology.currency_id_for("SEK").has_value() == false);

    const auto eur = *topology.currency_id_for("EUR");
    const auto path = topology.path_to_pivot(eur);
    REQUIRE(path.size() == 1);
    CHECK(topology.code_of(path.front().base) == "EUR");
    CHECK(topology.code_of(path.front().quote) == "USD");
}

TEST_CASE("the driver/derived direction survives into the built topology", "[topology_builder]") {
    const std::vector<ccy_pair_input> pairs = {
        {"EUR", "USD", true},
        {"USD", "JPY", false}, // a derived-only edge, not a driver
    };
    const auto topology = topology_builder::build(pairs, "USD", {"EUR", "JPY"});

    const auto eur = *topology.currency_id_for("EUR");
    const auto eur_path = topology.path_to_pivot(eur);
    REQUIRE(eur_path.size() == 1);
    CHECK(eur_path.front().is_driver == true);

    const auto jpy = *topology.currency_id_for("JPY");
    const auto jpy_path = topology.path_to_pivot(jpy);
    REQUIRE(jpy_path.size() == 1);
    CHECK(jpy_path.front().is_driver == false);
}

TEST_CASE("multi-hop path resolves through the pivot", "[topology_builder]") {
    const auto topology =
        topology_builder::build(majors_spanning_tree(), "USD", {"EUR", "GBP", "JPY", "CHF"});

    // EUR/JPY has no direct edge; it must triangulate through USD.
    const auto jpy = *topology.currency_id_for("JPY");
    const auto path = topology.path_to_pivot(jpy);
    REQUIRE(path.size() == 1);
    CHECK(topology.code_of(path.front().base) == "USD");
    CHECK(topology.code_of(path.front().quote) == "JPY");
}

TEST_CASE("a second path between two currencies throws at build time, never resolved silently",
          "[topology_builder]") {
    auto pairs = majors_spanning_tree();
    // EUR is already connected to USD via the first edge; this adds a
    // second, independent path EUR -> GBP -> ... -> EUR, i.e. a cycle.
    pairs.push_back({"EUR", "GBP", true});

    bool threw = false;
    try {
        [[maybe_unused]] const auto topology =
            topology_builder::build(pairs, "USD", {"EUR", "GBP"});
    } catch (const topology_build_error& error) {
        threw = true;
        REQUIRE(error.errors().size() == 1);
        CHECK(error.errors().front().kind == topology_error_kind::cycle_conflict);
    }
    CHECK(threw);
}

TEST_CASE("a missing major produces a readable error", "[topology_builder]") {
    const std::vector<ccy_pair_input> pairs = {{"EUR", "USD", true}};

    bool threw = false;
    try {
        [[maybe_unused]] const auto topology =
            topology_builder::build(pairs, "USD", {"EUR", "JPY"});
    } catch (const topology_build_error& error) {
        threw = true;
        const auto& errors = error.errors();
        // Exactly one error for JPY -- missing_major, not also
        // disconnected_currency for the same currency.
        REQUIRE(errors.size() == 1);
        CHECK(errors.front().kind == topology_error_kind::missing_major);
        CHECK(errors.front().base_code == "JPY");
        CHECK(errors.front().describe().find("JPY") != std::string::npos);
    }
    CHECK(threw);
}

TEST_CASE("a duplicate pair in the input is rejected", "[topology_builder]") {
    const std::vector<ccy_pair_input> pairs = {
        {"EUR", "USD", true},
        {"EUR", "USD", true},
    };

    bool threw = false;
    try {
        [[maybe_unused]] const auto topology = topology_builder::build(pairs, "USD", {"EUR"});
    } catch (const topology_build_error& error) {
        threw = true;
        CHECK(error.errors().front().kind == topology_error_kind::duplicate_edge);
    }
    CHECK(threw);
}

TEST_CASE("a disconnected currency not on the majors list is still reported",
          "[topology_builder]") {
    std::vector<ccy_pair_input> pairs = majors_spanning_tree();
    pairs.push_back({"AUD", "NZD", true}); // an island, disconnected from USD

    bool threw = false;
    try {
        [[maybe_unused]] const auto topology = topology_builder::build(pairs, "USD", {"EUR"});
    } catch (const topology_build_error& error) {
        threw = true;
        const auto& errors = error.errors();
        const bool has_aud = std::any_of(errors.begin(), errors.end(), [](const auto& e) {
            return e.kind == topology_error_kind::disconnected_currency && e.base_code == "AUD";
        });
        const bool has_nzd = std::any_of(errors.begin(), errors.end(), [](const auto& e) {
            return e.kind == topology_error_kind::disconnected_currency && e.base_code == "NZD";
        });
        CHECK(has_aud);
        CHECK(has_nzd);
    }
    CHECK(threw);
}
