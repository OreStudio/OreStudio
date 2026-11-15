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
#include "ores.marketdata.core/service/curve_build_planner.hpp"
#include "test_suite.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

/**
 * @file curve_build_planner_tests.cpp
 * @brief Orders the curves of a currency by the edge each projection declares,
 * and refuses what cannot be ordered.
 *
 * The planner is pure, so every case here states the set and asserts the levels
 * it plans: no database, no service, no clock.
 */

namespace {

using ores::marketdata::service::curve_build_node;
using ores::marketdata::service::curve_build_refusal_kind;
using ores::marketdata::service::plan_curve_builds;

constexpr const char* planner_tags = "[curve_build_planner]";

curve_build_node funding(const std::string& id) {
    return curve_build_node{id, {}};
}

curve_build_node projection(const std::string& id, const std::string& discounts_off) {
    return curve_build_node{id, {discounts_off}};
}

}

TEST_CASE("a_projection_curve_is_planned_after_the_curve_it_discounts_off", planner_tags) {
    auto lg(make_logger(test_suite));

    const std::vector<curve_build_node> curves{projection("USD-LIBOR-3M", "USD-OIS"),
                                               projection("USD-LIBOR-6M", "USD-OIS"),
                                               funding("USD-OIS")};
    BOOST_LOG_SEV(lg, info) << "planning " << curves.size() << " curve(s)";

    const auto plan = plan_curve_builds(curves);
    REQUIRE(plan.has_value());
    REQUIRE(plan->levels.size() == 2);
    CHECK(plan->levels[0] == std::vector<std::string>{"USD-OIS"});
    CHECK(plan->levels[1] == std::vector<std::string>{"USD-LIBOR-3M", "USD-LIBOR-6M"});
    CHECK(plan->unbuildable.empty());
}

TEST_CASE("a_projection_whose_funding_curve_is_not_in_the_set_is_unbuildable", planner_tags) {
    auto lg(make_logger(test_suite));

    const std::vector<curve_build_node> curves{funding("USD-OIS"),
                                               projection("USD-LIBOR-3M", "USD-EURIBOR")};
    BOOST_LOG_SEV(lg, info) << "planning " << curves.size() << " curve(s)";

    const auto plan = plan_curve_builds(curves);
    REQUIRE(plan.has_value());
    REQUIRE(plan->levels.size() == 1);
    CHECK(plan->levels[0] == std::vector<std::string>{"USD-OIS"});
    REQUIRE(plan->unbuildable.size() == 1);
    CHECK(plan->unbuildable[0].node_id == "USD-LIBOR-3M");
    CHECK(plan->unbuildable[0].input_id == "USD-EURIBOR");
}

TEST_CASE("a_curve_reading_an_unbuildable_curve_is_unbuildable_too", planner_tags) {
    auto lg(make_logger(test_suite));

    // USD-LIBOR-3M reads a curve the set does not hold, so neither it nor the two
    // curves that read it can build -- an artefact with no set is not an input.
    const std::vector<curve_build_node> curves{
        funding("USD-OIS"),
        projection("USD-LIBOR-3M", "GBP-OIS"),
        projection("USD-LIBOR-3M-BASIS", "USD-LIBOR-3M"),
        projection("USD-LIBOR-6M-BASIS", "USD-LIBOR-3M-BASIS")};
    BOOST_LOG_SEV(lg, info) << "planning " << curves.size() << " curve(s)";

    const auto plan = plan_curve_builds(curves);
    REQUIRE(plan.has_value());
    REQUIRE(plan->levels.size() == 1);
    CHECK(plan->levels[0] == std::vector<std::string>{"USD-OIS"});
    REQUIRE(plan->unbuildable.size() == 3);
    CHECK(plan->unbuildable[0].node_id == "USD-LIBOR-3M");
    CHECK(plan->unbuildable[0].input_id == "GBP-OIS");
    CHECK(plan->unbuildable[1].node_id == "USD-LIBOR-3M-BASIS");
    CHECK(plan->unbuildable[1].input_id == "USD-LIBOR-3M");
    CHECK(plan->unbuildable[2].node_id == "USD-LIBOR-6M-BASIS");
    CHECK(plan->unbuildable[2].input_id == "USD-LIBOR-3M-BASIS");
}

TEST_CASE("each_level_holds_the_curves_the_level_before_it_frees", planner_tags) {
    auto lg(make_logger(test_suite));

    const std::vector<curve_build_node> curves{projection("USD-LIBOR-6M", "USD-LIBOR-3M"),
                                               projection("USD-LIBOR-3M", "USD-OIS"),
                                               funding("USD-OIS")};
    BOOST_LOG_SEV(lg, info) << "planning " << curves.size() << " curve(s)";

    const auto plan = plan_curve_builds(curves);
    REQUIRE(plan.has_value());
    REQUIRE(plan->levels.size() == 3);
    CHECK(plan->levels[0] == std::vector<std::string>{"USD-OIS"});
    CHECK(plan->levels[1] == std::vector<std::string>{"USD-LIBOR-3M"});
    CHECK(plan->levels[2] == std::vector<std::string>{"USD-LIBOR-6M"});
}

TEST_CASE("a_cycle_is_refused_by_the_plan_not_discovered_by_a_retry", planner_tags) {
    auto lg(make_logger(test_suite));

    const std::vector<curve_build_node> curves{funding("USD-OIS"),
                                               projection("USD-LIBOR-3M", "USD-LIBOR-3M")};
    BOOST_LOG_SEV(lg, info) << "planning " << curves.size() << " curve(s)";

    const auto plan = plan_curve_builds(curves);
    REQUIRE_FALSE(plan.has_value());
    CHECK(plan.error().kind == curve_build_refusal_kind::cycle);
    CHECK(plan.error().node_ids == std::vector<std::string>{"USD-LIBOR-3M"});
}

TEST_CASE("two_curves_that_discount_off_each_other_are_refused", planner_tags) {
    auto lg(make_logger(test_suite));

    const std::vector<curve_build_node> curves{projection("USD-LIBOR-3M", "USD-LIBOR-6M"),
                                               projection("USD-LIBOR-6M", "USD-LIBOR-3M"),
                                               funding("USD-OIS")};
    BOOST_LOG_SEV(lg, info) << "planning " << curves.size() << " curve(s)";

    const auto plan = plan_curve_builds(curves);
    REQUIRE_FALSE(plan.has_value());
    CHECK(plan.error().kind == curve_build_refusal_kind::cycle);
    CHECK(plan.error().node_ids == std::vector<std::string>{"USD-LIBOR-3M", "USD-LIBOR-6M"});
}

TEST_CASE("a_set_that_names_one_curve_twice_is_refused", planner_tags) {
    auto lg(make_logger(test_suite));

    const std::vector<curve_build_node> curves{funding("USD-OIS"), funding("USD-OIS")};
    BOOST_LOG_SEV(lg, info) << "planning " << curves.size() << " curve(s)";

    const auto plan = plan_curve_builds(curves);
    REQUIRE_FALSE(plan.has_value());
    CHECK(plan.error().kind == curve_build_refusal_kind::repeated_node);
    CHECK(plan.error().node_ids == std::vector<std::string>{"USD-OIS"});
}

TEST_CASE("a_curve_that_reads_nothing_is_the_whole_first_level", planner_tags) {
    auto lg(make_logger(test_suite));

    const std::vector<curve_build_node> curves{
        funding("USD-OIS"), funding("GBP-OIS"), funding("EUR-EURIBOR-3M")};
    BOOST_LOG_SEV(lg, info) << "planning " << curves.size() << " curve(s)";

    const auto plan = plan_curve_builds(curves);
    REQUIRE(plan.has_value());
    REQUIRE(plan->levels.size() == 1);
    CHECK(plan->levels[0] == std::vector<std::string>{"USD-OIS", "GBP-OIS", "EUR-EURIBOR-3M"});
}

TEST_CASE("an_empty_set_plans_no_level", planner_tags) {
    auto lg(make_logger(test_suite));

    const std::vector<curve_build_node> curves{};
    BOOST_LOG_SEV(lg, info) << "planning " << curves.size() << " curve(s)";

    const auto plan = plan_curve_builds(curves);
    REQUIRE(plan.has_value());
    CHECK(plan->levels.empty());
    CHECK(plan->unbuildable.empty());
}
