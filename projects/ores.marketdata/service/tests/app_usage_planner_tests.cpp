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
#include "ores.marketdata.service/app/usage_planner.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

using namespace std::chrono_literals;
using ores::marketdata::service::app::demand_catalogue;
using ores::marketdata::service::app::demand_state;
using ores::marketdata::service::app::plan_demand_intervals;
using ores::marketdata::service::app::plan_subscription_intervals;
using ores::marketdata::service::app::subscription_state;

struct app_usage_planner_fixture {
    boost::uuids::random_generator gen;
    std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
    boost::uuids::uuid party = gen();
    boost::uuids::uuid sim = gen();
    boost::uuids::uuid live = gen();
    std::string actor = "ores_test_marketdata_service";

    subscription_state state(const boost::uuids::uuid& source, bool derived = false) const {
        subscription_state s;
        s.party_id = party;
        s.provider_code = "bloomberg";
        s.source_id = source;
        s.display = true;
        s.derived = derived;
        return s;
    }

    ores::marketdata::domain::subscription_interval
    open_interval(const subscription_state& s) const {
        ores::marketdata::domain::subscription_interval i;
        i.id = boost::uuids::random_generator{}();
        i.tenant_id = s.tenant_id;
        i.party_id = s.party_id;
        i.provider_code = s.provider_code;
        i.source_id = s.source_id;
        i.display = s.display;
        i.non_display = s.non_display;
        i.derived = s.derived;
        i.interval_from = now - 1h;
        return i;
    }

    demand_state item(const std::string& uri) const {
        demand_state d;
        d.party_id = party;
        d.provider_code = "bloomberg";
        d.source_id = sim;
        d.oresmd_uri = uri;
        return d;
    }

    ores::marketdata::domain::demand_interval open_demand(const demand_state& d) const {
        ores::marketdata::domain::demand_interval i;
        i.id = boost::uuids::random_generator{}();
        i.tenant_id = d.tenant_id;
        i.party_id = d.party_id;
        i.provider_code = d.provider_code;
        i.source_id = d.source_id;
        i.oresmd_uri = d.oresmd_uri;
        i.interval_from = now - 1h;
        return i;
    }
};

}

TEST_CASE("a_new_route_with_a_subscription_opens_an_interval", "[app][usage_planner]") {
    app_usage_planner_fixture f;

    const auto plan = plan_subscription_intervals({f.state(f.sim)}, {}, f.now, f.actor);

    REQUIRE(plan.to_open.size() == 1);
    CHECK(plan.to_close.empty());
    CHECK(plan.to_open.front().interval_from == f.now);
    CHECK_FALSE(plan.to_open.front().interval_to.has_value());
    CHECK(plan.to_open.front().source_id == f.sim);
    CHECK(plan.to_open.front().display);
}

TEST_CASE("an_interval_that_still_holds_is_left_alone", "[app][usage_planner]") {
    app_usage_planner_fixture f;
    const auto s = f.state(f.sim);

    const auto plan = plan_subscription_intervals({s}, {f.open_interval(s)}, f.now, f.actor);

    CHECK(plan.to_open.empty());
    CHECK(plan.to_close.empty());
}

TEST_CASE("a_route_that_ended_closes_its_interval", "[app][usage_planner]") {
    app_usage_planner_fixture f;
    const auto open = f.open_interval(f.state(f.sim));

    const auto plan = plan_subscription_intervals({}, {open}, f.now, f.actor);

    REQUIRE(plan.to_close.size() == 1);
    CHECK(plan.to_open.empty());
    CHECK(plan.to_close.front().id == open.id);
    CHECK(plan.to_close.front().interval_to == f.now);
    CHECK(plan.to_close.front().interval_from == open.interval_from);
}

TEST_CASE("a_change_of_source_closes_one_interval_and_opens_the_next", "[app][usage_planner]") {
    app_usage_planner_fixture f;
    const auto open = f.open_interval(f.state(f.sim));

    const auto plan = plan_subscription_intervals({f.state(f.live)}, {open}, f.now, f.actor);

    REQUIRE(plan.to_close.size() == 1);
    REQUIRE(plan.to_open.size() == 1);
    CHECK(plan.to_close.front().source_id == f.sim);
    CHECK(plan.to_open.front().source_id == f.live);
    CHECK(plan.to_open.front().interval_from == plan.to_close.front().interval_to);
}

TEST_CASE("a_change_of_rights_closes_one_interval_and_opens_the_next", "[app][usage_planner]") {
    app_usage_planner_fixture f;
    const auto open = f.open_interval(f.state(f.sim));

    const auto plan = plan_subscription_intervals({f.state(f.sim, true)}, {open}, f.now, f.actor);

    REQUIRE(plan.to_close.size() == 1);
    REQUIRE(plan.to_open.size() == 1);
    CHECK_FALSE(plan.to_close.front().derived);
    CHECK(plan.to_open.front().derived);
}

TEST_CASE("an_interval_that_is_already_closed_is_not_closed_again", "[app][usage_planner]") {
    app_usage_planner_fixture f;
    auto closed = f.open_interval(f.state(f.sim));
    closed.interval_to = f.now - 30min;

    const auto plan = plan_subscription_intervals({}, {closed}, f.now, f.actor);

    CHECK(plan.to_close.empty());
    CHECK(plan.to_open.empty());
}

TEST_CASE("every_interval_carries_the_actor_and_a_reason", "[app][usage_planner]") {
    app_usage_planner_fixture f;
    const auto open = f.open_interval(f.state(f.sim));

    const auto plan = plan_subscription_intervals({f.state(f.live)}, {open}, f.now, f.actor);

    for (const auto& i : plan.to_open) {
        CHECK(i.modified_by == f.actor);
        CHECK(i.performed_by == f.actor);
        CHECK_FALSE(i.change_reason_code.empty());
        CHECK_FALSE(i.change_commentary.empty());
    }
    for (const auto& i : plan.to_close) {
        CHECK(i.modified_by == f.actor);
        CHECK(i.performed_by == f.actor);
        CHECK_FALSE(i.change_reason_code.empty());
    }
}

TEST_CASE("an_item_that_enters_the_demand_opens_an_interval_and_one_that_leaves_closes_it",
          "[app][usage_planner]") {
    app_usage_planner_fixture f;
    const auto eur = f.item("oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD");
    const auto gbp = f.item("oresmd://fx/GBP?type=quote&instrument=fx_spot&quote=rate&ccy=USD");

    const auto plan = plan_demand_intervals({eur}, {f.open_demand(gbp)}, f.now, f.actor);

    REQUIRE(plan.to_open.size() == 1);
    REQUIRE(plan.to_close.size() == 1);
    CHECK(plan.to_open.front().oresmd_uri == eur.oresmd_uri);
    CHECK(plan.to_close.front().oresmd_uri == gbp.oresmd_uri);
    CHECK(plan.to_close.front().interval_to == f.now);
}

TEST_CASE("an_item_that_stays_in_the_demand_keeps_its_interval", "[app][usage_planner]") {
    app_usage_planner_fixture f;
    const auto eur = f.item("oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD");

    const auto plan = plan_demand_intervals({eur}, {f.open_demand(eur)}, f.now, f.actor);

    CHECK(plan.to_open.empty());
    CHECK(plan.to_close.empty());
}

TEST_CASE("the_states_come_from_the_routes_that_hold_a_subscription_with_a_right",
          "[app][usage_planner]") {
    app_usage_planner_fixture f;
    demand_catalogue c;
    ores::marketdata::domain::route routed;
    routed.party_id = f.party;
    routed.provider_code = "bloomberg";
    routed.source_id = f.sim;
    ores::marketdata::domain::route unsubscribed = routed;
    unsubscribed.party_id = f.gen();
    c.routes = {routed, unsubscribed};
    ores::marketdata::domain::subscription s;
    s.party_id = f.party;
    s.provider_code = "bloomberg";
    s.display = true;
    s.derived = true;
    c.subscriptions = {s};

    const auto states = ores::marketdata::service::app::subscription_states(c);

    REQUIRE(states.size() == 1);
    CHECK(states.front().party_id == f.party);
    CHECK(states.front().source_id == f.sim);
    CHECK(states.front().display);
    CHECK(states.front().derived);
    CHECK_FALSE(states.front().non_display);
}

TEST_CASE("a_tenant_whose_close_fails_opens_nothing_until_the_next_sync", "[app][usage_planner]") {
    using ores::marketdata::domain::subscription_interval;
    using ores::marketdata::service::app::interval_plan;
    using ores::marketdata::service::app::write_plan;
    using ores::utility::uuid::tenant_id;

    boost::uuids::random_generator gen;
    const auto failing = tenant_id::from_uuid(gen()).value();
    const auto healthy = tenant_id::from_uuid(gen()).value();
    auto interval_of = [&gen](const tenant_id& tenant) {
        subscription_interval i;
        i.id = gen();
        i.tenant_id = tenant;
        return i;
    };

    interval_plan<subscription_interval> plan;
    plan.to_close = {interval_of(failing)};
    plan.to_open = {interval_of(failing), interval_of(healthy)};

    std::vector<boost::uuids::uuid> written;
    std::size_t reported = 0;
    const auto outcome = write_plan(
        plan,
        [&](const subscription_interval& i) {
            if (i.id == plan.to_close.front().id)
                throw std::runtime_error("the store refused the close");
            written.push_back(i.id);
        },
        [&](const subscription_interval&, const std::exception&) { ++reported; });

    REQUIRE(written.size() == 1);
    CHECK(written.front() == plan.to_open.back().id);
    CHECK(outcome.closed == 0);
    CHECK(outcome.opened == 1);
    CHECK(outcome.failed == 1);
    CHECK(outcome.held == 1);
    CHECK(reported == 1);
}
