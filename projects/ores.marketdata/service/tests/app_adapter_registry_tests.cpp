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
#include "ores.marketdata.service/app/adapter_registry.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

using namespace std::chrono_literals;
using ores::marketdata::messaging::adapter_announce_request;
using ores::marketdata::messaging::adapter_demand_response;
using ores::marketdata::messaging::rejected_id;
using ores::marketdata::messaging::vendor_id;
using ores::marketdata::service::app::adapter_registry;
using ores::marketdata::service::app::adapter_state;
using ores::marketdata::service::app::universe_check;

vendor_id id(const std::string& external) {
    return vendor_id{"bbg_ticker", external, "PX_LAST"};
}

universe_check app_adapter_registry_catalogue() {
    return universe_check{.provider_known = [](const std::string& p) { return p == "bloomberg"; },
                          .id_known =
                              [](const std::string&, const vendor_id& v) {
                                  return v.external_id == "EURUSD Curncy" ||
                                         v.external_id == "GBPUSD Curncy";
                              }};
}

adapter_announce_request announce(const std::string& provider = "bloomberg") {
    adapter_announce_request r;
    r.adapter_id = "bloomberg-sim";
    r.provider_code = provider;
    r.universe = {id("EURUSD Curncy"), id("GBPUSD Curncy"), id("XAUUSD Curncy")};
    return r;
}

const auto t0 = std::chrono::system_clock::now();

}

TEST_CASE("an_adapter_of_a_known_provider_is_ready_after_its_announce", "[app][adapter_registry]") {
    adapter_registry sut;

    const auto response = sut.announce(announce(), app_adapter_registry_catalogue(), t0);

    CHECK(response.success);
    CHECK(response.served_count == 2);
    CHECK(response.unknown_count == 1);
    CHECK(sut.state_of("bloomberg-sim") == adapter_state::ready);
    CHECK(sut.provider_of("bloomberg-sim") == "bloomberg");
}

TEST_CASE("an_adapter_of_an_unknown_provider_is_rejected", "[app][adapter_registry]") {
    adapter_registry sut;

    const auto response = sut.announce(announce("nobody"), app_adapter_registry_catalogue(), t0);

    CHECK_FALSE(response.success);
    CHECK_FALSE(response.message.empty());
    CHECK(sut.state_of("bloomberg-sim") == adapter_state::rejected);
    CHECK(sut.demandable().empty());
}

TEST_CASE("an_announce_with_no_adapter_id_is_refused", "[app][adapter_registry]") {
    adapter_registry sut;
    auto request = announce();
    request.adapter_id.clear();

    CHECK_FALSE(sut.announce(request, app_adapter_registry_catalogue(), t0).success);
    CHECK(sut.status().empty());
}

TEST_CASE("an_accepted_demand_makes_an_adapter_serve_and_an_empty_one_makes_it_ready",
          "[app][adapter_registry]") {
    adapter_registry sut;
    sut.announce(announce(), app_adapter_registry_catalogue(), t0);

    adapter_demand_response accepted;
    accepted.accepted = {id("EURUSD Curncy")};
    accepted.rejected = {rejected_id{id("XAUUSD Curncy"), "unknown_id"}};
    sut.demand_replied("bloomberg-sim", 2, accepted);
    CHECK(sut.state_of("bloomberg-sim") == adapter_state::serving);

    sut.demand_replied("bloomberg-sim", 0, adapter_demand_response{});
    CHECK(sut.state_of("bloomberg-sim") == adapter_state::ready);
}

TEST_CASE("the_status_shows_the_accepted_and_rejected_ids_of_the_last_reply",
          "[app][adapter_registry]") {
    adapter_registry sut;
    sut.announce(announce(), app_adapter_registry_catalogue(), t0);
    adapter_demand_response reply;
    reply.accepted = {id("EURUSD Curncy")};
    reply.rejected = {rejected_id{id("XAUUSD Curncy"), "not_entitled"}};
    sut.demand_replied("bloomberg-sim", 2, reply);

    const auto status = sut.status();

    REQUIRE(status.size() == 1);
    CHECK(status.front().state == "serving");
    CHECK(status.front().demanded_count == 2);
    REQUIRE(status.front().accepted.size() == 1);
    REQUIRE(status.front().rejected.size() == 1);
    CHECK(status.front().rejected.front().reason == "not_entitled");
    CHECK(status.front().served_count == 2);
    CHECK(status.front().unknown_count == 1);
}

TEST_CASE("a_missed_heartbeat_makes_an_adapter_stale", "[app][adapter_registry]") {
    adapter_registry sut(15s);
    sut.announce(announce(), app_adapter_registry_catalogue(), t0);
    adapter_demand_response accepted;
    accepted.accepted = {id("EURUSD Curncy")};
    sut.demand_replied("bloomberg-sim", 1, accepted);

    CHECK(sut.expire(t0 + 14s).empty());
    const auto stale = sut.expire(t0 + 16s);

    REQUIRE(stale.size() == 1);
    CHECK(stale.front() == "bloomberg-sim");
    CHECK(sut.state_of("bloomberg-sim") == adapter_state::stale);
    CHECK(sut.demandable().empty());
    CHECK(sut.expire(t0 + 30s).empty());
}

TEST_CASE("a_heartbeat_keeps_an_adapter_from_going_stale", "[app][adapter_registry]") {
    adapter_registry sut(15s);
    sut.announce(announce(), app_adapter_registry_catalogue(), t0);

    const auto outcome = sut.heartbeat("bloomberg-sim", 1, t0 + 10s);

    CHECK(outcome.known);
    CHECK_FALSE(outcome.recovered);
    CHECK(sut.expire(t0 + 20s).empty());
    CHECK(sut.expire(t0 + 26s).size() == 1);
}

TEST_CASE("a_returning_heartbeat_makes_a_stale_adapter_ready_and_asks_for_demand",
          "[app][adapter_registry]") {
    adapter_registry sut(15s);
    sut.announce(announce(), app_adapter_registry_catalogue(), t0);
    (void)sut.expire(t0 + 20s);
    REQUIRE(sut.state_of("bloomberg-sim") == adapter_state::stale);

    const auto outcome = sut.heartbeat("bloomberg-sim", 7, t0 + 21s);

    CHECK(outcome.known);
    CHECK(outcome.recovered);
    CHECK(sut.state_of("bloomberg-sim") == adapter_state::ready);
    CHECK(sut.status().front().heartbeat_counter == 7);
}

TEST_CASE("a_heartbeat_of_an_unknown_adapter_changes_nothing", "[app][adapter_registry]") {
    adapter_registry sut;

    const auto outcome = sut.heartbeat("nobody", 1, t0);

    CHECK_FALSE(outcome.known);
    CHECK(sut.status().empty());
}

TEST_CASE("a_rejected_adapter_never_goes_stale_or_serves", "[app][adapter_registry]") {
    adapter_registry sut(15s);
    sut.announce(announce("nobody"), app_adapter_registry_catalogue(), t0);

    CHECK(sut.expire(t0 + 60s).empty());
    adapter_demand_response accepted;
    accepted.accepted = {id("EURUSD Curncy")};
    sut.demand_replied("bloomberg-sim", 1, accepted);

    CHECK(sut.state_of("bloomberg-sim") == adapter_state::rejected);
}

TEST_CASE("a_stale_adapter_keeps_its_last_reply", "[app][adapter_registry]") {
    adapter_registry sut(15s);
    sut.announce(announce(), app_adapter_registry_catalogue(), t0);
    adapter_demand_response accepted;
    accepted.accepted = {id("EURUSD Curncy")};
    sut.demand_replied("bloomberg-sim", 1, accepted);
    (void)sut.expire(t0 + 20s);

    sut.demand_replied("bloomberg-sim", 0, adapter_demand_response{});

    CHECK(sut.state_of("bloomberg-sim") == adapter_state::stale);
    CHECK(sut.status().front().accepted.size() == 1);
}

TEST_CASE("a_second_announce_starts_the_adapter_again", "[app][adapter_registry]") {
    adapter_registry sut;
    sut.announce(announce(), app_adapter_registry_catalogue(), t0);
    adapter_demand_response accepted;
    accepted.accepted = {id("EURUSD Curncy")};
    sut.demand_replied("bloomberg-sim", 1, accepted);

    sut.announce(announce(), app_adapter_registry_catalogue(), t0 + 1s);

    CHECK(sut.state_of("bloomberg-sim") == adapter_state::ready);
    CHECK(sut.status().front().accepted.empty());
}
