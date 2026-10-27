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
#include "../src/fault_handler.hpp"
#include "../src/generator_runner.hpp"
#include "../src/simulator_adapter.hpp"
#include "ores.marketdata.api/domain/adapter_limits.hpp"
#include "ores.marketdata.api/domain/adapter_subjects.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.synthetic.api/feeds/adapter_faults.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "test_helpers.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <memory>
#include <span>
#include <string>
#include <thread>

namespace {

namespace messaging = ores::marketdata::messaging;
namespace domain = ores::marketdata::domain;
using ores::synthetic::feed::fault_plan;
using ores::synthetic::feed::fault_state;
using ores::synthetic::feed::faulted_ticks;
using ores::synthetic::service::fault_handler;
using ores::synthetic::service::generator_offer;
using ores::synthetic::service::generator_runner;
using ores::synthetic::service::simulator_adapter;
using ores::synthetic::service::simulator_adapter_timing;

messaging::adapter_tick tick_at(std::chrono::system_clock::time_point at, std::string value) {
    messaging::adapter_tick tick;
    tick.id_scheme = "bbg_ticker";
    tick.external_id = "EURUSD Curncy";
    tick.field = "PX_LAST";
    tick.value = std::move(value);
    tick.vendor_time = at;
    return tick;
}

}

TEST_CASE("every fault is off by default", "[simulator_faults]") {
    fault_state state;
    CHECK_FALSE(state.get().any());
    CHECK_FALSE(fault_plan{}.any());
}

TEST_CASE("with no fault a tick goes out once and unchanged", "[simulator_faults]") {
    std::optional<messaging::adapter_tick> previous;
    const auto now = std::chrono::system_clock::now();

    const auto out = faulted_ticks(fault_plan{}, tick_at(now, "1.25"), previous);

    REQUIRE(out.size() == 1);
    CHECK(out.front().value == "1.25");
    CHECK(out.front().vendor_time == now);
}

TEST_CASE("an out of range fault sends a value beyond what the service accepts",
          "[simulator_faults]") {
    std::optional<messaging::adapter_tick> previous;
    fault_plan plan;
    plan.out_of_range = true;

    const auto out =
        faulted_ticks(plan, tick_at(std::chrono::system_clock::now(), "1.25"), previous);

    REQUIRE(out.size() == 1);
    CHECK(std::stod(out.front().value) > domain::adapter_limits::max_abs_value);
}

TEST_CASE("a delay fault sends a vendor time older than the age limit", "[simulator_faults]") {
    std::optional<messaging::adapter_tick> previous;
    fault_plan plan;
    plan.tick_delay = domain::adapter_limits::max_tick_age + std::chrono::milliseconds(1);
    const auto now = std::chrono::system_clock::now();

    const auto out = faulted_ticks(plan, tick_at(now, "1.25"), previous);

    REQUIRE(out.size() == 1);
    CHECK(now - out.front().vendor_time > domain::adapter_limits::max_tick_age);
}

TEST_CASE("a lost connection delivers no tick", "[simulator_faults]") {
    std::optional<messaging::adapter_tick> previous;
    fault_plan plan;
    plan.connection_lost = true;

    CHECK(faulted_ticks(plan, tick_at(std::chrono::system_clock::now(), "1.25"), previous).empty());
}

TEST_CASE("a repeat fault sends the tick before the new one again, with its older vendor time",
          "[simulator_faults]") {
    std::optional<messaging::adapter_tick> previous;
    fault_plan plan;
    plan.repeat_ticks = true;
    const auto first_time = std::chrono::system_clock::now();
    const auto second_time = first_time + std::chrono::seconds(1);

    const auto first = faulted_ticks(plan, tick_at(first_time, "1.25"), previous);
    CHECK(first.size() == 1);

    const auto second = faulted_ticks(plan, tick_at(second_time, "1.26"), previous);
    REQUIRE(second.size() == 2);
    CHECK(second[0].vendor_time == second_time);
    CHECK(second[1].vendor_time == first_time);
    CHECK(second[1].vendor_time < second[0].vendor_time);
}

TEST_CASE("a not entitled fault rejects every id and runs no generator", "[simulator_faults]") {
    ores::nats::service::client nats(ores::testing::make_nats_options());
    auto faults = std::make_shared<fault_state>();
    generator_runner runner([] { return std::vector<generator_offer>{}; });
    simulator_adapter adapter(nats, runner, "test-faults", "bloomberg", {}, faults);

    messaging::adapter_demand_request request;
    request.ids = {{.id_scheme = "bbg_ticker", .external_id = "EURUSD Curncy", .field = "PX_LAST"}};

    fault_plan plan;
    plan.not_entitled = true;
    faults->set(plan);
    const auto response = adapter.on_demand(request);

    CHECK(response.accepted.empty());
    REQUIRE(response.rejected.size() == 1);
    CHECK(response.rejected.front().reason == "not_entitled");
    CHECK(runner.running_count() == 0);

    faults->set(fault_plan{});
    const auto normal = adapter.on_demand(request);
    CHECK(normal.rejected.front().reason == "unknown_id");
}

TEST_CASE("a lost connection sends no heartbeat and resumes when the fault is off",
          "[simulator_faults]") {
    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();
    const auto adapter_id =
        "test-faults-" + boost::uuids::to_string(boost::uuids::random_generator()());

    std::atomic<int> announces{0};
    std::atomic<int> beats{0};
    auto announce_sub = nats.subscribe(domain::adapter_announce_subject(), [&](auto msg) {
        auto request =
            ores::nats::default_wire_codec().decode<messaging::adapter_announce_request>(msg.data);
        if (!request || request->adapter_id != adapter_id)
            return;
        ++announces;
        messaging::adapter_announce_response response;
        response.success = true;
        ores::service::messaging::reply(nats, msg, response);
    });
    auto beat_sub =
        nats.subscribe(domain::adapter_heartbeat_subject(adapter_id), [&](auto) { ++beats; });

    auto faults = std::make_shared<fault_state>();
    generator_runner runner([] { return std::vector<generator_offer>{}; });
    simulator_adapter adapter(
        nats,
        runner,
        adapter_id,
        "bloomberg",
        simulator_adapter_timing{.heartbeat_interval = std::chrono::milliseconds(20),
                                 .reannounce_interval = std::chrono::milliseconds(40),
                                 .retry_interval = std::chrono::milliseconds(40),
                                 .reply_timeout = std::chrono::milliseconds(200)},
        faults);

    fault_plan lost;
    lost.connection_lost = true;
    faults->set(lost);
    adapter.start();
    std::this_thread::sleep_for(std::chrono::milliseconds(300));
    CHECK(announces == 0);
    CHECK(beats == 0);

    faults->set(fault_plan{});
    CHECK(eventually([&] { return beats > 0 && announces > 0; }));

    // A change of the plan announces the adapter again, so the service sends its demand again.
    const auto before = announces.load();
    fault_plan entitled_off;
    entitled_off.not_entitled = true;
    faults->set(entitled_off);
    CHECK(eventually([&] { return announces > before; }));
    adapter.stop();
}

TEST_CASE("the set_faults request maps to the plan", "[simulator_faults]") {
    ores::synthetic::messaging::set_faults_request request;
    request.connection_lost = true;
    request.tick_delay_ms = 7000;
    request.repeat_ticks = true;
    request.out_of_range = true;
    request.not_entitled = true;

    const auto plan = fault_handler::plan_of(request);

    CHECK(plan.connection_lost);
    CHECK(plan.tick_delay == std::chrono::milliseconds(7000));
    CHECK(plan.repeat_ticks);
    CHECK(plan.out_of_range);
    CHECK(plan.not_entitled);
    CHECK_FALSE(fault_handler::plan_of({}).any());
}
