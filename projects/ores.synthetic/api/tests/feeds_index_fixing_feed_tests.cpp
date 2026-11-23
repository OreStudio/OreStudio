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
#include "ores.analytics.quant/domain/market_board.hpp"
#include "ores.analytics.quant/service/run_clock.hpp"
#include "ores.analytics.quant/service/run_market.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.synthetic.api/feeds/index_fixing_feed.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <set>
#include <string>
#include <thread>

namespace {

using namespace std::chrono;
using namespace ores::synthetic::feed;
using ores::analytics::quant::domain::market_member_kind;
using ores::platform::time::time_of_day;

const std::string index_fixing_feed_tags("[feeds][index_fixing]");

/// Friday 20 November 2026, when New York is five hours behind UTC.
constexpr year_month_day a_cut_friday{year{2026}, November, day{20}};

system_clock::time_point utc_at(year_month_day date, hours h, minutes m = {}, seconds s = {}) {
    return sys_days{date} + h + m + s;
}

const fixing_time new_york_ten_cut{.at = time_of_day(hours{10}, minutes{0}),
                                   .zone = "America/New_York"};

}

TEST_CASE("an_fx_fixing_falls_due_at_its_cut_on_the_cut_zones_clock", index_fixing_feed_tags) {
    CHECK_FALSE(fixing_due(new_york_ten_cut, utc_at(a_cut_friday, 14h, 59min, 59s), std::nullopt));
    CHECK(fixing_due(new_york_ten_cut, utc_at(a_cut_friday, 15h), std::nullopt) == a_cut_friday);
}

TEST_CASE("an_fx_cut_follows_its_zone_across_a_daylight_saving_change", index_fixing_feed_tags) {
    // New York leaves daylight saving on 1 November 2026, so its 10:00 is 14:00 UTC on the
    // Friday before and 15:00 UTC on the Monday after.
    constexpr year_month_day before{year{2026}, October, day{30}};
    constexpr year_month_day after{year{2026}, November, day{2}};
    CHECK_FALSE(fixing_due(new_york_ten_cut, utc_at(before, 13h, 59min), std::nullopt));
    CHECK(fixing_due(new_york_ten_cut, utc_at(before, 14h), std::nullopt) == before);
    CHECK_FALSE(fixing_due(new_york_ten_cut, utc_at(after, 14h, 30min), before));
    CHECK(fixing_due(new_york_ten_cut, utc_at(after, 15h), before) == after);
}

TEST_CASE("a_fixing_is_due_once_a_day_and_never_at_a_weekend", index_fixing_feed_tags) {
    CHECK_FALSE(fixing_due(new_york_ten_cut, utc_at(a_cut_friday, 16h), a_cut_friday));
    const year_month_day saturday{sys_days{a_cut_friday} + days{1}};
    CHECK_FALSE(fixing_due(new_york_ten_cut, utc_at(saturday, 16h), a_cut_friday));
    const fixing_time daily{};
    CHECK(fixing_due(daily, utc_at(a_cut_friday, 0h, 1min), std::nullopt) == a_cut_friday);
    CHECK_FALSE(fixing_due(daily, utc_at(saturday, 9h), a_cut_friday));
}

TEST_CASE("an_fx_fixing_is_the_pairs_spot_at_the_cut", index_fixing_feed_tags) {
    manual_run_clock clock(utc_at(a_cut_friday, 14h));
    ores::analytics::quant::service::run_market market(clock, {}, 7);
    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();
    REQUIRE(nats.is_connected());

    const std::string subject = "test.synthetic.fixings.eurusd";
    auto ticks = nats.subscribe_buffered(subject, 64);
    const auto spot = market.values().member_of(market_member_kind::spot, "synthetic.test.eurusd");

    index_fixing_feed feed(
        nats,
        index_fixing_feed_input{
            .source_name = "synthetic.test.fixing.eurusd",
            .target = {.id = {.id_scheme = "oresmd",
                              .external_id =
                                  "oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=USD",
                              .field = "value"},
                       .subject = subject,
                       .provider_code = "simulated-fixings",
                       .faults = {}},
            .item = "oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=USD",
            .level_kind = market_member_kind::spot,
            .level_code = "synthetic.test.eurusd",
            .when = new_york_ten_cut,
            .history_days = 3,
            .run_seed = 42,
            .held =
                [](const std::string&, year_month_day) {
                    return std::expected<std::set<year_month_day>, std::string>{};
                }},
        market);

    market.values().publish_spot(spot, 1.0842, utc_at(a_cut_friday, 14h));
    CHECK(feed.tick({.elapsed_seconds = 1.0, .observed_at = utc_at(a_cut_friday, 14h)}) == "3");

    market.values().publish_spot(spot, 1.09, utc_at(a_cut_friday, 15h));
    CHECK(feed.tick({.elapsed_seconds = 1.0, .observed_at = utc_at(a_cut_friday, 15h, 0min, 1s)}) ==
          "1");
    CHECK(feed.tick({.elapsed_seconds = 1.0, .observed_at = utc_at(a_cut_friday, 16h)}) == "0");

    for (int i = 0; i < 50 && ticks.size() < 4; ++i)
        std::this_thread::sleep_for(milliseconds(100));
    const auto sent = ticks.snapshot();
    REQUIRE(sent.size() == 4);
    const auto last =
        ores::nats::default_wire_codec().decode<ores::marketdata::messaging::adapter_tick>(
            sent.back().data);
    REQUIRE(last);
    CHECK(last->value == "1.090000");
    CHECK(last->vendor_time == utc_at(a_cut_friday, 12h));
    const auto first_history_day =
        ores::nats::default_wire_codec().decode<ores::marketdata::messaging::adapter_tick>(
            sent[2].data);
    REQUIRE(first_history_day);
    CHECK(first_history_day->vendor_time ==
          utc_at(year_month_day{sys_days{a_cut_friday} - days{1}}, 12h));
}
