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
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.shell/app/commands/marketdata_commands.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cli/cli.h>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

using ores::nats::service::nats_client;
using ores::shell::app::commands::history_tick;
using ores::shell::app::commands::marketdata_commands;
using namespace ores::logging;
using namespace std::chrono_literals;

namespace {

const std::string_view test_suite("ores.shell.application.tests");
const std::string tags("[commands][marketdata][ticks]");

const std::string spot_uri("oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD");
const std::string other_uri("oresmd://fx/GBP?type=quote&instrument=fx_spot&quote=rate&ccy=USD");

/// A reply of three ticks over two coordinates, one second then 2.5 seconds apart.
ores::marketdata::messaging::get_series_ticks_response a_reply() {
    ores::marketdata::messaging::get_series_ticks_response reply;
    reply.coordinates = {spot_uri, other_uri};
    reply.first_instant = std::chrono::sys_days{std::chrono::year{2026} / 10 / 23} + 14h + 30min;
    reply.gaps_us = {0, 1000000, 2500000};
    reply.coordinate_indexes = {0, 1, 0};
    reply.values = {1.0845, 1.3, 1.0850};
    return reply;
}

}

TEST_CASE("marketdata_commands registers the ticks verb", tags) {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;

    marketdata_commands::register_commands(root_menu, session);

    const auto completions = root_menu.GetCompletions("marketdata ");
    CHECK(std::find(completions.begin(), completions.end(), std::string{"marketdata ticks"}) !=
          completions.end());
}

TEST_CASE("the_times_of_a_history_reply_are_the_first_instant_plus_the_gaps", tags) {
    auto lg(make_logger(test_suite));

    const auto reply = a_reply();
    const auto ticks = marketdata_commands::history_ticks(reply);

    REQUIRE(ticks.has_value());
    REQUIRE(ticks->size() == 3);
    CHECK((*ticks)[0].at == *reply.first_instant);
    CHECK((*ticks)[1].at == *reply.first_instant + 1s);
    CHECK((*ticks)[2].at == *reply.first_instant + 3500ms);
    CHECK((*ticks)[0].oresmd_uri == spot_uri);
    CHECK((*ticks)[1].oresmd_uri == other_uri);
    CHECK((*ticks)[2].oresmd_uri == spot_uri);
    CHECK((*ticks)[1].value == 1.3);
}

TEST_CASE("a_history_reply_with_no_tick_gives_no_tick", tags) {
    auto lg(make_logger(test_suite));

    const ores::marketdata::messaging::get_series_ticks_response reply;
    const auto ticks = marketdata_commands::history_ticks(reply);

    REQUIRE(ticks.has_value());
    CHECK(ticks->empty());
}

TEST_CASE("a_history_reply_whose_arrays_differ_in_length_is_malformed", tags) {
    auto lg(make_logger(test_suite));

    auto reply = a_reply();
    reply.gaps_us.pop_back();

    const auto ticks = marketdata_commands::history_ticks(reply);

    REQUIRE_FALSE(ticks.has_value());
    CHECK(ticks.error().find("differ in length") != std::string::npos);
}

TEST_CASE("a_history_reply_that_names_a_missing_coordinate_is_malformed", tags) {
    auto lg(make_logger(test_suite));

    auto reply = a_reply();
    reply.coordinate_indexes[1] = 2;

    const auto ticks = marketdata_commands::history_ticks(reply);

    REQUIRE_FALSE(ticks.has_value());
    CHECK(ticks.error().find("names coordinate 2") != std::string::npos);
}

TEST_CASE("a_history_reply_with_ticks_and_no_first_instant_is_malformed", tags) {
    auto lg(make_logger(test_suite));

    auto reply = a_reply();
    reply.first_instant.reset();

    const auto ticks = marketdata_commands::history_ticks(reply);

    REQUIRE_FALSE(ticks.has_value());
    CHECK(ticks.error().find("no first instant") != std::string::npos);
}

TEST_CASE("a_date_is_midnight_utc_and_an_instant_keeps_its_microseconds", tags) {
    auto lg(make_logger(test_suite));

    const auto midnight = std::chrono::sys_days{std::chrono::year{2026} / 10 / 23};
    const auto date = marketdata_commands::parse_span_bound("2026-10-23");
    const auto instant = marketdata_commands::parse_span_bound("2026-10-23 14:30:00.250000Z");

    REQUIRE(date.has_value());
    CHECK(*date == midnight);
    REQUIRE(instant.has_value());
    CHECK(*instant == midnight + 14h + 30min + 250ms);
}

TEST_CASE("a_bound_that_is_neither_a_date_nor_an_instant_is_refused_with_its_text", tags) {
    auto lg(make_logger(test_suite));

    const auto bound = marketdata_commands::parse_span_bound("yesterday");

    REQUIRE_FALSE(bound.has_value());
    CHECK(bound.error().find("'yesterday'") != std::string::npos);
}

TEST_CASE("a_history_tick_renders_its_time_coordinate_and_value", tags) {
    auto lg(make_logger(test_suite));

    const auto ticks = marketdata_commands::history_ticks(a_reply());
    REQUIRE(ticks.has_value());

    CHECK(marketdata_commands::history_tick_line((*ticks)[0]) ==
          "2026-10-23 14:30:00.000000Z  " + spot_uri + "  1.0845");
}

TEST_CASE("a_history_tick_row_escapes_a_coordinate_with_a_comma", tags) {
    auto lg(make_logger(test_suite));

    const history_tick tick{
        std::chrono::sys_days{std::chrono::year{2026} / 10 / 23}, "oresmd://x/a,b", 2.5};

    CHECK(marketdata_commands::history_tick_row(tick) ==
          "2026-10-23 00:00:00.000000Z,\"oresmd://x/a,b\",2.5");
}

TEST_CASE("the_ticks_command_states_its_usage_when_it_has_no_identity", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    marketdata_commands::process_ticks(out, session, {});

    CHECK(out.str().find("Usage: marketdata ticks --asset") != std::string::npos);
}

TEST_CASE("the_ticks_command_refuses_a_session_that_is_not_logged_in", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    marketdata_commands::process_ticks(out,
                                       session,
                                       {"--asset",
                                        "fx",
                                        "--scope",
                                        "AUD",
                                        "--instrument",
                                        "fx_spot",
                                        "--quote",
                                        "rate",
                                        "--fields",
                                        "ccy=USD"});

    CHECK(out.str().find("Not logged in.") != std::string::npos);
}

TEST_CASE("identity_fields_are_read_as_name_value_pairs_in_the_order_given", tags) {
    const auto fields =
        marketdata_commands::parse_identity_fields("curve_id=EUR-6M,day_counter=A365");

    REQUIRE(fields);
    REQUIRE(fields->size() == 2);
    CHECK(fields->front().name == "curve_id");
    CHECK(fields->front().text == "EUR-6M");
    CHECK(fields->back().name == "day_counter");
    CHECK(fields->back().text == "A365");
}

TEST_CASE("no_identity_fields_is_an_empty_list_and_not_an_error", tags) {
    const auto fields = marketdata_commands::parse_identity_fields("");

    REQUIRE(fields);
    CHECK(fields->empty());
}

TEST_CASE("an_identity_field_with_no_name_or_no_value_is_refused_with_its_text", tags) {
    for (const auto* text : {"ccy", "=USD", "ccy="}) {
        const auto fields = marketdata_commands::parse_identity_fields(text);
        REQUIRE_FALSE(fields);
        CHECK(fields.error().find(text) != std::string::npos);
    }
}
