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
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/marketdata/quote_board.hpp"
#include "ores.http/routes/marketdata/quote_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::routes::marketdata::quote_board;
using ores::http::routes::marketdata::quote_routes;
using ores::marketdata::messaging::market_tick;
using ores::marketdata::messaging::watchable_feed;
using namespace ores::logging;
using namespace std::chrono_literals;

namespace {

const std::string quotes_gbpusd =
    "oresmd://fx/GBP?type=quote&instrument=fx_spot&quote=rate&ccy=USD";
const std::string quotes_sofr_5y =
    "oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M&term=5Y";
const std::string quotes_bloomberg_feed = "0a42775e-3da7-4c61-a732-6b62cb9f948f";
const std::string quotes_lseg_feed = "c0f53bf1-a03a-4557-9906-7213091fcd82";

/// A transport that hands the board's subject its ticks by hand.
struct quotes_transport final {
    std::optional<quote_board::tick_handler> handler;
    std::string subject;

    quote_board::subscriber subscriber() {
        return [this](const std::string& s, quote_board::tick_handler on_tick) {
            subject = s;
            handler = std::move(on_tick);
            return std::shared_ptr<void>();
        };
    }

    void send(const std::string& provider,
              const std::string& feed,
              const std::string& uri,
              const std::string& value) {
        market_tick tick;
        tick.oresmd_uri = uri;
        tick.value = value;
        tick.source = provider + "-a";
        tick.observation_time = std::chrono::sys_days{std::chrono::year{2026} / 11 / 19} + 12h;
        (*handler)("ores.dev.test.marketdata.v1.ops.market_tick." + provider + "." + feed +
                       ".fx_spot.x._",
                   tick);
    }
};

http_response quotes_answer(const std::string& path,
                            http_request req,
                            quotes_transport& transport,
                            std::vector<watchable_feed> entitled,
                            ores::marketdata::messaging::get_series_ticks_response history = {}) {
    auto table = std::make_shared<ores::http::net::router>();
    auto board = std::make_shared<quote_board>(transport.subscriber());
    board->start();
    transport.send("bloomberg", quotes_bloomberg_feed, quotes_gbpusd, "1.30810");
    transport.send("lseg", quotes_lseg_feed, quotes_gbpusd, "1.30825");
    transport.send("bloomberg", quotes_bloomberg_feed, quotes_sofr_5y, "4.1533");
    transport.send("bloomberg", quotes_bloomberg_feed, quotes_gbpusd, "1.30822");
    quote_routes::register_routes(
        table,
        std::make_shared<ores::http::openapi::endpoint_registry>(),
        board,
        [entitled](const http_request&) { return entitled; },
        [history](const http_request&,
                  const ores::marketdata::messaging::get_provider_ticks_request&) {
            return history;
        });
    const auto route = table->match(http_method::get, path, req.path_params);
    REQUIRE(route.has_value());
    REQUIRE(route->requires_auth);
    boost::asio::io_context io;
    std::optional<http_response> answer;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> { answer = co_await route->handler(req); },
        boost::asio::detached);
    io.run();
    REQUIRE(answer.has_value());
    return *answer;
}

http_request quotes_query(std::map<std::string, std::string> params) {
    http_request req;
    for (auto& [name, value] : params)
        req.query_params[name] = value;
    return req;
}

std::size_t quotes_count(const std::string& body, const std::string& text) {
    std::size_t n = 0;
    for (auto at = body.find(text); at != std::string::npos; at = body.find(text, at + 1))
        ++n;
    return n;
}

}

TEST_CASE("the_board_listens_once_on_every_feed_subject", "[quotes]") {
    auto lg(make_logger(test_suite));
    quotes_transport transport;
    quote_board board(transport.subscriber());

    board.start();
    board.start();

    CHECK(transport.subject == "marketdata.v1.ops.market_tick.>");
}

TEST_CASE("a_quote_replaces_the_previous_one_of_its_datum_and_feed_and_keeps_its_value",
          "[quotes]") {
    auto lg(make_logger(test_suite));
    quotes_transport transport;
    quote_board board(transport.subscriber());
    board.start();

    transport.send("bloomberg", quotes_bloomberg_feed, quotes_gbpusd, "1.30810");
    transport.send("bloomberg", quotes_bloomberg_feed, quotes_gbpusd, "1.30822");

    const auto rows = board.after(0, {{"bloomberg", quotes_bloomberg_feed}});
    REQUIRE(rows.size() == 1);
    CHECK(rows[0].value == "1.30822");
    CHECK(rows[0].previous == "1.30810");
    CHECK(rows[0].sequence == 2);
    CHECK(rows[0].source == "bloomberg-a");
}

TEST_CASE("a_quote_is_labelled_from_its_uri", "[quotes]") {
    auto lg(make_logger(test_suite));
    quotes_transport transport;
    quote_board board(transport.subscriber());
    board.start();

    transport.send("bloomberg", quotes_bloomberg_feed, quotes_sofr_5y, "4.1533");

    const auto rows = board.after(0, {{"bloomberg", quotes_bloomberg_feed}});
    REQUIRE(rows.size() == 1);
    CHECK(rows[0].instrument_type == "ir_swap");
    CHECK(rows[0].asset_class == "interest_rates");
    CHECK(rows[0].series == "USD 0D 3M");
    CHECK(rows[0].point == "5Y");
    CHECK(rows[0].series_uri ==
          "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M");
}

TEST_CASE("a_tick_on_a_subject_that_names_no_feed_or_with_a_bad_uri_is_ignored", "[quotes]") {
    auto lg(make_logger(test_suite));
    quotes_transport transport;
    quote_board board(transport.subscriber());
    board.start();
    market_tick tick;
    tick.oresmd_uri = quotes_gbpusd;
    tick.value = "1.3";

    (*transport.handler)("ores.dev.test.synthetic.v1.ops.tick.x", tick);
    transport.send("bloomberg", quotes_bloomberg_feed, "oresmd://fx/GBP?type=quote", "1.3");

    CHECK(board.sequence() == 0);
    CHECK(board.after(0, {{"bloomberg", quotes_bloomberg_feed}}).empty());
}

TEST_CASE("a_read_returns_only_the_feeds_the_caller_may_watch", "[quotes]") {
    auto lg(make_logger(test_suite));
    quotes_transport transport;

    const auto answer = quotes_answer("/api/marketdata/quotes",
                                      quotes_query({}),
                                      transport,
                                      {{"bloomberg", quotes_bloomberg_feed}});

    CHECK(answer.status == http_status::ok);
    CHECK(quotes_count(answer.body, "\"provider\":\"bloomberg\"") == 2);
    CHECK(quotes_count(answer.body, "\"provider\":\"lseg\"") == 0);
    CHECK(answer.body.find("\"sequence\":4") != std::string::npos);
}

TEST_CASE("a_caller_that_may_watch_both_feeds_sees_a_datum_from_each", "[quotes]") {
    auto lg(make_logger(test_suite));
    quotes_transport transport;

    const auto answer =
        quotes_answer("/api/marketdata/quotes",
                      quotes_query({}),
                      transport,
                      {{"bloomberg", quotes_bloomberg_feed}, {"lseg", quotes_lseg_feed}});

    CHECK(quotes_count(answer.body, quotes_gbpusd) == 2);
}

TEST_CASE("a_read_after_a_sequence_returns_only_the_newer_quotes", "[quotes]") {
    auto lg(make_logger(test_suite));
    quotes_transport transport;

    const auto answer = quotes_answer("/api/marketdata/quotes",
                                      quotes_query({{"after", "3"}}),
                                      transport,
                                      {{"bloomberg", quotes_bloomberg_feed}});

    CHECK(quotes_count(answer.body, "\"provider\":") == 1);
    CHECK(answer.body.find("\"value\":\"1.30822\"") != std::string::npos);
}

TEST_CASE("a_read_with_an_after_that_is_not_a_sequence_is_refused", "[quotes]") {
    auto lg(make_logger(test_suite));
    quotes_transport transport;

    const auto answer = quotes_answer("/api/marketdata/quotes",
                                      quotes_query({{"after", "soon"}}),
                                      transport,
                                      {{"bloomberg", quotes_bloomberg_feed}});

    CHECK(answer.status == http_status::bad_request);
}

TEST_CASE("the_feeds_read_names_the_source_of_a_feed_the_board_has_heard", "[quotes]") {
    auto lg(make_logger(test_suite));
    quotes_transport transport;

    const auto answer = quotes_answer(
        "/api/marketdata/quotes/feeds",
        quotes_query({}),
        transport,
        {{"bloomberg", quotes_bloomberg_feed}, {"ice", "082e749a-8c4d-4cc3-b2fb-8e6d3ef61aaf"}});

    CHECK(answer.body.find("\"source\":\"bloomberg-a\"") != std::string::npos);
    CHECK(answer.body.find("\"provider\":\"ice\"") != std::string::npos);
    CHECK(answer.body.find("\"last_at\":null") != std::string::npos);
}

TEST_CASE("the_feeds_read_states_the_rank_of_each_provider", "[quotes]") {
    auto lg(make_logger(test_suite));
    quotes_transport transport;

    const auto answer = quotes_answer("/api/marketdata/quotes/feeds",
                                      quotes_query({}),
                                      transport,
                                      {{"bloomberg", quotes_bloomberg_feed, 1},
                                       {"ice", "082e749a-8c4d-4cc3-b2fb-8e6d3ef61aaf", 3}});

    CHECK(answer.body.find("\"provider\":\"bloomberg\",\"feed\":\"" + quotes_bloomberg_feed +
                           "\",\"precedence\":1,") != std::string::npos);
    CHECK(answer.body.find("\"precedence\":3,") != std::string::npos);
}

TEST_CASE("a_history_read_that_names_no_series_is_refused", "[quotes]") {
    auto lg(make_logger(test_suite));
    quotes_transport transport;

    const auto answer = quotes_answer(
        "/api/marketdata/quotes/history",
        quotes_query({{"provider", "bloomberg"}, {"from", "2026-11-19T11:00:00.000Z"}}),
        transport,
        {});

    CHECK(answer.status == http_status::bad_request);
}

TEST_CASE("a_history_reply_whose_arrays_differ_in_length_is_answered_as_a_bad_gateway",
          "[quotes]") {
    auto lg(make_logger(test_suite));
    quotes_transport transport;
    ores::marketdata::messaging::get_series_ticks_response broken;
    broken.first_instant = std::chrono::system_clock::now();
    broken.values = {1.30810, 1.30822};
    broken.gaps_us = {0};
    broken.coordinate_indexes = {0, 0};

    const auto answer = quotes_answer("/api/marketdata/quotes/history",
                                      quotes_query({{"provider", "bloomberg"},
                                                    {"series", quotes_gbpusd},
                                                    {"from", "2026-11-19T11:00:00.000Z"}}),
                                      transport,
                                      {},
                                      broken);

    CHECK(answer.status == http_status::bad_gateway);
}

TEST_CASE("a_read_over_the_ceiling_states_the_sequence_of_the_last_quote_it_sent", "[quotes]") {
    auto lg(make_logger(test_suite));
    quotes_transport transport;
    auto board = std::make_shared<quote_board>(transport.subscriber());
    board->start();
    const auto total = quote_routes::most_quotes + 5;
    for (std::size_t i = 1; i <= total; ++i)
        transport.send("bloomberg",
                       quotes_bloomberg_feed,
                       "oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D"
                       "&tenor=3M&term=" +
                           std::to_string(i) + "D",
                       "4.0");
    auto table = std::make_shared<ores::http::net::router>();
    quote_routes::register_routes(
        table,
        std::make_shared<ores::http::openapi::endpoint_registry>(),
        board,
        [](const http_request&) {
            return std::vector<watchable_feed>{{"bloomberg", quotes_bloomberg_feed}};
        },
        [](const http_request&, const ores::marketdata::messaging::get_provider_ticks_request&) {
            return ores::marketdata::messaging::get_series_ticks_response{};
        });
    const auto read = [&](const std::string& after) {
        http_request req =
            quotes_query(after.empty() ? std::map<std::string, std::string>{} :
                                         std::map<std::string, std::string>{{"after", after}});
        const auto route =
            table->match(http_method::get, "/api/marketdata/quotes", req.path_params);
        REQUIRE(route.has_value());
        boost::asio::io_context io;
        std::optional<http_response> answer;
        boost::asio::co_spawn(
            io,
            [&]() -> boost::asio::awaitable<void> { answer = co_await route->handler(req); },
            boost::asio::detached);
        io.run();
        REQUIRE(answer.has_value());
        return answer->body;
    };

    const auto first = read("");
    const auto stated = "\"sequence\":" + std::to_string(quote_routes::most_quotes) + ",";
    CHECK(first.find(stated) != std::string::npos);
    CHECK(quotes_count(first, "\"provider\":") == quote_routes::most_quotes);

    const auto rest = read(std::to_string(quote_routes::most_quotes));
    CHECK(quotes_count(rest, "\"provider\":") == 5);
}
