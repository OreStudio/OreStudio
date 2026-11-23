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
#include "ores.http/routes/marketdata/market_reads_routes.hpp"
#include "ores.http/routes/marketdata/marketdata_routes.hpp"
#include "ores.logging/logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <utility>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::marketdata::market_reads_routes;
using ores::http::routes::marketdata::marketdata_routes;
using ores::http::marketdata::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string market_reads_instruments = "/api/market/instruments";
const std::string market_reads_ladder = "/api/market/rate-ladder";
const std::string market_reads_montage = "/api/market/fx-montage";
const std::string market_reads_analytics = "/api/market/curve-analytics";

const std::string market_reads_shape = "A market read states the body its screen sends.";

http_request market_reads_at(const std::string& body) {
    http_request req;
    req.body = body;
    return req;
}

}

TEST_CASE("the_market_reads_state_the_body_their_screens_send", "[marketdata_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the bodies the four desk reads refuse";

    for (const auto& [pattern, body] : std::vector<std::pair<std::string, std::string>>{
             {market_reads_instruments, R"json({"unknown":1})json"},
             {market_reads_instruments, R"json({"query":5})json"},
             {market_reads_ladder, R"json({"currency":"EUR","extra":"x"})json"},
             {market_reads_ladder, R"json({"currency":"EURO"})json"},
             {market_reads_montage, R"json({"base_currency":"EUR","as_of":"yesterday"})json"},
             {market_reads_analytics, R"json({"instrument_id":"not-an-id"})json"}}) {
        const auto refused = answer_at<marketdata_routes>(
            http_method::post, pattern, market_reads_at(body), pattern);
        CHECK(refused.status == http_status::bad_request);
    }

    const auto refused = answer_at<marketdata_routes>(http_method::post,
                                                      market_reads_instruments,
                                                      market_reads_at(R"json({"unknown":1})json"),
                                                      market_reads_instruments);
    CHECK(refused.body.find(market_reads_shape) != std::string::npos);
}

TEST_CASE("the_market_reads_reach_the_service", "[marketdata_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the four desk reads with no service behind them";

    for (
        const auto& [pattern, body] : std::vector<std::pair<std::string, std::string>>{
            {market_reads_instruments, R"json({})json"},
            {market_reads_ladder, R"json({"currency":"EUR","as_of":"2026-11-20 12:00:00Z"})json"},
            {market_reads_montage,
             R"json({"base_currency":"EUR","as_of":"2026-11-20 12:00:00Z"})json"},
            {market_reads_analytics,
             R"json({"instrument_id":"77777777-7777-4777-8777-777777777777","as_of":"2026-11-20 12:00:00Z"})json"}}) {
        CHECK(
            answer_at<marketdata_routes>(http_method::post, pattern, market_reads_at(body), pattern)
                .status == http_status::bad_gateway);
    }
}
