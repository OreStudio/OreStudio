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
#include "ores.http/routes/marketdata/series_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::marketdata::series_routes;
using ores::http::marketdata::tests::answer_at;
using namespace ores::logging;
namespace marketdata_domain = ores::marketdata::domain;

namespace {

const auto series_id = "00000000-0000-0000-0000-0000000000e1";
const std::string series_address = "/api/series/" + std::string(series_id) + "/observations";
const std::string series_pattern = "/api/series/{id}/observations";

bool series_holds(const std::string& text, const std::string& part) {
    return text.find(part) != std::string::npos;
}

marketdata_domain::market_observation
observation_of(unsigned day, const char* value, const char* source) {
    marketdata_domain::market_observation row;
    row.observation_datetime = std::chrono::system_clock::time_point{
        std::chrono::sys_days{std::chrono::year{2026} / std::chrono::March / day}};
    row.value = value;
    row.source = source;
    return row;
}

/// The status a read of the series' observations gives, with the window named.
http_status series_status(const std::string& window) {
    http_request req;
    req.query_params["window"] = window;
    return answer_at<series_routes>(http_method::get, series_address, req, series_pattern).status;
}

}

TEST_CASE("series_state_their_observations_oldest_first", "[series]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a series' observations";

    const auto answer = series_routes::observations_json(
        {observation_of(15, "1.2", "bloomberg"), observation_of(14, "1.1", "")}, 42);

    CHECK(series_holds(answer, R"({"at":"2026-03-14 00:00:00.000000Z")"));
    CHECK(series_holds(answer, R"("value":"1.1")"));
    CHECK(series_holds(answer, R"("source":"")"));
    CHECK(series_holds(answer, R"("total":42)"));
    CHECK(answer.find("2026-03-14") < answer.find("2026-03-15"));
    CHECK(series_routes::observations_json({}, 0) == R"({"observations":[],"total":0})");
}

TEST_CASE("series_refuse_a_read_that_names_no_series", "[series]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a read of observations";

    http_request req;
    CHECK(answer_at<series_routes>(
              http_method::get, "/api/series/not-an-id/observations", req, series_pattern)
              .status == http_status::bad_request);
    CHECK(series_status("decade") == http_status::bad_request);
    CHECK(series_status("all") == http_status::bad_gateway);
}

TEST_CASE("series_refuse_a_query_they_do_not_read", "[series]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a read's query";

    http_request req;
    req.query_params["colour"] = "red";
    CHECK(answer_at<series_routes>(http_method::get, series_address, req, series_pattern).status ==
          http_status::bad_request);

    req.query_params.erase("colour");
    req.query_params["limit"] = "0";
    CHECK(answer_at<series_routes>(http_method::get, series_address, req, series_pattern).status ==
          http_status::bad_request);

    req.query_params["limit"] = "many";
    CHECK(answer_at<series_routes>(http_method::get, series_address, req, series_pattern).status ==
          http_status::bad_request);

    req.query_params["limit"] = "5000";
    CHECK(answer_at<series_routes>(http_method::get, series_address, req, series_pattern).status ==
          http_status::bad_gateway);
}
