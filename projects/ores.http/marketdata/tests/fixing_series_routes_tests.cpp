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
#include "ores.http/routes/marketdata/fixing_series_routes.hpp"
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
using ores::http::routes::marketdata::fixing_series_routes;
using ores::http::routes::marketdata::marketdata_routes;
using ores::http::marketdata::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string fixing_series_pattern = "/api/marketdata/fixing-series";

const std::string fixing_observations_pattern = "/api/marketdata/fixing-observations";

const std::string fixing_series_refused = "An item's fixings are named by its URI.";

http_request query_at(const std::vector<std::pair<std::string, std::string>>& query) {
    http_request req;
    for (const auto& [name, value] : query)
        req.query_params[name] = value;
    return req;
}

}

TEST_CASE("an_items_fixings_are_named_by_their_uri", "[marketdata_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the URI an item's fixings are named by";

    for (const auto& query :
         std::vector<std::vector<std::pair<std::string, std::string>>>{{}, {{"uri", ""}}}) {
        const auto refused = answer_at<marketdata_routes>(
            http_method::get, fixing_series_pattern, query_at(query), fixing_series_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(refused.body.find(fixing_series_refused) != std::string::npos);
    }

    const auto unknown =
        answer_at<marketdata_routes>(http_method::get,
                                     fixing_series_pattern,
                                     query_at({{"uri", "ir.irs.USD.5Y"}, {"asOf", "2026-01-01"}}),
                                     fixing_series_pattern);
    CHECK(unknown.status == http_status::bad_request);
    CHECK(unknown.body.find("asOf") != std::string::npos);
}

TEST_CASE("an_items_fixings_reach_the_service", "[marketdata_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the item's series with no service behind it";

    CHECK(answer_at<marketdata_routes>(http_method::get,
                                       fixing_series_pattern,
                                       query_at({{"uri", "ir.irs.USD.5Y"}}),
                                       fixing_series_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("an_items_fixings_are_kept_privately_for_five_minutes", "[marketdata_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the cache policy the item's series carries";

    http_request req;
    const auto first = fixing_series_routes::fixing_series_answer(req, "{}");
    CHECK(first.status == http_status::ok);
    REQUIRE(first.headers.contains("ETag"));
    CHECK(first.headers.at("Cache-Control") == "private, max-age=300");
    CHECK(first.headers.at("Vary") == "Authorization, Cookie");

    http_request revalidation;
    revalidation.headers["if-none-match"] = first.headers.at("ETag");
    CHECK(fixing_series_routes::fixing_series_answer(revalidation, "{}").status ==
          http_status::not_modified);
}

TEST_CASE("the_fixing_observations_refuse_a_filter_they_do_not_know", "[marketdata_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the filters the fixing observations are listed by";

    const auto unknown =
        answer_at<marketdata_routes>(http_method::get,
                                     fixing_observations_pattern,
                                     query_at({{"asset_class", "ir"}, {"asOf", "2026-01-01"}}),
                                     fixing_observations_pattern);
    CHECK(unknown.status == http_status::bad_request);
    CHECK(unknown.body.find("asOf") != std::string::npos);
}

TEST_CASE("the_fixing_observations_reach_the_service", "[marketdata_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the fixing observations with no service behind them";

    CHECK(answer_at<marketdata_routes>(http_method::get,
                                       fixing_observations_pattern,
                                       query_at({{"asset_class", "ir"}, {"currency", "USD"}}),
                                       fixing_observations_pattern)
              .status == http_status::bad_gateway);
}
