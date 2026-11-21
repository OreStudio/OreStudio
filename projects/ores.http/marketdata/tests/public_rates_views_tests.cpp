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
#include "ores.http/routes/marketdata/marketdata_routes.hpp"
#include "ores.http/routes/marketdata/public_rates_views.hpp"
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
using ores::http::routes::marketdata::marketdata_routes;
using ores::http::routes::marketdata::public_rates_views;
using ores::http::marketdata::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string publishers_pattern = "/api/marketdata/public-rates";
const std::string coverage_pattern = "/api/marketdata/public-rates/coverage";
const std::string file_pattern = "/api/marketdata/public-rates/fixings.csv";

const std::string file_refused =
    "The fixings file names a publisher, and a range and item if it wants them.";

http_request public_at(const std::vector<std::pair<std::string, std::string>>& query) {
    http_request req;
    for (const auto& [name, value] : query)
        req.query_params[name] = value;
    return req;
}

}

TEST_CASE("the_public_publishers_reach_the_service_and_take_no_query", "[marketdata_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the public publishers with no service behind it";

    CHECK(answer_at<marketdata_routes>(
              http_method::get, publishers_pattern, http_request{}, publishers_pattern)
              .status == http_status::bad_gateway);
    CHECK(answer_at<marketdata_routes>(
              http_method::get, coverage_pattern, http_request{}, coverage_pattern)
              .status == http_status::bad_gateway);

    const auto refused = answer_at<marketdata_routes>(http_method::get,
                                                      publishers_pattern,
                                                      public_at({{"code", "BLOOMBERG"}}),
                                                      publishers_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(refused.body.find("code") != std::string::npos);
}

TEST_CASE("the_fixings_file_names_a_publisher", "[marketdata_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the query the fixings file takes";

    const auto refused =
        answer_at<marketdata_routes>(http_method::get, file_pattern, public_at({}), file_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(refused.body.find(file_refused) != std::string::npos);

    const auto unknown =
        answer_at<marketdata_routes>(http_method::get,
                                     file_pattern,
                                     public_at({{"publisher", "ECB"}, {"when", "1"}}),
                                     file_pattern);
    CHECK(unknown.status == http_status::bad_request);
    CHECK(unknown.body.find("when") != std::string::npos);
}

TEST_CASE("the_public_publishers_are_kept_privately_for_five_minutes", "[marketdata_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the cache policy the public views carry";

    for (const bool publishers : {true, false}) {
        http_request req;
        const auto first = publishers ? public_rates_views::publishers_answer(req, "{}") :
                                        public_rates_views::coverage_answer(req, "{}");
        if (publishers)
            INFO("publishers");
        else
            INFO("coverage");
        CHECK(first.status == http_status::ok);
        REQUIRE(first.headers.contains("ETag"));
        CHECK(first.headers.at("Cache-Control") == "private, max-age=300");
        CHECK(first.headers.at("Vary") == "Authorization, Cookie");

        http_request revalidation;
        revalidation.headers["if-none-match"] = first.headers.at("ETag");
        const auto again = publishers ? public_rates_views::publishers_answer(revalidation, "{}") :
                                        public_rates_views::coverage_answer(revalidation, "{}");
        CHECK(again.status == http_status::not_modified);
    }
}
