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
#include "ores.http/routes/marketdata/shape_fields_routes.hpp"
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
using ores::http::routes::marketdata::shape_fields_routes;
using ores::http::marketdata::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string shape_fields_rules_pattern = "/api/marketdata/series-classification-rules";
const std::string shape_fields_pattern = "/api/marketdata/series-identity-shape-fields";

http_request shape_fields_at(const std::vector<std::pair<std::string, std::string>>& query) {
    http_request req;
    for (const auto& [name, value] : query)
        req.query_params[name] = value;
    return req;
}

}

TEST_CASE("the_identity_fields_ask_nothing_of_their_query", "[marketdata_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the query the two editor views refuse";

    const auto refused_rules = answer_at<marketdata_routes>(http_method::get,
                                                            shape_fields_rules_pattern,
                                                            shape_fields_at({{"asOf", "x"}}),
                                                            shape_fields_rules_pattern);
    CHECK(refused_rules.status == http_status::bad_request);
    CHECK(refused_rules.body.find("asOf") != std::string::npos);

    const auto refused_fields = answer_at<marketdata_routes>(http_method::get,
                                                             shape_fields_pattern,
                                                             shape_fields_at({{"limit", "1"}}),
                                                             shape_fields_pattern);
    CHECK(refused_fields.status == http_status::bad_request);
    CHECK(refused_fields.body.find("limit") != std::string::npos);
}

TEST_CASE("the_identity_fields_reach_the_service", "[marketdata_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the two editor views with no service behind them";

    CHECK(answer_at<marketdata_routes>(http_method::get,
                                       shape_fields_rules_pattern,
                                       shape_fields_at({}),
                                       shape_fields_rules_pattern)
              .status == http_status::bad_gateway);
    CHECK(answer_at<marketdata_routes>(
              http_method::get, shape_fields_pattern, shape_fields_at({}), shape_fields_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("the_identity_fields_are_asked_about_at_each_read", "[marketdata_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the cache policy the two editor views carry";

    for (const auto& answer :
         {&shape_fields_routes::rules_answer, &shape_fields_routes::fields_answer}) {
        http_request req;
        const auto first = answer(req, "{}");
        CHECK(first.status == http_status::ok);
        REQUIRE(first.headers.contains("ETag"));
        CHECK(first.headers.at("Cache-Control") == "private, no-cache");
        CHECK(first.headers.at("Vary") == "Authorization, Cookie");

        http_request revalidation;
        revalidation.headers["if-none-match"] = first.headers.at("ETag");
        CHECK(answer(revalidation, "{}").status == http_status::not_modified);
    }
}
