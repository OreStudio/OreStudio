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
#include "ores.http/routes/compute/compute_routes.hpp"
#include "ores.http/routes/compute/grid_composed_routes.hpp"
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
using ores::http::routes::compute::compute_routes;
using ores::http::routes::compute::grid_composed_routes;
using ores::http::compute::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string summary_pattern = "/api/operations/grid";
const std::string series_pattern = "/api/operations/grid/series";
const std::string usage_pattern = "/api/operations/grid/usage";

const std::string series_refused = "The grid series read takes a range of 1h, 24h or 7d.";

bool grid_holds(const std::string& body, const std::string& part) {
    return body.find(part) != std::string::npos;
}

http_request at(const std::vector<std::pair<std::string, std::string>>& query) {
    http_request req;
    for (const auto& [name, value] : query)
        req.query_params[name] = value;
    return req;
}

}

TEST_CASE("the_grid_summary_takes_no_query_and_reaches_the_service", "[compute_grid_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the grid summary with no service behind it";

    const auto reached = answer_at<compute_routes>(
        http_method::get, summary_pattern, http_request{}, summary_pattern);
    // The deployment read is asked first, and it cannot be made either.
    CHECK((reached.status == http_status::bad_gateway ||
           reached.status == http_status::unauthorized));

    const auto refused = answer_at<compute_routes>(
        http_method::get, summary_pattern, at({{"scope", "all"}}), summary_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(grid_holds(refused.body, "scope"));
}

TEST_CASE("the_grid_series_names_a_range_or_it_is_refused", "[compute_grid_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the range a series read states";

    const std::vector<std::vector<std::pair<std::string, std::string>>> queries = {
        {}, {{"range", "6h"}}, {{"range", ""}}, {{"range", "1y"}}};
    for (const auto& query : queries) {
        const auto refused =
            answer_at<compute_routes>(http_method::get, series_pattern, at(query), series_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(grid_holds(refused.body, series_refused));
    }

    const auto unknown = answer_at<compute_routes>(
        http_method::get, series_pattern, at({{"range", "7d"}, {"bucket", "60"}}), series_pattern);
    CHECK(unknown.status == http_status::bad_request);
    CHECK(grid_holds(unknown.body, "bucket"));
}

TEST_CASE("a_grid_series_read_reaches_the_service", "[compute_grid_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing each series range with no service behind it";

    for (const std::string range : {"1h", "24h", "7d"}) {
        const auto reached = answer_at<compute_routes>(
            http_method::get, series_pattern, at({{"range", range}}), series_pattern);
        CHECK((reached.status == http_status::bad_gateway ||
               reached.status == http_status::unauthorized));
    }
}

TEST_CASE("the_grid_usage_takes_no_query_and_reaches_the_service", "[compute_grid_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the usage read with no service behind it";

    const auto reached =
        answer_at<compute_routes>(http_method::get, usage_pattern, http_request{}, usage_pattern);
    CHECK((reached.status == http_status::bad_gateway ||
           reached.status == http_status::unauthorized));

    const auto refused = answer_at<compute_routes>(
        http_method::get, usage_pattern, at({{"by", "party"}}), usage_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(grid_holds(refused.body, "by"));
}

TEST_CASE("the_three_grid_views_state_their_cache_policy", "[compute_grid_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the ETag and the freshness of each view";

    const std::vector<std::pair<std::string, std::pair<http_status, std::string>>> views = {
        {"summary", {http_status::ok, "private, max-age=10"}},
        {"series", {http_status::ok, "private, max-age=10"}},
        {"usage", {http_status::ok, "private, max-age=30"}}};
    for (const auto& [name, expected] : views) {
        http_request req;
        const auto first = name == "summary" ? grid_composed_routes::summary_answer(req, "{}") :
                           name == "series"  ? grid_composed_routes::series_answer(req, "{}") :
                                               grid_composed_routes::usage_answer(req, "{}");
        INFO(name);
        CHECK(first.status == expected.first);
        REQUIRE(first.headers.contains("ETag"));
        CHECK(first.headers.at("Cache-Control") == expected.second);
        CHECK(first.headers.at("Vary") == "Authorization, Cookie");

        http_request revalidation;
        revalidation.headers["if-none-match"] = first.headers.at("ETag");
        const auto again =
            name == "summary" ? grid_composed_routes::summary_answer(revalidation, "{}") :
            name == "series"  ? grid_composed_routes::series_answer(revalidation, "{}") :
                                grid_composed_routes::usage_answer(revalidation, "{}");
        CHECK(again.status == http_status::not_modified);
    }
}
