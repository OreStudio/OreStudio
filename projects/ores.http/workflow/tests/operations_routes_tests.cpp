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
#include "ores.http/routes/workflow/workflow_routes.hpp"
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
using ores::http::routes::workflow::workflow_routes;
using ores::http::workflow::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string services_pattern = "/api/operations/services";
const std::string logs_pattern = "/api/operations/logs";

const std::string logs_refused =
    "The logs read takes a range, the filters the journey names, and a page.";

bool operations_holds(const std::string& body, const std::string& part) {
    return body.find(part) != std::string::npos;
}

/// A read of the logs at one query, with the range every read states.
http_request logs_at(const std::vector<std::pair<std::string, std::string>>& query) {
    http_request req;
    for (const auto& [name, value] : query)
        req.query_params[name] = value;
    return req;
}

}

TEST_CASE("the_roster_reaches_the_service_and_takes_no_query", "[workflow_operations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the services roster with no service behind it";

    CHECK(answer_at<workflow_routes>(
              http_method::get, services_pattern, http_request{}, services_pattern)
              .status == http_status::bad_gateway);

    http_request req;
    req.query_params["scope"] = "all";
    const auto refused =
        answer_at<workflow_routes>(http_method::get, services_pattern, req, services_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(operations_holds(refused.body, "scope"));
}

TEST_CASE("the_logs_name_a_range_or_they_are_refused", "[workflow_operations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the range a logs read states";

    const std::vector<std::vector<std::pair<std::string, std::string>>> queries = {
        {}, {{"range", "1y"}}, {{"range", ""}}};
    for (const auto& query : queries) {
        const auto refused = answer_at<workflow_routes>(
            http_method::get, logs_pattern, logs_at(query), logs_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(operations_holds(refused.body, logs_refused));
    }
}

TEST_CASE("the_logs_take_the_filters_the_journey_names", "[workflow_operations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the filters a logs read does not name";

    const std::vector<std::vector<std::pair<std::string, std::string>>> queries = {
        {{"range", "1h"}, {"level", "fatal"}},
        {{"range", "1h"}, {"source", "client"}},
        {{"range", "1h"}, {"offset", "-1"}},
        {{"range", "1h"}, {"offset", "ten"}},
        {{"range", "1h"}, {"limit", "0"}},
        {{"range", "1h"}, {"limit", "501"}}};
    for (const auto& query : queries) {
        const auto refused = answer_at<workflow_routes>(
            http_method::get, logs_pattern, logs_at(query), logs_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(operations_holds(refused.body, logs_refused));
    }
}

TEST_CASE("a_page_of_logs_reaches_the_service", "[workflow_operations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a logs read with no service behind it";

    const std::vector<std::vector<std::pair<std::string, std::string>>> queries = {
        {{"range", "15m"}},
        {{"range", "24h"}, {"limit", "500"}},
        {{"range", "6h"},
         {"level", "error"},
         {"source", "server"},
         {"component", "ores.comms"},
         {"tag", "feed"},
         {"message", "timed out"}},
        {{"range", "1h"}, {"offset", "100"}, {"limit", "50"}}};
    for (const auto& query : queries)
        CHECK(
            answer_at<workflow_routes>(http_method::get, logs_pattern, logs_at(query), logs_pattern)
                .status == http_status::bad_gateway);
}
