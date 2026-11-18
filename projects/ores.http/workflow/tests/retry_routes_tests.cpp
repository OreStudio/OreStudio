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
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::workflow::workflow_routes;
using ores::http::workflow::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string retry_pattern = "/api/workflow/{instanceId}/retry";

const std::string a_run_id = "44444444-4444-4444-4444-444444444444";
const std::string a_run = "/api/workflow/" + a_run_id + "/retry";

const std::string run_refused = "A run is named by its identifier.";
const std::string retry_refused = "A retry names the step to resume from, or no step at all.";

bool retry_holds(const std::string& body, const std::string& part) {
    return body.find(part) != std::string::npos;
}

}

TEST_CASE("a_retry_is_named_by_the_run_it_resumes", "[workflow_retry]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the identifier a retry names";

    const auto refused = answer_at<workflow_routes>(
        http_method::post, "/api/workflow/not-a-run/retry", http_request{}, retry_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(retry_holds(refused.body, run_refused));
}

TEST_CASE("a_retry_with_no_service_behind_it_reaches_the_service", "[workflow_retry]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a retry with no body and no service behind it";

    CHECK(answer_at<workflow_routes>(http_method::post, a_run, http_request{}, retry_pattern)
              .status == http_status::bad_gateway);

    http_request named;
    named.body = R"({"stepName":"load the trades"})";
    CHECK(answer_at<workflow_routes>(http_method::post, a_run, named, retry_pattern).status ==
          http_status::bad_gateway);

    http_request empty_object;
    empty_object.body = R"({})";
    CHECK(
        answer_at<workflow_routes>(http_method::post, a_run, empty_object, retry_pattern).status ==
        http_status::bad_gateway);
}

TEST_CASE("a_retry_names_a_step_and_nothing_else", "[workflow_retry]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the body a retry takes";

    const std::vector<std::string> bodies = {R"({"step":"load"})",
                                             R"({"stepName":"load","force":true})"};
    for (const auto& body : bodies) {
        http_request req;
        req.body = body;
        const auto refused =
            answer_at<workflow_routes>(http_method::post, a_run, req, retry_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(retry_holds(refused.body, retry_refused));
    }
}

TEST_CASE("a_retry_refuses_a_parameter_it_does_not_take", "[workflow_retry]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a parameter a retry does not name";

    http_request req;
    req.query_params["from"] = "the beginning";
    const auto refused = answer_at<workflow_routes>(http_method::post, a_run, req, retry_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(retry_holds(refused.body, "from"));
}
