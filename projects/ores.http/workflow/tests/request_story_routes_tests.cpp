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
#include "ores.http/routes/workflow/request_story_routes.hpp"
#include "ores.http/routes/workflow/workflow_routes.hpp"
#include "ores.logging/logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::workflow::request_story_routes;
using ores::http::routes::workflow::workflow_routes;
using ores::http::workflow::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string story_pattern = "/api/requests/{id}/story";

const std::string a_story_request_id = "99999999-9999-9999-9999-999999999999";
const std::string its_story = "/api/requests/" + a_story_request_id + "/story";

const std::string story_refused = "A request is named by its identifier.";

/// A story of one raised request and the answer given to it.
const std::string a_story_body =
    R"({"requestId":"99999999-9999-9999-9999-999999999999","events":[)"
    R"({"entityType":"ores.inbox.approval_request",)"
    R"("entityId":"99999999-9999-9999-9999-999999999999","kind":"raised",)"
    R"("at":"2026-11-18 09:00:00.000000Z","actor":"alice","version":1,)"
    R"("reasonCode":"","commentary":"","fields":[{"name":"State","value":"waiting"}]},)"
    R"({"entityType":"ores.inbox.approval_decision",)"
    R"("entityId":"99999999-9999-9999-9999-999999999999","kind":"decided",)"
    R"("at":"2026-11-18 09:05:00.000000Z","actor":"bob","version":1,)"
    R"("reasonCode":"","commentary":"","fields":[]}]})";

bool story_holds(const std::string& body, const std::string& part) {
    return body.find(part) != std::string::npos;
}

}

TEST_CASE("a_story_is_named_by_its_request", "[workflow_story]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the identifier a story names";

    const auto refused = answer_at<workflow_routes>(
        http_method::get, "/api/requests/not-a-request/story", http_request{}, story_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(story_holds(refused.body, story_refused));
}

TEST_CASE("a_story_answers_the_service_is_absent", "[workflow_story]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a story with no service behind it";

    CHECK(answer_at<workflow_routes>(http_method::get, its_story, http_request{}, story_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("a_story_refuses_a_parameter_it_does_not_take", "[workflow_story]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a parameter a story does not name";

    http_request req;
    req.query_params["since"] = "2026-01-01";
    const auto refused =
        answer_at<workflow_routes>(http_method::get, its_story, req, story_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(story_holds(refused.body, "since"));
}

TEST_CASE("a_story_is_kept_privately_for_fifteen_seconds", "[workflow_story]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the cache policy a story carries";

    http_request req;
    const auto first = request_story_routes::story_answer(req, a_story_body);
    CHECK(first.status == http_status::ok);
    REQUIRE(first.headers.contains("ETag"));
    CHECK(first.headers.at("Cache-Control") == "private, max-age=15");
    CHECK(first.headers.at("Vary") == "Authorization, Cookie");
    CHECK(story_holds(first.body, "approval_decision"));

    http_request revalidation;
    revalidation.headers["if-none-match"] = first.headers.at("ETag");
    CHECK(request_story_routes::story_answer(revalidation, a_story_body).status ==
          http_status::not_modified);

    http_request changed;
    changed.headers["if-none-match"] = first.headers.at("ETag");
    CHECK(request_story_routes::story_answer(changed, a_story_body.substr(0, 40)).status ==
          http_status::ok);
}
