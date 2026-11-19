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
#include "ores.http/routes/workflow/inbox_view_routes.hpp"
#include "ores.http/routes/workflow/workflow_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::workflow::inbox_view_routes;
using ores::http::routes::workflow::workflow_routes;
using ores::http::workflow::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string inbox_pattern = "/api/requests";

const std::string inbox_page_refused = "A page names an offset and a limit of 1 to 100.";

/// An inbox of one open request and nothing answered.
const std::string an_inbox_body =
    R"({"items":[{"id":"66666666-6666-6666-6666-666666666666","version":1,)"
    R"("kindCode":"iam.role_grant","stateCode":"waiting","autoApprovalCode":"",)"
    R"("requestedBy":"alice","requestedAt":"2026-11-18 09:00:00.000000Z","reason":"needs it",)"
    R"("expiresAt":null,"roles":[],"decision":null,"parts":[],"subjects":[]}],)"
    R"("total":1,"answered":[]})";

bool inbox_holds(const std::string& body, const std::string& part) {
    return body.find(part) != std::string::npos;
}

}

TEST_CASE("the_inbox_is_one_page_or_a_refusal", "[workflow_inbox]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the page the inbox takes";

    CHECK(answer_at<workflow_routes>(http_method::get, inbox_pattern, http_request{}, inbox_pattern)
              .status == http_status::bad_gateway);

    const std::vector<std::pair<std::string, std::string>> pages = {
        {"offset", "-1"}, {"limit", "0"}, {"limit", "101"}, {"limit", "ten"}};
    for (const auto& page : pages) {
        http_request req;
        req.query_params[page.first] = page.second;
        const auto refused =
            answer_at<workflow_routes>(http_method::get, inbox_pattern, req, inbox_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(inbox_holds(refused.body, inbox_page_refused));
    }
}

TEST_CASE("the_inbox_refuses_a_parameter_it_does_not_take", "[workflow_inbox]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a parameter the inbox does not name";

    http_request req;
    req.query_params["kind"] = "iam.role_grant";
    const auto refused =
        answer_at<workflow_routes>(http_method::get, inbox_pattern, req, inbox_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(inbox_holds(refused.body, "kind"));
}

TEST_CASE("the_inbox_is_kept_privately_for_fifteen_seconds", "[workflow_inbox]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the cache policy the inbox carries";

    http_request req;
    const auto first = inbox_view_routes::inbox_answer(req, an_inbox_body);
    CHECK(first.status == http_status::ok);
    REQUIRE(first.headers.contains("ETag"));
    CHECK(first.headers.at("Cache-Control") == "private, max-age=15");
    CHECK(first.headers.at("Vary") == "Authorization, Cookie");
    CHECK(inbox_holds(first.body, "iam.role_grant"));

    http_request revalidation;
    revalidation.headers["if-none-match"] = first.headers.at("ETag");
    CHECK(inbox_view_routes::inbox_answer(revalidation, an_inbox_body).status ==
          http_status::not_modified);
}
