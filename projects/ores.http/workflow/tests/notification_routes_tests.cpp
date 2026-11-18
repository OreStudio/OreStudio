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

const std::string notices_pattern = "/api/me/notifications";
const std::string unread_pattern = "/api/me/notifications/unread-count";
const std::string read_pattern = "/api/me/notifications/read";
const std::string clear_pattern = "/api/me/notifications/clear";

const std::string a_notice_id = "33333333-3333-3333-3333-333333333333";

const std::string notice_page_refused = "A page names an offset and a limit of 1 to 100.";
const std::string read_refused =
    "Marking notifications read names the ones to mark, or an empty list for all unread.";
const std::string clear_refused =
    "Clearing notifications names the ones to clear, or an empty list for all read.";

bool notice_holds(const std::string& body, const std::string& part) {
    return body.find(part) != std::string::npos;
}

}

TEST_CASE("the_notices_a_person_holds_are_one_page_or_a_refusal", "[workflow_notices]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the page of notices";

    CHECK(answer_at<workflow_routes>(
              http_method::get, notices_pattern, http_request{}, notices_pattern)
              .status == http_status::bad_gateway);

    const std::vector<std::pair<std::string, std::string>> pages = {
        {"offset", "-1"}, {"limit", "0"}, {"limit", "101"}, {"unreadOnly", "sometimes"}};
    for (const auto& page : pages) {
        http_request req;
        req.query_params[page.first] = page.second;
        const auto refused =
            answer_at<workflow_routes>(http_method::get, notices_pattern, req, notices_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(notice_holds(refused.body, notice_page_refused));
    }
}

TEST_CASE("the_notices_a_person_holds_take_a_page_and_that_is_all", "[workflow_notices]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the parameter a page of notices does not name";

    http_request req;
    req.query_params["kind"] = "inbox.role_granted";
    const auto refused =
        answer_at<workflow_routes>(http_method::get, notices_pattern, req, notices_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(notice_holds(refused.body, "kind"));

    http_request unread;
    unread.query_params["unreadOnly"] = "true";
    CHECK(answer_at<workflow_routes>(http_method::get, notices_pattern, unread, notices_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("the_unread_count_reaches_the_service", "[workflow_notices]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the unread count with no service behind it";

    CHECK(
        answer_at<workflow_routes>(http_method::get, unread_pattern, http_request{}, unread_pattern)
            .status == http_status::bad_gateway);
}

TEST_CASE("a_mark_names_the_notices_or_states_none", "[workflow_notices]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the body a mark takes";

    const std::vector<std::string> bodies = {
        R"({})", R"({"ids":["not-a-notice"]})", R"({"ids":[],"more":1})"};
    for (const auto& body : bodies) {
        http_request req;
        req.body = body;
        const auto refused =
            answer_at<workflow_routes>(http_method::post, read_pattern, req, read_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(notice_holds(refused.body, read_refused));
    }

    http_request empty;
    empty.body = R"({"ids":[]})";
    CHECK(answer_at<workflow_routes>(http_method::post, read_pattern, empty, read_pattern).status ==
          http_status::bad_gateway);

    http_request one;
    one.body = R"({"ids":[")" + a_notice_id + R"("]})";
    CHECK(answer_at<workflow_routes>(http_method::post, read_pattern, one, read_pattern).status ==
          http_status::bad_gateway);
}

TEST_CASE("a_clear_names_the_notices_or_states_none", "[workflow_notices]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the body a clear takes";

    http_request missing;
    missing.body = R"({})";
    const auto refused =
        answer_at<workflow_routes>(http_method::post, clear_pattern, missing, clear_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(notice_holds(refused.body, clear_refused));

    http_request empty;
    empty.body = R"({"ids":[]})";
    CHECK(
        answer_at<workflow_routes>(http_method::post, clear_pattern, empty, clear_pattern).status ==
        http_status::bad_gateway);
}
