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
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::workflow::workflow_routes;
using ores::http::workflow::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string mine_pattern = "/api/me/requests";
const std::string one_pattern = "/api/me/requests/{id}";
const std::string decision_pattern = "/api/requests/{id}/decision";
const std::string detail_pattern = "/api/requests/{id}";

const std::string a_request_id = "11111111-1111-1111-1111-111111111111";
const std::string a_role_id = "22222222-2222-2222-2222-222222222222";
const std::string one_request = "/api/me/requests/" + a_request_id;

const std::string page_refused = "A page names an offset and a limit of 1 to 100.";
const std::string ask_refused = "Asking for roles names at least one role, and why.";
const std::string withdraw_refused = "Withdrawing a request names the version read and why.";
const std::string id_refused = "A request is named by its identifier.";
const std::string decision_refused =
    "A decision is approve, refuse, hold or resume, against the version read.";

bool body_holds(const std::string& body, const std::string& part) {
    return body.find(part) != std::string::npos;
}

/// A request that asks for one role.
http_request asking_for_one_role() {
    http_request req;
    req.body = R"({"roleIds":[")" + a_role_id + R"("],"reason":"I need to book trades."})";
    return req;
}

}

TEST_CASE("the_requests_a_person_raised_are_one_page_or_a_refusal", "[workflow_requests]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the page of a person's own requests";

    const auto answered =
        answer_at<workflow_routes>(http_method::get, mine_pattern, http_request{}, mine_pattern);
    CHECK(answered.status == http_status::bad_gateway);

    const std::vector<std::pair<std::string, std::string>> pages = {
        {"offset", "-1"}, {"offset", "ten"}, {"limit", "0"}, {"limit", "101"}};
    for (const auto& page : pages) {
        http_request req;
        req.query_params[page.first] = page.second;
        const auto refused =
            answer_at<workflow_routes>(http_method::get, mine_pattern, req, mine_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(body_holds(refused.body, page_refused));
    }
}

TEST_CASE("the_requests_a_person_raised_take_a_page_and_nothing_else", "[workflow_requests]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a parameter the page does not name";

    http_request req;
    req.query_params["kind"] = "iam.role_grant";
    const auto refused =
        answer_at<workflow_routes>(http_method::get, mine_pattern, req, mine_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(body_holds(refused.body, "kind"));
}

TEST_CASE("an_ask_names_at_least_one_role_and_why", "[workflow_requests]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the body an ask takes";

    const std::vector<std::string> bodies = {R"({"roleIds":[],"reason":"because"})",
                                             R"({"roleIds":["not-a-role"],"reason":"because"})",
                                             R"({"reason":"because"})",
                                             R"({"roleIds":[")" + a_role_id + R"("]})",
                                             R"({"roleIds":[")" + a_role_id +
                                                 R"("],"reason":"because","more":1})"};
    for (const auto& body : bodies) {
        http_request req;
        req.body = body;
        const auto refused =
            answer_at<workflow_routes>(http_method::post, mine_pattern, req, mine_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(body_holds(refused.body, ask_refused));
    }
}

TEST_CASE("an_ask_reaches_the_service_when_it_is_the_shape_it_takes", "[workflow_requests]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing an ask with no service behind it";

    CHECK(answer_at<workflow_routes>(
              http_method::post, mine_pattern, asking_for_one_role(), mine_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("a_request_is_withdrawn_by_its_identifier", "[workflow_requests]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the identifier a withdrawal names";

    http_request req;
    req.body = R"({"version":0,"comment":"no longer needed"})";
    const auto refused = answer_at<workflow_routes>(
        http_method::delete_, "/api/me/requests/not-an-id", req, one_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(body_holds(refused.body, id_refused));
}

TEST_CASE("a_withdrawal_names_the_version_read", "[workflow_requests]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the body a withdrawal takes";

    const std::vector<std::string> bodies = {R"({"comment":"no longer needed"})",
                                             R"({"version":-1,"comment":"no longer needed"})",
                                             R"({"version":0})"};
    for (const auto& body : bodies) {
        http_request req;
        req.body = body;
        const auto refused =
            answer_at<workflow_routes>(http_method::delete_, one_request, req, one_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(body_holds(refused.body, withdraw_refused));
    }
}

TEST_CASE("a_withdrawal_reaches_the_service_when_it_is_the_shape_it_takes", "[workflow_requests]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a withdrawal with no service behind it";

    http_request req;
    req.body = R"({"version":3,"comment":"no longer needed"})";
    CHECK(answer_at<workflow_routes>(http_method::delete_, one_request, req, one_pattern).status ==
          http_status::bad_gateway);
}

TEST_CASE("a_withdrawal_refuses_a_parameter_it_does_not_take", "[workflow_requests]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a parameter a withdrawal does not name";

    http_request req;
    req.query_params["force"] = "true";
    req.body = R"({"version":0,"comment":""})";
    const auto refused =
        answer_at<workflow_routes>(http_method::delete_, one_request, req, one_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(body_holds(refused.body, "force"));
}

TEST_CASE("a_decision_is_one_of_four_verbs_against_the_version_read", "[workflow_requests]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the body a decision takes";

    const std::vector<std::string> bodies = {
        R"({"version":0,"comment":"","decisionCode":"maybe"})",
        R"({"version":-1,"comment":"","decisionCode":"approve"})",
        R"({"comment":"","decisionCode":"approve"})",
        R"({"version":0,"comment":"","decisionCode":"approve","partCode":"Not-A-Part"})",
        R"({"version":0,"comment":"","decisionCode":"hold","holdReasonCode":"Bad Code"})",
        R"({"version":0,"comment":"","decisionCode":"approve","extra":1})"};
    for (const auto& body : bodies) {
        http_request req;
        req.body = body;
        const auto refused =
            answer_at<workflow_routes>(http_method::post,
                                       "/api/requests/" + a_request_id + "/decision",
                                       req,
                                       decision_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(body_holds(refused.body, decision_refused));
    }
}

TEST_CASE("a_decision_takes_the_part_and_the_reason_a_hold_names", "[workflow_requests]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a decision with no service behind it";

    http_request req;
    req.body =
        R"({"version":2,"comment":"parked","decisionCode":"hold","partCode":"book","holdReasonCode":"awaiting.risk"})";
    CHECK(
        answer_at<workflow_routes>(
            http_method::post, "/api/requests/" + a_request_id + "/decision", req, decision_pattern)
            .status == http_status::bad_gateway);
}

TEST_CASE("a_decision_is_named_by_the_requests_identifier", "[workflow_requests]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the identifier a decision names";

    http_request req;
    req.body = R"({"version":0,"comment":"","decisionCode":"approve"})";
    const auto refused = answer_at<workflow_routes>(
        http_method::post, "/api/requests/not-an-id/decision", req, decision_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(body_holds(refused.body, id_refused));
}

TEST_CASE("one_request_is_named_by_its_identifier", "[workflow_requests]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the identifier one request names";

    const auto refused = answer_at<workflow_routes>(
        http_method::get, "/api/requests/not-an-id", http_request{}, detail_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(body_holds(refused.body, id_refused));
}

TEST_CASE("one_request_answers_the_service_is_absent", "[workflow_requests]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing one request with no service behind it";

    CHECK(answer_at<workflow_routes>(
              http_method::get, "/api/requests/" + a_request_id, http_request{}, detail_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("one_request_refuses_a_parameter_it_does_not_take", "[workflow_requests]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a parameter one request does not name";

    http_request req;
    req.query_params["asOf"] = "2026-01-01";
    const auto refused = answer_at<workflow_routes>(
        http_method::get, "/api/requests/" + a_request_id, req, detail_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(body_holds(refused.body, "asOf"));
}
