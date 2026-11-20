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
#include "ores.http/routes/workflow/request_proposal_routes.hpp"
#include "ores.http/routes/workflow/workflow_routes.hpp"
#include "ores.logging/logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <tuple>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::workflow::request_proposal_routes;
using ores::http::routes::workflow::workflow_routes;
using ores::http::workflow::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string changes_pattern = "/api/requests/{id}/changes";
const std::string checks_pattern = "/api/requests/{id}/checks";

const std::string a_proposal_request_id = "77777777-7777-7777-7777-777777777777";
const std::string a_proposal = "/api/requests/" + a_proposal_request_id;
const std::string its_changes = a_proposal + "/changes";
const std::string its_checks = a_proposal + "/checks";

const std::string proposal_refused = "A request is named by its identifier.";

/// A proposal of one book line against the live book.
const std::string a_proposal_body =
    R"({"requestId":"77777777-7777-7777-7777-777777777777",)"
    R"("kindCode":"refdata.book_change","changes":[{"lineNo":1,"operation":"put",)"
    R"("subjectId":"88888888-8888-8888-8888-888888888888","baseVersion":3,)"
    R"("applied":false,"fields":[{"name":"name","before":"OLD","after":"NEW",)"
    R"("changed":true}]}]})";

/// The verdicts of the same proposal, run through the write.
const std::string a_checks_body =
    R"({"requestId":"77777777-7777-7777-7777-777777777777","lines":[{"lineNo":1,)"
    R"("operation":"put","subjectId":"88888888-8888-8888-8888-888888888888",)"
    R"("refusal":""}],"partCodes":["book"]})";

bool proposal_holds(const std::string& body, const std::string& part) {
    return body.find(part) != std::string::npos;
}

}

TEST_CASE("a_proposal_is_named_by_its_request", "[workflow_proposal]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the identifier both proposal reads name";

    for (const auto& [method, path, pattern] :
         std::vector<std::tuple<http_method, std::string, std::string>>{
             {http_method::get, "/api/requests/not-a-request/changes", changes_pattern},
             {http_method::get, "/api/requests/not-a-request/checks", checks_pattern}}) {
        const auto refused = answer_at<workflow_routes>(method, path, http_request{}, pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(proposal_holds(refused.body, proposal_refused));
    }
}

TEST_CASE("a_proposal_answers_the_service_is_absent", "[workflow_proposal]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing both proposal reads with no service behind them";

    CHECK(answer_at<workflow_routes>(http_method::get, its_changes, http_request{}, changes_pattern)
              .status == http_status::bad_gateway);
    CHECK(answer_at<workflow_routes>(http_method::get, its_checks, http_request{}, checks_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("a_proposal_refuses_a_parameter_it_does_not_take", "[workflow_proposal]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a parameter a proposal read does not name";

    http_request req;
    req.query_params["asOf"] = "2026-01-01";
    const auto refused =
        answer_at<workflow_routes>(http_method::get, its_changes, req, changes_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(proposal_holds(refused.body, "asOf"));
}

TEST_CASE("what_a_request_proposes_is_kept_for_fifteen_seconds", "[workflow_proposal]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the cache policy of the proposed lines";

    http_request req;
    const auto first = request_proposal_routes::changes_answer(req, a_proposal_body);
    CHECK(first.status == http_status::ok);
    REQUIRE(first.headers.contains("ETag"));
    CHECK(first.headers.at("Cache-Control") == "private, max-age=15");
    CHECK(proposal_holds(first.body, "book_change"));

    http_request revalidation;
    revalidation.headers["if-none-match"] = first.headers.at("ETag");
    CHECK(request_proposal_routes::changes_answer(revalidation, a_proposal_body).status ==
          http_status::not_modified);
}

TEST_CASE("what_running_a_request_says_is_kept_for_half_a_minute", "[workflow_proposal]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the cache policy of the checks";

    http_request req;
    const auto first = request_proposal_routes::checks_answer(req, a_checks_body);
    CHECK(first.status == http_status::ok);
    REQUIRE(first.headers.contains("ETag"));
    CHECK(first.headers.at("Cache-Control") == "private, max-age=30");
    CHECK(proposal_holds(first.body, "partCodes"));

    http_request revalidation;
    revalidation.headers["if-none-match"] = first.headers.at("ETag");
    CHECK(request_proposal_routes::checks_answer(revalidation, a_checks_body).status ==
          http_status::not_modified);
}
