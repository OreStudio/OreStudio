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
#include "ores.http/routes/workflow/progress_routes.hpp"
#include "ores.http/routes/workflow/workflow_routes.hpp"
#include "ores.logging/logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::workflow::progress_routes;
using ores::http::routes::workflow::workflow_routes;
using ores::http::workflow::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string progress_pattern = "/api/workflow/{instanceId}";

const std::string a_progress_run_id = "55555555-5555-5555-5555-555555555555";
const std::string a_progress_run = "/api/workflow/" + a_progress_run_id;

const std::string progress_run_refused = "A run is named by its identifier.";

/// A run's answer in the shape the engine writes it: one completed step.
const std::string a_run_body =
    R"({"success":true,"message":"","status":"completed","error":"","step_count":1,)"
    R"("steps":[{"id":"step-1","name":"load","label":"Load the trades","description":"",)"
    R"("status":"completed","step_index":0,"created_at":"2026-11-18 10:00:00.000000Z",)"
    R"("started_at":"2026-11-18 10:00:01.000000Z",)"
    R"("completed_at":"2026-11-18 10:00:02.000000Z","error":"","log":[],)"
    R"("progress":"","progress_done":null,"progress_total":null}]})";

bool progress_holds(const std::string& body, const std::string& part) {
    return body.find(part) != std::string::npos;
}

}

TEST_CASE("a_run_is_named_by_its_identifier", "[workflow_progress]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the identifier a run's progress names";

    const auto refused = answer_at<workflow_routes>(
        http_method::get, "/api/workflow/not-a-run", http_request{}, progress_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(progress_holds(refused.body, progress_run_refused));
}

TEST_CASE("a_run_with_no_service_behind_it_is_refused_as_upstream", "[workflow_progress]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a run with no service behind it";

    CHECK(answer_at<workflow_routes>(
              http_method::get, a_progress_run, http_request{}, progress_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("a_run_refuses_a_parameter_it_does_not_take", "[workflow_progress]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a parameter a run's progress does not name";

    http_request req;
    req.query_params["steps"] = "all";
    const auto refused =
        answer_at<workflow_routes>(http_method::get, a_progress_run, req, progress_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(progress_holds(refused.body, "steps"));
}

TEST_CASE("a_runs_progress_is_kept_privately_for_five_seconds", "[workflow_progress]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the cache policy a run's progress carries";

    http_request req;
    const auto first = progress_routes::progress_answer(req, a_run_body);
    CHECK(first.status == http_status::ok);
    REQUIRE(first.headers.contains("ETag"));
    CHECK(first.headers.at("Cache-Control") == "private, max-age=5");
    CHECK(first.headers.at("Vary") == "Authorization, Cookie");
    CHECK(progress_holds(first.body, "Load the trades"));

    http_request revalidation;
    revalidation.headers["if-none-match"] = first.headers.at("ETag");
    const auto revalidated = progress_routes::progress_answer(revalidation, a_run_body);
    CHECK(revalidated.status == http_status::not_modified);
    CHECK(revalidated.headers.at("ETag") == first.headers.at("ETag"));

    http_request changed;
    changed.headers["if-none-match"] = first.headers.at("ETag");
    const auto moved = progress_routes::progress_answer(
        changed, a_run_body.substr(0, a_run_body.size() - 1) + R"(],"status":"running"})");
    CHECK(moved.status == http_status::ok);
    CHECK(moved.headers.at("ETag") != first.headers.at("ETag"));
}
