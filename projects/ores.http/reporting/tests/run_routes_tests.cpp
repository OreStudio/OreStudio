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
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http/routes/reporting/run_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::reporting::run_routes;
using ores::http::reporting::tests::answer_at;
using namespace ores::logging;

namespace {

const auto runs_definition = "00000000-0000-0000-0000-0000000000a1";
const auto runs_run = "00000000-0000-0000-0000-0000000000c1";

}

TEST_CASE("reports_refuse_a_run_address_that_names_no_definition", "[report]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the run routes' addresses";

    const auto status =
        [](http_method method, const std::string& address, const std::string& pattern) {
            return answer_at<run_routes>(method, address, http_request{}, pattern).status;
        };
    CHECK(status(http_method::post,
                 "/api/reporting/reports/not-an-id/run",
                 "/api/reporting/reports/{id}/run") == http_status::bad_request);
    CHECK(status(http_method::get,
                 "/api/reporting/reports/not-an-id/runs",
                 "/api/reporting/reports/{id}/runs") == http_status::bad_request);
    CHECK(status(http_method::get,
                 "/api/reporting/reports/" + std::string(runs_definition) + "/runs",
                 "/api/reporting/reports/{id}/runs") == http_status::bad_gateway);
}

TEST_CASE("reports_refuse_a_run_started_by_a_token_that_names_no_tenant", "[report]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a run start without a tenant";

    http_request req;
    req.authenticated_user.emplace();

    CHECK(answer_at<run_routes>(http_method::post,
                                "/api/reporting/reports/" + std::string(runs_definition) + "/run",
                                req,
                                "/api/reporting/reports/{id}/run")
              .status == http_status::bad_request);
}

TEST_CASE("reports_refuse_a_run_page_for_an_address_that_is_no_id", "[report]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a run's page and its problems";

    const auto status = [](const std::string& address, const std::string& pattern) {
        return answer_at<run_routes>(http_method::get, address, http_request{}, pattern).status;
    };
    CHECK(status("/api/reporting/runs/not-an-id", "/api/reporting/runs/{id}") ==
          http_status::bad_request);
    CHECK(status("/api/reporting/runs/not-an-id/errors", "/api/reporting/runs/{id}/errors") ==
          http_status::bad_request);
    CHECK(status("/api/reporting/runs/" + std::string(runs_run), "/api/reporting/runs/{id}") ==
          http_status::bad_gateway);
    CHECK(status("/api/reporting/runs/" + std::string(runs_run) + "/errors",
                 "/api/reporting/runs/{id}/errors") == http_status::bad_gateway);
}

TEST_CASE("runs_read_newest_first_and_a_run_with_no_start_last", "[run]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the order the runs of a report are read in";

    const auto at = [](int day) {
        return std::optional<std::chrono::system_clock::time_point>{
            std::chrono::system_clock::time_point{
                std::chrono::sys_days{std::chrono::year{2026} / std::chrono::March / day}}};
    };
    const auto run = [&](const char* id,
                         std::optional<std::chrono::system_clock::time_point> started) {
        run_routes::run_view row;
        row.id = boost::uuids::string_generator()(id);
        row.name = id;
        row.health = "completed clean";
        row.state_name = "Succeeded";
        row.started_at = started;
        return row;
    };

    const auto answer =
        run_routes::runs_json({run("00000000-0000-0000-0000-0000000000a1", at(14)),
                               run("00000000-0000-0000-0000-0000000000a2", std::nullopt),
                               run("00000000-0000-0000-0000-0000000000a3", at(16))});

    CHECK(answer.find("00000000-0000-0000-0000-0000000000a3") <
          answer.find("00000000-0000-0000-0000-0000000000a1"));
    CHECK(answer.find("00000000-0000-0000-0000-0000000000a1") <
          answer.find("00000000-0000-0000-0000-0000000000a2"));
    CHECK(answer.find(R"("state_name":"Succeeded")") != std::string::npos);
    CHECK(answer.find(R"("health":"completed clean")") != std::string::npos);
    CHECK(answer.find(R"("duration_ms":null)") != std::string::npos);
}
