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
#include "ores.http/routes/reporting/report_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::reporting::report_routes;
using ores::http::reporting::tests::answer_at;
using namespace ores::logging;
namespace reporting_domain = ores::reporting::domain;

namespace {

const auto report_id = "00000000-0000-0000-0000-0000000000a1";
const auto report_party = "00000000-0000-0000-0000-0000000000b1";
const auto report_run = "00000000-0000-0000-0000-0000000000c1";

bool report_holds(const std::string& text, const std::string& part) {
    return text.find(part) != std::string::npos;
}

reporting_domain::report_definition definition_of(const char* name) {
    reporting_domain::report_definition row;
    row.id = boost::uuids::string_generator()(report_id);
    row.party_id = boost::uuids::string_generator()(report_party);
    row.name = name;
    row.report_type = "risk";
    row.schedule_expression = "0 0 * * *";
    return row;
}

report_routes::party_name party_of(const char* name, const char* code) {
    report_routes::party_name party;
    party.id = boost::uuids::string_generator()(report_party);
    party.name = name;
    party.short_code = code;
    return party;
}

report_routes::run_row run_of() {
    report_routes::run_row run;
    run.id = boost::uuids::string_generator()(report_run);
    run.definition_id = boost::uuids::string_generator()(report_id);
    return run;
}

http_status reports_status(const std::string& query) {
    http_request req;
    if (!query.empty()) {
        const auto at = query.find('=');
        req.query_params[query.substr(0, at)] = query.substr(at + 1);
    }
    return answer_at<report_routes>(
               http_method::get, "/api/reporting/reports", req, "/api/reporting/reports")
        .status;
}

}

TEST_CASE("reports_state_a_row_from_its_definitions_and_decorations", "[report]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a report row";

    const auto answer = report_routes::rows_json(
        {definition_of("Daily risk")},
        std::vector{party_of("ACME", "AC")},
        std::vector{run_of()},
        {{boost::uuids::string_generator()(report_run), "Succeeded", "completed clean"}},
        std::vector<report_routes::count_row>{{boost::uuids::string_generator()(report_run), 2, 5}},
        std::vector<report_routes::fire_row>{});

    CHECK(report_holds(answer, R"("name":"Daily risk")"));
    CHECK(report_holds(answer, R"("reportType":"risk")"));
    CHECK(report_holds(answer, R"("partyName":"ACME")"));
    CHECK(report_holds(answer, R"("partyCode":"AC")"));
    CHECK(report_holds(answer, R"("scheduled":false)"));
    CHECK(report_holds(answer, R"("health":"completed clean")"));
    CHECK(report_holds(answer, R"("stateName":"Succeeded")"));
    CHECK(report_holds(answer, R"("errors":2)"));
    CHECK(report_holds(answer, R"("warnings":5)"));
    CHECK(report_holds(answer, R"("cutKind":null)"));
    CHECK(report_holds(answer, R"("nextFireAt":null)"));
}

TEST_CASE("reports_state_never_run_and_the_cells_a_failed_read_leaves_out", "[report]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the cells a read that failed leaves out";

    const auto never_ran = report_routes::rows_json({definition_of("Daily risk")},
                                                    std::vector{party_of("ACME", "")},
                                                    std::vector<report_routes::run_row>{},
                                                    {},
                                                    std::vector<report_routes::count_row>{},
                                                    std::vector<report_routes::fire_row>{});
    CHECK(report_holds(never_ran, R"("health":"never run")"));
    CHECK(report_holds(never_ran, R"("latestRun":null)"));
    CHECK(report_holds(never_ran, R"("partyCode":null)"));

    // Every decoration unread: each cell is absent and none is guessed.
    const auto unread = report_routes::rows_json(
        {definition_of("Daily risk")}, std::nullopt, std::nullopt, {}, std::nullopt, std::nullopt);
    CHECK(report_holds(unread, R"("health":null)"));
    CHECK(report_holds(unread, R"("partyName":"")"));
    CHECK(unread == report_routes::rows_json({definition_of("Daily risk")},
                                             std::nullopt,
                                             std::nullopt,
                                             {},
                                             std::nullopt,
                                             std::nullopt));
}

TEST_CASE("reports_refuse_a_page_they_do_not_read", "[report]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the list's page";

    CHECK(reports_status("") == http_status::bad_gateway);
    CHECK(reports_status("limit=0") == http_status::bad_request);
    CHECK(reports_status("limit=1001") == http_status::bad_request);
    CHECK(reports_status("limit=many") == http_status::bad_request);
    CHECK(reports_status("descending=down") == http_status::bad_request);
    CHECK(reports_status("party_id=not-an-id") == http_status::bad_request);
    CHECK(reports_status("colour=red") == http_status::bad_request);
}

TEST_CASE("reports_refuse_a_detail_address_that_names_no_record", "[report]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the detail address";

    const auto status = [](const std::string& address, const std::string& pattern) {
        return answer_at<report_routes>(http_method::get, address, http_request{}, pattern).status;
    };
    CHECK(status("/api/reporting/reports/by-name/" + std::string(201, 'a'),
                 "/api/reporting/reports/by-name/{name}") == http_status::bad_request);
    CHECK(status("/api/reporting/reports/by-name/a-report",
                 "/api/reporting/reports/by-name/{name}") == http_status::bad_gateway);
    CHECK(status("/api/reporting/reports/" + std::string(201, 'a'),
                 "/api/reporting/reports/{segment}") == http_status::bad_request);
    CHECK(status("/api/reporting/reports/not-an-id/configurations",
                 "/api/reporting/reports/{id}/configurations") == http_status::bad_request);
    CHECK(status("/api/reporting/reports/" + std::string(report_id),
                 "/api/reporting/reports/{segment}") == http_status::bad_gateway);
}

TEST_CASE("reports_refuse_a_write_they_do_not_read", "[report]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a definition's write";

    const auto put = [](const std::string& body) {
        http_request req;
        req.body = body;
        return answer_at<report_routes>(http_method::put,
                                        "/api/reporting/reports/" + std::string(report_id),
                                        req,
                                        "/api/reporting/reports/{id}")
            .status;
    };

    CHECK(put("") == http_status::bad_request);
    CHECK(put(R"({"version":1})") == http_status::bad_request);
    CHECK(put(R"({"version":1,"change":{},"intent":{"reason_code":"system.update"}})") ==
          http_status::bad_request);
    CHECK(put(R"({"version":1,"change":{"description":"","report_type":"risk",)"
              R"("concurrency_policy":"replace","schedule_expression":"0 0 * * *",)"
              R"("is_official":true,"pre_processing":"","prepared_input_key":"",)"
              R"("post_processing":"","skipped_families":""},"intent":{}})") ==
          http_status::bad_request);
    CHECK(put(R"({"version":1,"change":{"description":"","report_type":"risk",)"
              R"("concurrency_policy":"replace","schedule_expression":"0 0 * * *",)"
              R"("is_official":true,"pre_processing":"","prepared_input_key":"",)"
              R"("post_processing":"","skipped_families":""},)"
              R"("intent":{"reason_code":"system.update"}})") == http_status::bad_gateway);
}

TEST_CASE("reports_refuse_a_run_address_that_names_no_definition", "[report]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the run routes' addresses";

    const auto status =
        [](http_method method, const std::string& address, const std::string& pattern) {
            return answer_at<report_routes>(method, address, http_request{}, pattern).status;
        };
    CHECK(status(http_method::post,
                 "/api/reporting/reports/not-an-id/run",
                 "/api/reporting/reports/{id}/run") == http_status::bad_request);
    CHECK(status(http_method::get,
                 "/api/reporting/reports/not-an-id/runs",
                 "/api/reporting/reports/{id}/runs") == http_status::bad_request);
    CHECK(status(http_method::get,
                 "/api/reporting/reports/" + std::string(report_id) + "/runs",
                 "/api/reporting/reports/{id}/runs") == http_status::bad_gateway);
}

TEST_CASE("reports_refuse_a_run_started_by_a_token_that_names_no_tenant", "[report]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a run start without a tenant";

    http_request req;
    req.authenticated_user.emplace();

    CHECK(answer_at<report_routes>(http_method::post,
                                   "/api/reporting/reports/" + std::string(report_id) + "/run",
                                   req,
                                   "/api/reporting/reports/{id}/run")
              .status == http_status::bad_request);
}
