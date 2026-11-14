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
#include "ores.http/routes/reporting/configuration_routes.hpp"
#include "ores.http/routes/reporting/cut_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::reporting::tests::answer_at;
using ores::http::routes::reporting::cut_routes;
using namespace ores::logging;

namespace {

const std::string cuts_address = "/api/reporting/cuts";
const std::string cut_address = "/api/reporting/cuts/00000000-0000-0000-0000-0000000000c1";
const std::string cut_pattern = "/api/reporting/cuts/{id}";
const std::string runs_pattern = "/api/reporting/cuts/{id}/runs";
const std::string take_address = "/api/reporting/cuts/take";

/// A take body the floor sends, with one member left out on request.
std::string a_take(const std::string& without = "") {
    std::string body = "{";
    const std::pair<std::string, std::string> members[] = {
        {"kindCode", R"("end_of_day")"},
        {"scopeCode", R"("firm")"},
        {"businessCentreCode", R"("WRLD")"},
        {"timepoint", R"("2026-11-13T17:00:00Z")"},
        {"timepointTimezone", R"("Europe/London")"},
        {"cutTimeCode", R"("eod")"},
        {"reason", R"("the close")"},
        {"preview", "false"}};
    bool first = true;
    for (const auto& [name, value] : members) {
        if (!without.empty() && name == without)
            continue;
        if (!first)
            body += ",";
        body += "\"" + name + "\":" + value;
        first = false;
    }
    return body + "}";
}

}

TEST_CASE("the_cuts_refuse_a_page_or_a_narrowing_they_do_not_read", "[cut]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the cuts list's query";

    // A page states a limit, as the BFF's own schema required.
    CHECK(answer_at<cut_routes>(http_method::get, cuts_address, http_request{}, cuts_address).status ==
          http_status::bad_request);

    http_request sorted;
    sorted.query_params["limit"] = "10";
    sorted.query_params["sort"] = "label";
    CHECK(answer_at<cut_routes>(http_method::get, cuts_address, sorted, cuts_address).status ==
          http_status::bad_request);

    http_request stated;
    stated.query_params["limit"] = "10";
    stated.query_params["state"] = "nearly_taken";
    CHECK(answer_at<cut_routes>(http_method::get, cuts_address, stated, cuts_address).status ==
          http_status::bad_request);
}

TEST_CASE("a_cut_refuses_an_id_that_is_not_a_uuid", "[cut]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a cut's address";

    http_request req;
    req.path_params["id"] = "not-an-id";
    CHECK(answer_at<cut_routes>(http_method::get, "/api/reporting/cuts/not-an-id", req, cut_pattern)
              .status == http_status::bad_request);
    CHECK(answer_at<cut_routes>(http_method::get,
                                "/api/reporting/cuts/not-an-id/runs",
                                req,
                                runs_pattern)
              .status == http_status::bad_request);
}

TEST_CASE("a_take_refuses_a_body_it_does_not_state", "[cut]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the take's shape";

    const auto status = [](const std::string& body) {
        http_request req;
        req.body = body;
        return answer_at<cut_routes>(http_method::post, take_address, req, take_address).status;
    };

    // Every member the floor sends is stated, and the instant needs its clock.
    CHECK(status(a_take("kindCode")) == http_status::bad_request);
    CHECK(status(a_take("timepointTimezone")) == http_status::bad_request);
    CHECK(status(R"({"kindCode":"end_of_day","scopeCode":"firm"})") == http_status::bad_request);
}

TEST_CASE("a_take_with_no_service_is_refused_as_upstream", "[cut]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a take with no service behind it";

    http_request req;
    req.body = a_take();
    CHECK(answer_at<cut_routes>(http_method::post, take_address, req, take_address).status ==
          http_status::bad_gateway);
}

TEST_CASE("the_runs_of_a_cut_with_no_service_are_refused_as_upstream", "[cut]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a cut's runs with no service behind it";

    CHECK(answer_at<cut_routes>(http_method::get,
                                cut_address + "/runs",
                                http_request{},
                                runs_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("a_configuration_refuses_an_id_that_is_not_a_uuid", "[configuration]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a configuration's address";

    http_request req;
    req.path_params["id"] = "not-an-id";
    CHECK(answer_at<ores::http::routes::reporting::configuration_routes>(
              http_method::get,
              "/api/reporting/configurations/not-an-id",
              req,
              "/api/reporting/configurations/{id}")
              .status == http_status::bad_request);
}
