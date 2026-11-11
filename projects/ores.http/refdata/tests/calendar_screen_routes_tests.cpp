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
#include "ores.http/routes/refdata/calendar_screen_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::refdata::tests::answer_at;
using ores::http::routes::refdata::calendar_screen_routes;
using namespace ores::logging;

namespace {

const std::string days_pattern = "/api/refdata/calendars/{code}/days";
const std::string rebuild_pattern = "/api/refdata/calendars/{code}/rebuild";

http_response days_in(const std::string& year) {
    http_request req;
    if (!year.empty())
        req.query_params["year"] = year;
    return answer_at<calendar_screen_routes>(
        http_method::get, "/api/refdata/calendars/TARGET/days", req, days_pattern);
}

http_response rebuilt_to(const std::string& body) {
    http_request req;
    req.body = body;
    return answer_at<calendar_screen_routes>(
        http_method::post, "/api/refdata/calendars/TARGET/rebuild", req, rebuild_pattern);
}

}

TEST_CASE("calendar_days_are_read_one_year_from_1900_to_2200", "[calendar_screen]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the years a calendar's days are read for";

    for (const std::string year : {"", "1899", "2201", "x", "2026.5"})
        CHECK(days_in(year).status == http_status::bad_request);

    // The client is not connected, so an accepted read fails at the forward step.
    CHECK(days_in("1900").status == http_status::bad_gateway);
    CHECK(days_in("2200").status == http_status::bad_gateway);
}

TEST_CASE("calendar_rebuild_names_its_last_year_from_1900_to_2200", "[calendar_screen]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the years a calendar is built to";

    for (const std::string body :
         {"{}", R"({"endYear":1899})", R"({"endYear":2201})", R"({"endYear":"2030"})", "not json"})
        CHECK(rebuilt_to(body).status == http_status::bad_request);

    CHECK(rebuilt_to(R"({"endYear":2030})").status == http_status::bad_gateway);
}
