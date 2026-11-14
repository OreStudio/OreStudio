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
#include "ores.http/routes/reporting/schedule_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::reporting::schedule_routes;
using namespace ores::logging;

namespace {

const std::string schedule_address = "/api/reporting/reports/schedule";
const std::string unschedule_address = "/api/reporting/reports/unschedule";

const std::string an_id = "00000000-0000-0000-0000-0000000000d1";
const std::string another_id = "00000000-0000-0000-0000-0000000000d2";

http_status post_of(const std::string& address, const std::string& body) {
    http_request req;
    req.body = body;
    return ores::http::reporting::tests::answer_at<schedule_routes>(http_method::post,
                                                                    address,
                                                                    req,
                                                                    address)
        .status;
}

/// A schedule body naming the ids, and the expression when one is given.
std::string a_schedule(const std::string& ids, const std::string& expression = "") {
    std::string body = "{\"ids\":[" + ids + "]";
    if (!expression.empty())
        body += ",\"schedule_expression\":\"" + expression + "\"";
    return body + "}";
}

}

TEST_CASE("a_schedule_refuses_the_definitions_it_does_not_name", "[schedule]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the schedule body's definitions";

    // A schedule names at least one definition, and names it by its id.
    CHECK(post_of(schedule_address, "{\"ids\":[]}") == http_status::bad_request);
    CHECK(post_of(schedule_address, "{\"ids\":[\"not-an-id\"]}") == http_status::bad_request);
    CHECK(post_of(schedule_address, "{}") == http_status::bad_request);
    CHECK(post_of(unschedule_address, "{\"ids\":[]}") == http_status::bad_request);
    CHECK(post_of(unschedule_address, "{\"ids\":[\"not-an-id\"]}") == http_status::bad_request);
}

TEST_CASE("a_schedule_expression_names_one_definition", "[schedule]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an expression and a list do not mix";

    // The expression is written onto one definition, so a body that states one
    // names a single id and no other.
    CHECK(post_of(schedule_address, a_schedule("\"" + an_id + "\",\"" + another_id + "\"", "0 17 * * *")) ==
          http_status::bad_request);
    CHECK(post_of(schedule_address, a_schedule("\"" + an_id + "\"", "0 17 * * *")) ==
          http_status::bad_gateway);
}

TEST_CASE("a_schedule_expression_is_one_of_1_to_200_characters", "[schedule]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the expression's length";

    CHECK(post_of(schedule_address, a_schedule("\"" + an_id + "\"", "   ")) ==
          http_status::bad_request);
    const std::string too_long(201, 'x');
    CHECK(post_of(schedule_address, a_schedule("\"" + an_id + "\"", too_long)) ==
          http_status::bad_request);
}

TEST_CASE("a_schedule_with_no_service_is_refused_as_upstream", "[schedule]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a schedule with no service behind it";

    CHECK(post_of(schedule_address, a_schedule("\"" + an_id + "\"")) ==
          http_status::bad_gateway);
    CHECK(post_of(unschedule_address, a_schedule("\"" + an_id + "\"")) ==
          http_status::bad_gateway);
    CHECK(post_of(unschedule_address,
                  a_schedule("\"" + an_id + "\",\"" + another_id + "\"")) ==
          http_status::bad_gateway);
}
