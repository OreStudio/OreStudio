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
#include "ores.http/routes/analytics/analytics_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::analytics::analytics_routes;
using ores::http::analytics::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string keys_pattern = "/api/analytics/sensitivity-templates";
const std::string a_key_pattern = "/api/analytics/sensitivity-templates/{code}";
const std::string its_history_pattern = "/api/analytics/sensitivity-templates/{code}/history";

bool templates_holds(const std::string& body, const std::string& part) {
    return body.find(part) != std::string::npos;
}

}

TEST_CASE("a_page_of_keys_refuses_a_parameter_it_does_not_take", "[analytics_templates]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a parameter a page of keys does not name";

    http_request req;
    req.query_params["filter"] = "FD";
    const auto refused =
        answer_at<analytics_routes>(http_method::get, keys_pattern, req, keys_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(templates_holds(refused.body, "filter"));
}

TEST_CASE("a_page_of_keys_refuses_a_limit_it_cannot_read", "[analytics_templates]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a limit outside the range a page takes";

    http_request req;
    req.query_params["limit"] = "5000";
    const auto refused =
        answer_at<analytics_routes>(http_method::get, keys_pattern, req, keys_pattern);
    CHECK(refused.status == http_status::bad_request);
}

TEST_CASE("a_key_is_named_by_its_code", "[analytics_templates]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the code a key answers to";

    const std::string too_long(101, 'x');
    const auto refused =
        answer_at<analytics_routes>(http_method::get,
                                    "/api/analytics/sensitivity-templates/" + too_long,
                                    http_request{},
                                    a_key_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(templates_holds(refused.body, "named by its code"));
}

TEST_CASE("a_page_of_keys_answers_the_service_is_absent", "[analytics_templates]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a page with no service behind it";

    CHECK(answer_at<analytics_routes>(http_method::get, keys_pattern, http_request{}, keys_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("a_keys_history_answers_the_service_is_absent", "[analytics_templates]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a history read with no service behind it";

    CHECK(answer_at<analytics_routes>(http_method::get,
                                      "/api/analytics/sensitivity-templates/FD.IR.1/history",
                                      http_request{},
                                      its_history_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("a_key_states_the_shape_of_the_body_it_takes", "[analytics_templates]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a create body that names a member it does not take";

    http_request req;
    req.body = R"({"code":"FD.IR.1","methodCode":"FD","assetClassCode":"interest_rates",)"
               R"("description":"","reasonCode":"new","commentary":"","shiftSize":1})";
    const auto refused =
        answer_at<analytics_routes>(http_method::post, keys_pattern, req, keys_pattern);
    CHECK(refused.status == http_status::bad_request);
    // The body is refused by the sentence the screen reads, which names the
    // shape and not the member this one happened to carry.
    CHECK(templates_holds(refused.body, "states its code, its method"));
}
