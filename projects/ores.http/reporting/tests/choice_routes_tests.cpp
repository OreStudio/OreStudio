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
#include "ores.http/routes/reporting/choice_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::reporting::choice_routes;
using ores::http::reporting::tests::answer_at;
using namespace ores::logging;

TEST_CASE("choices_state_the_codes_a_report_form_offers", "[choice]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the codes a report's edit form reads";

    const auto answer = choice_routes::choices_json({"risk"}, {"replace"}, {"Banking", "Trading"});

    CHECK(answer == R"({"reportTypes":["risk"],"concurrencyPolicies":["replace"],)"
                    R"("regulatoryBookTypes":["Banking","Trading"]})");
    CHECK(choice_routes::choices_json({}, {}, {}) ==
          R"({"reportTypes":[],"concurrencyPolicies":[],"regulatoryBookTypes":[]})");
}

TEST_CASE("choices_refuse_a_read_without_a_service", "[choice]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the choices' refusal when the service does not answer";

    CHECK(answer_at<choice_routes>(
              http_method::get, "/api/reporting/choices", http_request{}, "/api/reporting/choices")
              .status == http_status::bad_gateway);
}

TEST_CASE("choices_refuse_a_query_they_do_not_read", "[choice]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a query the choices read does not name";

    http_request req;
    req.query_params["party"] = "00000000-0000-0000-0000-0000000000a1";

    CHECK(answer_at<choice_routes>(
              http_method::get, "/api/reporting/choices", req, "/api/reporting/choices")
              .status == http_status::bad_request);
}
