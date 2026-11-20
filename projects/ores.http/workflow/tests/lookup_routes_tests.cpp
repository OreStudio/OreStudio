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

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::workflow::workflow_routes;
using ores::http::workflow::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string mode_pattern = "/api/approvals-mode";
const std::string reasons_pattern = "/api/hold-reasons";

bool lookup_holds(const std::string& body, const std::string& part) {
    return body.find(part) != std::string::npos;
}

}

TEST_CASE("the_approvals_mode_reaches_the_service", "[workflow_lookups]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the approvals mode with no service behind it";

    CHECK(answer_at<workflow_routes>(http_method::get, mode_pattern, http_request{}, mode_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("the_approvals_mode_refuses_a_parameter_it_does_not_take", "[workflow_lookups]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a parameter the approvals mode does not name";

    http_request req;
    req.query_params["tenant"] = "acme";
    const auto refused =
        answer_at<workflow_routes>(http_method::get, mode_pattern, req, mode_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(lookup_holds(refused.body, "tenant"));
}

TEST_CASE("the_hold_reasons_reach_the_service", "[workflow_lookups]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the hold reasons with no service behind it";

    CHECK(answer_at<workflow_routes>(
              http_method::get, reasons_pattern, http_request{}, reasons_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("the_hold_reasons_refuse_a_parameter_they_do_not_take", "[workflow_lookups]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a parameter the hold reasons do not name";

    http_request req;
    req.query_params["kind"] = "trading.book_change";
    const auto refused =
        answer_at<workflow_routes>(http_method::get, reasons_pattern, req, reasons_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(lookup_holds(refused.body, "kind"));
}
