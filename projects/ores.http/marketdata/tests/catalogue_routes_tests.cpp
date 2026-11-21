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
#include "ores.http/routes/marketdata/catalogue_routes.hpp"
#include "ores.http/routes/marketdata/marketdata_routes.hpp"
#include "ores.logging/logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::marketdata::catalogue_routes;
using ores::http::routes::marketdata::marketdata_routes;
using ores::http::marketdata::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string catalogue_pattern = "/api/marketdata/catalogue";

}

TEST_CASE("the_catalogue_reaches_the_service_and_takes_no_query", "[marketdata_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the catalogue with no service behind it";

    CHECK(answer_at<marketdata_routes>(
              http_method::get, catalogue_pattern, http_request{}, catalogue_pattern)
              .status == http_status::bad_gateway);

    http_request req;
    req.query_params["code"] = "BLOOMBERG";
    const auto refused =
        answer_at<marketdata_routes>(http_method::get, catalogue_pattern, req, catalogue_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(refused.body.find("code") != std::string::npos);
}

TEST_CASE("the_catalogue_is_kept_privately_for_five_minutes", "[marketdata_views]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the cache policy the catalogue carries";

    http_request req;
    const auto first = catalogue_routes::catalogue_answer(req, "{}");
    CHECK(first.status == http_status::ok);
    REQUIRE(first.headers.contains("ETag"));
    CHECK(first.headers.at("Cache-Control") == "private, max-age=300");
    CHECK(first.headers.at("Vary") == "Authorization, Cookie");

    http_request revalidation;
    revalidation.headers["if-none-match"] = first.headers.at("ETag");
    CHECK(catalogue_routes::catalogue_answer(revalidation, "{}").status ==
          http_status::not_modified);
}
