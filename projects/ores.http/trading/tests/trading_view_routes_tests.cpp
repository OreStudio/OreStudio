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
#include "ores.http/routes/trading/trading_view_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::trading::trading_view_routes;
using ores::http::trading::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string tree_body = R"({"portfolios":[{"id":"p1"}],"books":[{"id":"b1"}]})";

bool view_holds(const ores::http::domain::http_response& answer, const std::string& header) {
    return answer.headers.contains(header);
}

}

TEST_CASE("trading_views_keep_the_book_tree_for_a_minute", "[trading_view]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the book tree's cache policy";

    http_request req;
    const auto first = trading_view_routes::tree_answer(req, tree_body);
    CHECK(first.status == http_status::ok);
    CHECK(first.body == tree_body);
    REQUIRE(view_holds(first, "ETag"));
    REQUIRE(view_holds(first, "Cache-Control"));
    CHECK(first.headers.at("Cache-Control") == "private, max-age=60");
    CHECK(first.headers.at("Vary") == "Authorization, Cookie");

    http_request second;
    second.headers["if-none-match"] = first.headers.at("ETag");
    const auto revalidated = trading_view_routes::tree_answer(second, tree_body);
    CHECK(revalidated.status == http_status::not_modified);
    CHECK(revalidated.body.empty());
    CHECK(revalidated.headers.at("ETag") == first.headers.at("ETag"));
}

TEST_CASE("trading_views_keep_the_blotter_for_half_a_minute", "[trading_view]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the blotter's cache policy";

    const std::string blotter_body = R"({"tree":[],"rows":[],"total":0})";
    http_request req;
    const auto first = trading_view_routes::blotter_answer(req, blotter_body);
    CHECK(first.status == http_status::ok);
    CHECK(first.headers.at("Cache-Control") == "private, max-age=30");
    REQUIRE(view_holds(first, "ETag"));

    http_request second;
    second.headers["if-none-match"] = first.headers.at("ETag");
    const auto revalidated = trading_view_routes::blotter_answer(second, blotter_body);
    CHECK(revalidated.status == http_status::not_modified);
    CHECK(revalidated.body.empty());
}

TEST_CASE("trading_views_refuse_a_page_the_query_malforms", "[trading_view]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the blotter's query checks";

    const auto status_of = [](const std::string& name, const std::string& value) {
        http_request req;
        req.query_params[name] = value;
        return answer_at<trading_view_routes>(http_method::get, "/api/blotter", req, "/api/blotter")
            .status;
    };
    CHECK(status_of("view", "desk") == http_status::bad_request);
    CHECK(status_of("flat", "yes") == http_status::bad_request);
    CHECK(status_of("limit", "0") == http_status::bad_request);
    CHECK(status_of("limit", "201") == http_status::bad_request);
    CHECK(status_of("sort", "notional") == http_status::bad_request);
    CHECK(status_of("descending", "1") == http_status::bad_request);
    CHECK(status_of("node", std::string(101, 'n')) == http_status::bad_request);
    CHECK(status_of("sort", "status") == http_status::bad_gateway);
}

TEST_CASE("trading_views_refuse_a_view_without_a_service", "[trading_view]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the book tree's refusal when the services do not answer";

    CHECK(answer_at<trading_view_routes>(
              http_method::get, "/api/books/tree", http_request{}, "/api/books/tree")
              .status == http_status::bad_gateway);
}
