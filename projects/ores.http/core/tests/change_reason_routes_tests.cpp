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
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http.core/routes/change_reason_routes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <optional>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::routes::change_reason_routes;

namespace {

/// The answer the change reasons route gives a request, on a client that is not connected.
http_response reasons_answer(const http_request& query) {
    ores::nats::service::nats_client session;
    auto table = std::make_shared<ores::http::net::router>();
    change_reason_routes::register_routes(
        table, std::make_shared<ores::http::openapi::endpoint_registry>(), session);
    http_request req = query;
    const auto route = table->match(http_method::get, "/api/change-reasons", req.path_params);
    REQUIRE(route.has_value());
    REQUIRE(route->requires_auth);
    boost::asio::io_context io;
    std::optional<http_response> answer;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> { answer = co_await route->handler(req); },
        boost::asio::detached);
    io.run();
    REQUIRE(answer.has_value());
    return *answer;
}

}

TEST_CASE("change_reasons_read_a_page_of_1_to_1000", "[change_reason_routes]") {
    for (const std::string limit : {"0", "1001", "x"}) {
        http_request req;
        req.query_params["limit"] = limit;
        CHECK(reasons_answer(req).status == http_status::bad_request);
    }
    http_request negative;
    negative.query_params["offset"] = "-1";
    CHECK(reasons_answer(negative).status == http_status::bad_request);

    // The client is not connected, so an accepted read fails at the forward step.
    CHECK(reasons_answer(http_request{}).status == http_status::bad_gateway);
    http_request page;
    page.query_params["offset"] = "200";
    page.query_params["limit"] = "1000";
    CHECK(reasons_answer(page).status == http_status::bad_gateway);
}
