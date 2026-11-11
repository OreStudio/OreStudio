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
#include "ores.http/routes/refdata/pick_list_routes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <optional>
#include <string>
#include <utility>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::net::router;
using ores::http::openapi::endpoint_registry;
using ores::http::routes::refdata::pick_list_routes;
using ores::nats::service::nats_client;

TEST_CASE("pick_list_routes_registers_each_form_s_lists", "[pick_list_routes]") {
    nats_client session;
    auto table = std::make_shared<router>();
    pick_list_routes::register_routes(table, std::make_shared<endpoint_registry>(), session);
    for (const auto* form : {"books", "conventions", "counterparties", "tenors"}) {
        const auto pattern = std::string("/api/") + form + "/pick-lists";
        INFO(pattern);
        CHECK(std::any_of(table->routes().begin(), table->routes().end(), [&](const auto& r) {
            return r.method == http_method::get && r.pattern == pattern && r.requires_auth;
        }));
    }
}

TEST_CASE("pick_list_routes_forwards_every_list_read", "[pick_list_routes]") {
    nats_client session;
    auto table = std::make_shared<router>();
    pick_list_routes::register_routes(table, std::make_shared<endpoint_registry>(), session);
    // The client is not connected, so a list read fails at the forward step.
    for (const auto& route : table->routes()) {
        boost::asio::io_context io;
        std::optional<http_response> answer;
        http_request req;
        boost::asio::co_spawn(
            io,
            [&]() -> boost::asio::awaitable<void> { answer = co_await route.handler(req); },
            boost::asio::detached);
        io.run();
        INFO(route.pattern);
        REQUIRE(answer.has_value());
        CHECK(answer->status == http_status::bad_gateway);
    }
}
