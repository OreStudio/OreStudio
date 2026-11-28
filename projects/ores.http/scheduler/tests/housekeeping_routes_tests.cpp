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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.http.api/domain/http_method.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/scheduler/housekeeping_routes.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <string>
#include <vector>

using namespace ores::logging;
using ores::http::domain::http_method;
using ores::http::net::router;
using ores::http::openapi::endpoint_registry;
using ores::http::routes::scheduler::housekeeping_routes;
using ores::nats::service::nats_client;

namespace {

struct expected_route {
    http_method method;
    std::string pattern;
};

/// The routes the unit registers, read from a router with no transport behind it.
///
/// Registration touches no transport: the client is only what each route
/// forwards on, so an unconnected one is enough to read the route table.
std::vector<expected_route> registered_routes(nats_client& session) {
    auto table = std::make_shared<router>();
    auto registry = std::make_shared<endpoint_registry>();
    housekeeping_routes::register_routes(table, registry, session);
    std::vector<expected_route> routes;
    for (const auto& route : table->routes())
        routes.push_back(expected_route{route.method, route.pattern});
    return routes;
}

}

TEST_CASE("the housekeeping unit registers the screen's four routes", "[housekeeping]") {
    auto lg(make_logger(test_suite));
    nats_client session;

    const auto routes = registered_routes(session);

    const auto holds = [&routes](http_method method, std::string_view pattern) {
        for (const auto& route : routes) {
            if (route.method == method && route.pattern == pattern)
                return true;
        }
        return false;
    };

    CHECK(holds(http_method::get, "/api/operations/housekeeping/stores"));
    CHECK(holds(http_method::get, "/api/operations/housekeeping/stores/{code}/samples"));
    CHECK(holds(http_method::get, "/api/operations/housekeeping/capacity"));
    CHECK(holds(http_method::put, "/api/operations/housekeeping/stores/{code}/policy"));

    ORES_LOG_SEV(lg, debug) << "Registered " << routes.size() << " housekeeping routes";
}

TEST_CASE("every housekeeping route admits only a session", "[housekeeping]") {
    auto lg(make_logger(test_suite));
    nats_client session;

    auto table = std::make_shared<router>();
    auto registry = std::make_shared<endpoint_registry>();
    housekeeping_routes::register_routes(table, registry, session);

    REQUIRE(!table->routes().empty());
    for (const auto& route : table->routes()) {
        CHECK(route.requires_auth);
        CHECK_FALSE(route.pattern.empty());
    }

    ORES_LOG_SEV(lg, debug) << "Checked " << table->routes().size() << " routes for a session";
}
