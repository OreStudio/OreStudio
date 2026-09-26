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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_http_route_tests.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/assets/tag_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <string>
#include <string_view>

using namespace ores::logging;
using ores::http::domain::http_method;
using ores::http::net::router;
using ores::http::openapi::endpoint_registry;
using ores::http::routes::assets::tag_routes;
using ores::nats::service::nats_client;

namespace {

const std::string_view test_suite("ores.http.assets.tests");
const std::string tags("[routes]");

/// One route the unit must register, as the derivation states it.
struct expected_route {
    http_method method;
    std::string pattern;
    bool requires_auth;
};

/// A router holding the unit's routes, registered against a bare client.
///
/// Registration touches no transport: the client is only what each route
/// forwards on, so an unconnected one is enough to read the route table.
std::shared_ptr<router> registered_routes(nats_client& session) {
    auto table = std::make_shared<router>();
    auto registry = std::make_shared<endpoint_registry>();
    tag_routes::register_routes(table, registry, session);
    return table;
}

}

TEST_CASE("tag_routes_registers_every_derived_route", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    const auto table = registered_routes(session);
    const auto& routes = table->routes();

    REQUIRE(routes.size() == 10);

    // The router's own list is the only public view of what was registered,
    // so a route that is missing from it was never registered.
    for (const auto& expected : {
             expected_route{http_method::get, std::string{"/api/v1/assets/tags"}, true},
             expected_route{http_method::get, std::string{"/api/v1/assets/tags/{name}"}, true},
             expected_route{http_method::post, std::string{"/api/v1/assets/tags/get-many"}, true},
             expected_route{http_method::post, std::string{"/api/v1/assets/tags"}, true},
             expected_route{http_method::put, std::string{"/api/v1/assets/tags"}, true},
             expected_route{http_method::post, std::string{"/api/v1/assets/tags/put-many"}, true},
             expected_route{http_method::delete_, std::string{"/api/v1/assets/tags/{name}"}, true},
             expected_route{
                 http_method::post, std::string{"/api/v1/assets/tags/delete-many"}, true},
             expected_route{
                 http_method::get, std::string{"/api/v1/assets/tags/{name}/versions"}, true},
             expected_route{http_method::get,
                            std::string{"/api/v1/assets/tags/{name}/versions/{version}"},
                            true},
         }) {
        const auto found = std::find_if(routes.begin(), routes.end(), [&](const auto& route) {
            return route.method == expected.method && route.pattern == expected.pattern;
        });
        CHECK(found != routes.end());
    }

    BOOST_LOG_SEV(lg, debug) << "Registered 10 route(s).";
}

TEST_CASE("tag_routes_requires_a_session_where_the_operation_does", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    const auto table = registered_routes(session);
    const auto& routes = table->routes();

    // The flag is the operation's own stated requirement. A route that needs
    // a session cannot be registered without one, and one that does not is
    // not made unreachable by a flag the model never asked for.
    for (const auto& expected : {
             expected_route{http_method::get, std::string{"/api/v1/assets/tags"}, true},
             expected_route{http_method::get, std::string{"/api/v1/assets/tags/{name}"}, true},
             expected_route{http_method::post, std::string{"/api/v1/assets/tags/get-many"}, true},
             expected_route{http_method::post, std::string{"/api/v1/assets/tags"}, true},
             expected_route{http_method::put, std::string{"/api/v1/assets/tags"}, true},
             expected_route{http_method::post, std::string{"/api/v1/assets/tags/put-many"}, true},
             expected_route{http_method::delete_, std::string{"/api/v1/assets/tags/{name}"}, true},
             expected_route{
                 http_method::post, std::string{"/api/v1/assets/tags/delete-many"}, true},
             expected_route{
                 http_method::get, std::string{"/api/v1/assets/tags/{name}/versions"}, true},
             expected_route{http_method::get,
                            std::string{"/api/v1/assets/tags/{name}/versions/{version}"},
                            true},
         }) {
        const auto found = std::find_if(routes.begin(), routes.end(), [&](const auto& route) {
            return route.method == expected.method && route.pattern == expected.pattern;
        });
        REQUIRE(found != routes.end());
        CHECK(found->requires_auth == expected.requires_auth);
        // The builder refuses a route that states no position, so every
        // registered route must carry the declaration as well as the value.
        CHECK(found->auth_declared);
    }

    BOOST_LOG_SEV(lg, debug) << "Checked the auth flag of 10 route(s).";
}
