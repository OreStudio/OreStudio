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
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/refdata/party_contact_information_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <initializer_list>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

using namespace ores::logging;
using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::net::router;
using ores::http::openapi::endpoint_registry;
using ores::http::routes::refdata::party_contact_information_routes;
using ores::nats::service::nats_client;

namespace {

/// One route the unit must register, as the derivation states it.
struct party_contact_information_expected_route {
    http_method method;
    std::string pattern;
    bool requires_auth;
};

/// A router holding the unit's routes, registered against a bare client.
///
/// Registration touches no transport: the client is only what each route
/// forwards on, so an unconnected one is enough to read the route table.
std::shared_ptr<router> party_contact_information_registered_routes(nats_client& session) {
    auto table = std::make_shared<router>();
    auto registry = std::make_shared<endpoint_registry>();
    party_contact_information_routes::register_routes(table, registry, session);
    return table;
}

/// Runs a handler to completion. The handler never reaches a socket, so the
/// context has no I/O to wait on.
[[maybe_unused]] http_response
party_contact_information_run(boost::asio::awaitable<http_response> work) {
    boost::asio::io_context io;
    std::optional<http_response> answer;
    boost::asio::co_spawn(
        io,
        [&answer, work = std::move(work)]() mutable -> boost::asio::awaitable<void> {
            answer = co_await std::move(work);
        },
        boost::asio::detached);
    io.run();
    return answer.value_or(http_response{});
}

/// A GET request that carries only these query parameters.
[[maybe_unused]] http_request party_contact_information_query(
    std::initializer_list<std::pair<const std::string, std::string>> params) {
    http_request req;
    req.method = http_method::get;
    req.query_params = params;
    return req;
}

}

TEST_CASE("party_contact_information_routes_registers_every_derived_route", "[routes]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    const auto table = party_contact_information_registered_routes(session);
    const auto& routes = table->routes();

    REQUIRE(routes.size() == 11);

    // The router's own list is the only public view of what was registered,
    // so a route that is missing from it was never registered.
    for (const auto& expected : {
             party_contact_information_expected_route{
                 http_method::get,
                 std::string{"/api/v1/reference-data/party_contact_informations"},
                 true},
             party_contact_information_expected_route{
                 http_method::get,
                 std::string{"/api/v1/reference-data/party_contact_informations/{contact_type}"},
                 true},
             party_contact_information_expected_route{
                 http_method::post,
                 std::string{"/api/v1/reference-data/party_contact_informations/get-many"},
                 true},
             party_contact_information_expected_route{
                 http_method::post,
                 std::string{"/api/v1/reference-data/party_contact_informations"},
                 true},
             party_contact_information_expected_route{
                 http_method::put,
                 std::string{"/api/v1/reference-data/party_contact_informations"},
                 true},
             party_contact_information_expected_route{
                 http_method::post,
                 std::string{"/api/v1/reference-data/party_contact_informations/put-many"},
                 true},
             party_contact_information_expected_route{
                 http_method::delete_,
                 std::string{"/api/v1/reference-data/party_contact_informations/{contact_type}"},
                 true},
             party_contact_information_expected_route{
                 http_method::post,
                 std::string{"/api/v1/reference-data/party_contact_informations/delete-many"},
                 true},
             party_contact_information_expected_route{
                 http_method::get,
                 std::string{
                     "/api/v1/reference-data/party_contact_informations/by-party-id/{party_id}"},
                 true},
             party_contact_information_expected_route{
                 http_method::get,
                 std::string{
                     "/api/v1/reference-data/party_contact_informations/{contact_type}/versions"},
                 true},
             party_contact_information_expected_route{
                 http_method::get,
                 std::string{"/api/v1/reference-data/party_contact_informations/{contact_type}/"
                             "versions/{version}"},
                 true},
         }) {
        const auto found = std::find_if(routes.begin(), routes.end(), [&](const auto& route) {
            return route.method == expected.method && route.pattern == expected.pattern;
        });
        CHECK(found != routes.end());
    }

    BOOST_LOG_SEV(lg, debug) << "Registered 11 route(s).";
}

TEST_CASE("party_contact_information_routes_requires_a_session_where_the_operation_does",
          "[routes]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    const auto table = party_contact_information_registered_routes(session);
    const auto& routes = table->routes();

    // The flag is the operation's own stated requirement. A route that needs
    // a session cannot be registered without one, and one that does not is
    // not made unreachable by a flag the model never asked for.
    for (const auto& expected : {
             party_contact_information_expected_route{
                 http_method::get,
                 std::string{"/api/v1/reference-data/party_contact_informations"},
                 true},
             party_contact_information_expected_route{
                 http_method::get,
                 std::string{"/api/v1/reference-data/party_contact_informations/{contact_type}"},
                 true},
             party_contact_information_expected_route{
                 http_method::post,
                 std::string{"/api/v1/reference-data/party_contact_informations/get-many"},
                 true},
             party_contact_information_expected_route{
                 http_method::post,
                 std::string{"/api/v1/reference-data/party_contact_informations"},
                 true},
             party_contact_information_expected_route{
                 http_method::put,
                 std::string{"/api/v1/reference-data/party_contact_informations"},
                 true},
             party_contact_information_expected_route{
                 http_method::post,
                 std::string{"/api/v1/reference-data/party_contact_informations/put-many"},
                 true},
             party_contact_information_expected_route{
                 http_method::delete_,
                 std::string{"/api/v1/reference-data/party_contact_informations/{contact_type}"},
                 true},
             party_contact_information_expected_route{
                 http_method::post,
                 std::string{"/api/v1/reference-data/party_contact_informations/delete-many"},
                 true},
             party_contact_information_expected_route{
                 http_method::get,
                 std::string{
                     "/api/v1/reference-data/party_contact_informations/by-party-id/{party_id}"},
                 true},
             party_contact_information_expected_route{
                 http_method::get,
                 std::string{
                     "/api/v1/reference-data/party_contact_informations/{contact_type}/versions"},
                 true},
             party_contact_information_expected_route{
                 http_method::get,
                 std::string{"/api/v1/reference-data/party_contact_informations/{contact_type}/"
                             "versions/{version}"},
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

    BOOST_LOG_SEV(lg, debug) << "Checked the auth flag of 11 route(s).";
}

// The client below is not connected. A request the route accepts therefore
// fails at the forward step with a server error, and only a request the route
// refuses is a bad request. That difference is what each case reads.
TEST_CASE("party_contact_information_routes_list_accepts_a_page_and_an_order", "[routes]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    const auto response =
        party_contact_information_run(party_contact_information_routes::handle_list(
            party_contact_information_query(
                {{"offset", "10"}, {"limit", "5"}, {"sort", "version"}, {"descending", "true"}}),
            session));

    CHECK(response.status == http_status::internal_server_error);
    BOOST_LOG_SEV(lg, debug) << "Response: " << response.body;
}

TEST_CASE("party_contact_information_routes_list_accepts_every_filter_it_declares", "[routes]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    // Each name is the one its reader reads. An empty value narrows nothing,
    // so the case isolates the declaration from the reading of a value.
    const std::vector<std::string> names{
        "f.party_id",
        "f.id_one_of",
        "f.party_id_one_of",
    };
    for (const auto& name : names) {
        const auto response =
            party_contact_information_run(party_contact_information_routes::handle_list(
                party_contact_information_query({{name, ""}}), session));
        INFO(name);
        CHECK(response.status == http_status::internal_server_error);
    }
    BOOST_LOG_SEV(lg, debug) << "Checked " << names.size() << " declared filter(s).";
}

TEST_CASE("party_contact_information_routes_list_refuses_an_undeclared_filter", "[routes]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    const auto response =
        party_contact_information_run(party_contact_information_routes::handle_list(
            party_contact_information_query({{"f.no_such_column", "x"}}), session));

    CHECK(response.status == http_status::bad_request);
    CHECK(response.body.find("no_such_column") != std::string::npos);
    BOOST_LOG_SEV(lg, debug) << "Response: " << response.body;
}

TEST_CASE("party_contact_information_routes_list_refuses_an_empty_item_in_f_id_one_of",
          "[routes]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    const auto response =
        party_contact_information_run(party_contact_information_routes::handle_list(
            party_contact_information_query({{"f.id_one_of", ","}}), session));

    CHECK(response.status == http_status::bad_request);
    CHECK(response.body.find("f.id_one_of") != std::string::npos);
    BOOST_LOG_SEV(lg, debug) << "Response: " << response.body;
}

TEST_CASE("party_contact_information_routes_list_refuses_an_empty_item_in_f_party_id_one_of",
          "[routes]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    const auto response =
        party_contact_information_run(party_contact_information_routes::handle_list(
            party_contact_information_query({{"f.party_id_one_of", ","}}), session));

    CHECK(response.status == http_status::bad_request);
    CHECK(response.body.find("f.party_id_one_of") != std::string::npos);
    BOOST_LOG_SEV(lg, debug) << "Response: " << response.body;
}
