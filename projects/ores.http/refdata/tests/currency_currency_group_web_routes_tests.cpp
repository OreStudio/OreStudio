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
 * Template: cpp_http_route_web_records_tests.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/refdata/currency_currency_group_web_routes.hpp"
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
#include <utility>

using namespace ores::logging;
using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::net::router;
using ores::http::openapi::endpoint_registry;
using ores::http::routes::refdata::currency_currency_group_web_routes;
using ores::nats::service::nats_client;

namespace {

/// Runs a handler to completion. The handler never reaches a socket.
http_response currency_currency_group_web_run(boost::asio::awaitable<http_response> work) {
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

/// A request with these query parameters and this body.
http_request currency_currency_group_web_request(
    std::initializer_list<std::pair<const std::string, std::string>> params,
    std::string body = {}) {
    http_request req;
    req.query_params = params;
    req.body = std::move(body);
    return req;
}

bool currency_currency_group_web_says(const http_response& response, const std::string& code) {
    return response.body.find("\"code\":\"" + code + "\"") != std::string::npos;
}

}

TEST_CASE("currency_currency_group_web_routes_registers_the_screen_addresses", "[routes][web]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    auto table = std::make_shared<router>();
    currency_currency_group_web_routes::register_routes(
        table, std::make_shared<endpoint_registry>(), session);
    const auto& routes = table->routes();

    const auto has = [&](http_method method, const std::string& pattern) {
        return std::any_of(routes.begin(), routes.end(), [&](const auto& route) {
            return route.method == method && route.pattern == pattern && route.requires_auth;
        });
    };
    CHECK(has(http_method::get, "/api/refdata/currency-memberships"));
    CHECK(has(http_method::put, "/api/refdata/currency-memberships"));
    CHECK(has(http_method::delete_, "/api/refdata/currency-memberships"));
    CHECK(has(http_method::get, "/api/refdata/currency-memberships/by/{parent}"));
    BOOST_LOG_SEV(lg, debug) << "Registered " << routes.size() << " route(s).";
}

TEST_CASE("currency_currency_group_web_routes_names_the_resource_in_its_catalogue_entry",
          "[routes][web]") {
    const std::string entry{currency_currency_group_web_routes::catalogue_entry()};
    CHECK(entry.find("\"key\":\"currency-memberships\"") != std::string::npos);
    CHECK(entry.find("\"entityType\":\"ores.refdata.currency_currency_group\"") !=
          std::string::npos);
    CHECK(entry.find("\"writePermission\":\"refdata::currency_currency_groups:write\"") !=
          std::string::npos);
}

TEST_CASE("currency_currency_group_web_routes_refuses_a_page_it_cannot_read", "[routes][web]") {
    nats_client session;
    for (const auto& limit : {"0", "1001", "ten"}) {
        const auto response =
            currency_currency_group_web_run(currency_currency_group_web_routes::handle_list(
                currency_currency_group_web_request({{"limit", limit}}), session));
        INFO(limit);
        CHECK(response.status == http_status::bad_request);
        CHECK(currency_currency_group_web_says(response, "invalid-request"));
    }
}

TEST_CASE(
    "currency_currency_group_web_routes_refuses_an_order_a_search_or_a_filter_it_does_not_declare",
    "[routes][web]") {
    nats_client session;
    const auto sorted =
        currency_currency_group_web_run(currency_currency_group_web_routes::handle_list(
            currency_currency_group_web_request({{"limit", "10"}, {"sort", "no_such_column"}}),
            session));
    CHECK(sorted.status == http_status::bad_request);
    CHECK(sorted.body.find("no_such_column") != std::string::npos);

    const auto filtered =
        currency_currency_group_web_run(currency_currency_group_web_routes::handle_list(
            currency_currency_group_web_request({{"limit", "10"}, {"f.no_such_column", "x"}}),
            session));
    CHECK(filtered.status == http_status::bad_request);
    CHECK(filtered.body.find("no_such_column") != std::string::npos);

    const auto searched =
        currency_currency_group_web_run(currency_currency_group_web_routes::handle_list(
            currency_currency_group_web_request({{"limit", "10"}, {"search", "x"}}), session));
    CHECK(searched.status == http_status::bad_request);
}

TEST_CASE("currency_currency_group_web_routes_accepts_every_order_and_filter_it_declares",
          "[routes][web]") {
    nats_client session;
    // An accepted page fails at the forward step, which is a 502 and not a 400.
    for (const auto& sort : {""}) {
        const auto response =
            currency_currency_group_web_run(currency_currency_group_web_routes::handle_list(
                currency_currency_group_web_request(
                    {{"limit", "10"}, {"sort", sort}, {"descending", "true"}}),
                session));
        INFO(sort);
        CHECK(response.status == http_status::bad_gateway);
    }
    {
        const auto response =
            currency_currency_group_web_run(currency_currency_group_web_routes::handle_list(
                currency_currency_group_web_request({{"limit", "10"}, {"f.currency_iso_code", ""}}),
                session));
        INFO("f.currency_iso_code");
        CHECK(response.status == http_status::bad_gateway);
    }
}

TEST_CASE("currency_currency_group_web_routes_refuses_a_write_that_states_no_reason",
          "[routes][web]") {
    nats_client session;
    const auto put = currency_currency_group_web_run(currency_currency_group_web_routes::handle_put(
        currency_currency_group_web_request({}, "{}"), session));
    CHECK(put.status == http_status::bad_request);

    const auto removal =
        currency_currency_group_web_run(currency_currency_group_web_routes::handle_delete(
            currency_currency_group_web_request({}, "{}"), session));
    CHECK(removal.status == http_status::bad_request);
}
