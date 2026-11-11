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
 * Template: cpp_http_route_web_classification_tests.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/refdata/calendar_type_classification_routes.hpp"
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
using ores::http::routes::refdata::calendar_type_classification_routes;
using ores::nats::service::nats_client;

namespace {

http_response calendar_type_classification_run(boost::asio::awaitable<http_response> work) {
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

http_request calendar_type_classification_request(std::string body, std::string code = "X") {
    http_request req;
    req.body = std::move(body);
    req.path_params["code"] = std::move(code);
    return req;
}

}

TEST_CASE("calendar_type_classification_routes_registers_the_screen_addresses",
          "[routes][classification]") {
    nats_client session;
    auto table = std::make_shared<router>();
    calendar_type_classification_routes::register_routes(
        table, std::make_shared<endpoint_registry>(), session);
    const auto& routes = table->routes();
    const auto has = [&](http_method method, const std::string& pattern) {
        return std::any_of(routes.begin(), routes.end(), [&](const auto& route) {
            return route.method == method && route.pattern == pattern && route.requires_auth;
        });
    };
    CHECK(has(http_method::get, "/api/classifications/calendar-type"));
    CHECK(has(http_method::post, "/api/classifications/calendar-type/rows"));
    CHECK(has(http_method::put, "/api/classifications/calendar-type/rows/{code}"));
    CHECK(has(http_method::put, "/api/classifications/calendar-type/order"));
    CHECK(has(http_method::delete_, "/api/classifications/calendar-type/rows/{code}"));
    CHECK(has(http_method::put, "/api/classifications/calendar-type/rows/{code}/label"));
}

TEST_CASE("calendar_type_classification_routes_states_its_list_in_the_catalogue",
          "[routes][classification]") {
    const auto entry = calendar_type_classification_routes::catalogue_entry(7);
    CHECK(entry.find("\"key\":\"calendar-type\"") != std::string::npos);
    CHECK(entry.find("\"shape\":\"named\"") != std::string::npos);
    CHECK(entry.find("\"count\":7") != std::string::npos);
    CHECK(
        calendar_type_classification_routes::catalogue_entry(std::nullopt).find("\"count\":null") !=
        std::string::npos);
}

TEST_CASE("calendar_type_classification_routes_refuses_a_write_that_states_no_reason",
          "[routes][classification]") {
    nats_client session;
    // A list of ORE spellings refuses a write before it reads the body.
    const auto expected = http_status::bad_request;
    CHECK(calendar_type_classification_run(calendar_type_classification_routes::handle_create(
                                               calendar_type_classification_request("{}"), session))
              .status == expected);
    CHECK(calendar_type_classification_run(calendar_type_classification_routes::handle_update(
                                               calendar_type_classification_request("{}"), session))
              .status == expected);
    CHECK(calendar_type_classification_run(calendar_type_classification_routes::handle_order(
                                               calendar_type_classification_request("{}"), session))
              .status == expected);
    CHECK(calendar_type_classification_run(calendar_type_classification_routes::handle_delete(
                                               calendar_type_classification_request("{}"), session))
              .status == expected);
}

TEST_CASE("calendar_type_classification_routes_labels_a_row_of_any_list",
          "[routes][classification]") {
    nats_client session;
    // A label is how a code is drawn, so even a list of ORE spellings is labelled.
    const auto refused =
        calendar_type_classification_run(calendar_type_classification_routes::handle_label(
            calendar_type_classification_request("{}"), session));
    CHECK(refused.status == http_status::bad_request);
    const auto accepted =
        calendar_type_classification_run(calendar_type_classification_routes::handle_label(
            calendar_type_classification_request(R"({"badgeCode":null,"reasonCode":"x"})"),
            session));
    CHECK(accepted.status == http_status::bad_gateway);
}
