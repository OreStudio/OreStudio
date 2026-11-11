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
#include "ores.http.core/routes/history_routes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <initializer_list>
#include <optional>
#include <string>
#include <utility>

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::routes::history_routes;
using ores::nats::service::nats_client;

namespace {

http_response history_run(boost::asio::awaitable<http_response> work) {
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

http_request
history_query(std::initializer_list<std::pair<const std::string, std::string>> params) {
    http_request req;
    req.query_params = params;
    return req;
}

}

TEST_CASE("history_routes_serves_the_types_the_screens_read", "[history_routes]") {
    CHECK(history_routes::serves("ores.refdata.currency"));
    CHECK(history_routes::serves("ores.iam.tenant"));
    CHECK(history_routes::serves("ores.synthetic.fx_spot_generation_config"));
    CHECK(history_routes::serves("ores.refdata.monetary_nature"));
    CHECK_FALSE(history_routes::serves("ores.refdata.currency_country"));
    CHECK_FALSE(history_routes::serves("ores.nothing.thing"));
}

TEST_CASE("history_routes_refuses_a_record_it_cannot_name", "[history_routes]") {
    nats_client session;
    const auto unnamed = history_run(history_routes::handle_history(
        history_query({{"entityType", "ores.refdata.currency"}}), session));
    CHECK(unnamed.status == http_status::bad_request);

    const auto unknown = history_run(history_routes::handle_history(
        history_query({{"entityType", "ores.nothing.thing"}, {"entityId", "X"}}), session));
    CHECK(unknown.status == http_status::not_found);
    CHECK(unknown.body.find("ores.nothing.thing") != std::string::npos);
}

TEST_CASE("history_routes_forwards_a_record_it_serves", "[history_routes]") {
    nats_client session;
    // The client is not connected, so an accepted request fails at the forward step.
    const auto response = history_run(history_routes::handle_history(
        history_query({{"entityType", "ores.refdata.currency"}, {"entityId", "USD"}}), session));
    CHECK(response.status == http_status::bad_gateway);
}
