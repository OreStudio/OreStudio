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
#include "ores.http/routes/iam/audit_routes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <limits>
#include <map>
#include <memory>
#include <optional>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::routes::iam::audit_routes;
using ores::nats::service::nats_client;

namespace {

http_request audit_request(std::map<std::string, std::string> query,
                           std::map<std::string, std::string> path = {}) {
    http_request req;
    for (auto& [k, v] : query)
        req.query_params[k] = v;
    for (auto& [k, v] : path)
        req.path_params[k] = v;
    return req;
}

http_response audit_call(nats_client& session,
                         http_method method,
                         const std::string& pattern,
                         const http_request& req) {
    auto router = std::make_shared<ores::http::net::router>();
    audit_routes::register_routes(
        router, std::make_shared<ores::http::openapi::endpoint_registry>(), session);
    const auto& routes = router->routes();
    const auto found = std::ranges::find_if(
        routes, [&](const auto& r) { return r.method == method && r.pattern == pattern; });
    REQUIRE(found != routes.end());
    std::optional<http_response> answer;
    boost::asio::io_context io;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> { answer = co_await found->handler(req); },
        boost::asio::detached);
    io.run();
    return answer.value_or(http_response{});
}

const std::initializer_list<std::string_view> window_keys{"accountId", "period", "offset", "limit"};

}

TEST_CASE("audit_routes_read_a_window_as_the_bff_did", "[audit]") {
    const auto now = std::chrono::system_clock::now();
    const auto day = audit_routes::read_query(audit_request({}), window_keys, now);
    REQUIRE(day);
    CHECK(day->limit == 100);
    CHECK(day->to == now);
    CHECK(*day->to - *day->from == std::chrono::hours(24));
    const auto all = audit_routes::read_query(audit_request({{"period", "all"}}), window_keys, now);
    REQUIRE(all);
    CHECK_FALSE(all->from);
    const auto week =
        audit_routes::read_query(audit_request({{"period", "week"}}), window_keys, now);
    CHECK(*week->to - *week->from == std::chrono::hours(24 * 7));
    CHECK_FALSE(audit_routes::read_query(audit_request({{"period", "month"}}), window_keys, now));
    CHECK_FALSE(audit_routes::read_query(audit_request({{"limit", "501"}}), window_keys, now));
    CHECK_FALSE(
        audit_routes::read_query(audit_request({{"eventType", "login"}}), window_keys, now));
    CHECK_FALSE(audit_routes::read_query(
        audit_request({{"accountId", std::string(65, 'a')}}), window_keys, now));
}

TEST_CASE("audit_routes_answer_in_the_screen_shapes", "[audit]") {
    ores::iam::messaging::list_auth_events_response events;
    ores::iam::messaging::auth_event e;
    e.event_type = "login_success";
    e.error_detail = "none";
    events.events.push_back(e);
    const auto ev = audit_routes::events_json(events);
    for (const auto* key : {"\"eventType\":\"login_success\"",
                            "\"eventTime\":",
                            "\"accountId\":",
                            "\"sessionId\":",
                            "\"partyId\":",
                            "\"errorDetail\":\"none\""})
        CHECK(ev.find(key) != std::string::npos);

    ores::iam::messaging::get_session_statistics_response stats;
    ores::iam::messaging::session_statistics_row row;
    row.day = "2026-11-11";
    row.session_count = 3;
    row.avg_duration_seconds = 1.5;
    stats.rows.push_back(row);
    const auto st = audit_routes::statistics_json(stats);
    for (const auto* key : {"\"day\":\"2026-11-11\"",
                            "\"sessionCount\":3",
                            "\"avgDurationSeconds\":1.5",
                            "\"uniqueCountries\":0"})
        CHECK(st.find(key) != std::string::npos);
}

TEST_CASE("audit_routes_refuse_before_forwarding", "[audit]") {
    nats_client session;
    CHECK(audit_call(
              session, http_method::get, "/api/auth-events", audit_request({{"period", "year"}}))
              .status == http_status::bad_request);
    CHECK(audit_call(session, http_method::get, "/api/geo/country", audit_request({})).status ==
          http_status::bad_request);
    CHECK(audit_call(session,
                     http_method::get,
                     "/api/geo/country",
                     audit_request({{"address", "1.2.3.4"}, {"extra", "x"}}))
              .status == http_status::bad_request);
    CHECK(audit_call(session,
                     http_method::post,
                     "/api/sessions/{sessionId}/end",
                     audit_request({}, {{"sessionId", "nope"}}))
              .status == http_status::bad_request);
}

TEST_CASE("audit_routes_forward_an_accepted_request", "[audit]") {
    nats_client session;
    CHECK(audit_call(session, http_method::get, "/api/session-statistics", audit_request({}))
              .status == http_status::bad_gateway);
    CHECK(
        audit_call(
            session, http_method::get, "/api/geo/country", audit_request({{"address", "1.2.3.4"}}))
            .status == http_status::bad_gateway);
}

TEST_CASE("audit_routes_bound_the_window_and_state_no_number_json_cannot_hold", "[audit]") {
    const auto now = std::chrono::system_clock::now();
    CHECK(audit_routes::read_query(
        audit_request({{"accountId", std::string(64, 'a')}}), window_keys, now));
    CHECK_FALSE(audit_routes::read_query(audit_request({{"limit", "0"}}), window_keys, now));
    CHECK_FALSE(audit_routes::read_query(audit_request({{"offset", "x"}}), window_keys, now));

    ores::iam::messaging::get_session_statistics_response stats;
    ores::iam::messaging::session_statistics_row row;
    row.avg_bytes_sent = std::numeric_limits<double>::quiet_NaN();
    row.avg_bytes_received = std::numeric_limits<double>::infinity();
    stats.rows.push_back(row);
    const auto text = audit_routes::statistics_json(stats);
    CHECK(text.find("\"avgBytesSent\":null") != std::string::npos);
    CHECK(text.find("\"avgBytesReceived\":null") != std::string::npos);
}
