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
#include "ores.http/routes/refdata/counterparty_screen_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <optional>
#include <string>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::net::router;
using ores::http::openapi::endpoint_registry;
using ores::http::routes::refdata::counterparty_screen_routes;
using ores::nats::service::nats_client;
using namespace ores::logging;

namespace {

const std::string first = "00000000-0000-0000-0000-00000000000a";
const std::string second = "00000000-0000-0000-0000-00000000000b";
const std::string party = "00000000-0000-0000-0000-00000000000c";

std::shared_ptr<router> counterparty_table(nats_client& session) {
    auto table = std::make_shared<router>();
    counterparty_screen_routes::register_routes(
        table, std::make_shared<endpoint_registry>(), session);
    return table;
}

/// The answer the route at this address gives, through the router as a browser reaches it.
http_response answer_at(http_method method,
                        const std::string& path,
                        http_request req,
                        std::string expected_pattern) {
    nats_client session;
    const auto table = counterparty_table(session);
    const auto route = table->match(method, path, req.path_params);
    REQUIRE(route.has_value());
    REQUIRE(route->pattern == expected_pattern);
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

http_request with_body(std::string body) {
    http_request req;
    req.body = std::move(body);
    return req;
}

const std::string intent = R"("intent":{"reason_code":"system.new_record"})";

}

TEST_CASE("counterparty_centre_changes_link_before_they_close", "[counterparty_screen]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the business centre writes and their order";

    const auto changes = counterparty_screen_routes::changes_of({"GBLO", "USNY", "GBLO"},
                                                                {"DEFR", "GBLO", "DEFR", "JPTO"});
    CHECK(changes.links == std::vector<std::string>{"DEFR", "JPTO"});
    CHECK(changes.unlinks == std::vector<std::string>{"USNY"});

    const auto same = counterparty_screen_routes::changes_of({"GBLO"}, {"GBLO"});
    CHECK(same.links.empty());
    CHECK(same.unlinks.empty());
}

TEST_CASE("counterparty_children_read_names_1_to_200_identifiers", "[counterparty_screen]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the identifiers a children read accepts";

    const auto ids = counterparty_screen_routes::read_ids(first + ",," + second + ",");
    REQUIRE(ids);
    CHECK(*ids == std::vector<std::string>{first, second});

    const std::string upper = "00000000-0000-0000-0000-00000000000A";
    const auto folded = counterparty_screen_routes::read_ids(upper + "," + first + "," + second);
    REQUIRE(folded);
    CHECK(*folded == std::vector<std::string>{first, second});

    CHECK_FALSE(counterparty_screen_routes::read_ids(""));
    CHECK_FALSE(counterparty_screen_routes::read_ids(","));
    CHECK_FALSE(counterparty_screen_routes::read_ids(first + ",not-an-id"));

    const auto numbered = [](int n) {
        const auto digits = std::to_string(n);
        return "00000000-0000-0000-0000-" + std::string(12 - digits.size(), '0') + digits;
    };
    std::string many = numbered(0);
    for (int i = 1; i < 200; ++i)
        many += "," + numbered(i);
    CHECK(counterparty_screen_routes::read_ids(many));
    CHECK_FALSE(counterparty_screen_routes::read_ids(many + "," + numbered(200)));
}

TEST_CASE("counterparty_children_are_grouped_in_the_order_named", "[counterparty_screen]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the children read's shape";

    boost::uuids::string_generator uuid;
    ores::refdata::domain::counterparty_business_centre centre;
    centre.counterparty_id = uuid(second);
    centre.business_centre_code = "GBLO";

    const auto out = counterparty_screen_routes::children_json({second, first}, {}, {}, {centre});
    CHECK(out.starts_with(R"({"children":[{"counterpartyId":")" + second +
                          R"(","identifiers":[],"contacts":[],"centres":[{)"));
    CHECK(out.find("\"business_centre_code\":\"GBLO\"") != std::string::npos);
    CHECK(out.ends_with(R"(]},{"counterpartyId":")" + first +
                        R"(","identifiers":[],"contacts":[],"centres":[]}]})"));
}

TEST_CASE("counterparty_routes_refuse_a_malformed_request_before_forwarding",
          "[counterparty_screen]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing each route's refusals";

    const auto refused = [](const http_response& answer, const std::string& words) {
        CHECK(answer.status == http_status::bad_request);
        CHECK(answer.body.find(words) != std::string::npos);
    };

    refused(answer_at(http_method::get,
                      "/api/counterparties/not-an-id",
                      http_request{},
                      "/api/counterparties/{id}"),
            "named by its identifier");

    // The literal address reaches its own route, not the identifier's.
    http_request children;
    children.query_params["ids"] = "x";
    refused(answer_at(http_method::get,
                      "/api/counterparties/children",
                      children,
                      "/api/counterparties/children"),
            "comma separated");

    refused(answer_at(http_method::put,
                      "/api/counterparties/composite",
                      with_body(R"({"counterparty":{"id":")" + first + R"("}})"),
                      "/api/counterparties/composite"),
            "a reason");
    refused(answer_at(http_method::put,
                      "/api/counterparties/composite",
                      with_body(R"({"counterparty":{},)" + intent + "}"),
                      "/api/counterparties/composite"),
            "with its id");

    refused(answer_at(http_method::put,
                      "/api/counterparties/" + first + "/visibility",
                      with_body(R"({"partyId":"nope",)" + intent + "}"),
                      "/api/counterparties/{id}/visibility"),
            "names the party");

    refused(answer_at(http_method::put,
                      "/api/counterparties/" + first + "/business-centres",
                      with_body(R"({"codes":["GBLO","  "],)" + intent + "}"),
                      "/api/counterparties/{id}/business-centres"),
            "1 to 100 characters");
    refused(answer_at(http_method::put,
                      "/api/counterparties/" + first + "/business-centres",
                      with_body(R"({"codes":["GBLO"],"intent":{"reason_code":"  "}})"),
                      "/api/counterparties/{id}/business-centres"),
            "a reason");
}

TEST_CASE("counterparty_routes_forward_a_well_formed_request", "[counterparty_screen]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an accepted request reaches the service";

    // The client is not connected, so an accepted request fails at the forward step.
    const auto unanswered = [](const http_response& answer) {
        CHECK(answer.status == http_status::bad_gateway);
    };
    unanswered(answer_at(http_method::get,
                         "/api/counterparties/" + first,
                         http_request{},
                         "/api/counterparties/{id}"));
    http_request children;
    children.query_params["ids"] = first + "," + second;
    unanswered(answer_at(http_method::get,
                         "/api/counterparties/children",
                         children,
                         "/api/counterparties/children"));
    unanswered(answer_at(http_method::get,
                         "/api/counterparties/" + first + "/visibility",
                         http_request{},
                         "/api/counterparties/{id}/visibility"));
    unanswered(answer_at(http_method::put,
                         "/api/counterparties/" + first + "/visibility",
                         with_body(R"({"partyId":")" + party + R"(",)" + intent + "}"),
                         "/api/counterparties/{id}/visibility"));
    unanswered(answer_at(http_method::put,
                         "/api/counterparties/" + first + "/business-centres",
                         with_body(R"({"codes":[" GBLO "],)" + intent + "}"),
                         "/api/counterparties/{id}/business-centres"));
}
