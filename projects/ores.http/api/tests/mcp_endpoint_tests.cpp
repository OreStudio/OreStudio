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
#include "ores.http.api/mcp/endpoint.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/use_future.hpp>
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>

namespace {

using namespace ores::http::mcp;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::utility::codec::json_document;
using ores::utility::codec::json_value;

class fake_tools final : public tool_provider {
public:
    std::vector<tool> list(const http_request&) const override {
        return {
            tool{.name = "echo",
                 .description = "Answers its words.",
                 .input_schema = R"({"type":"object","properties":{"words":{"type":"string"}}})",
                 .read_only = true}};
    }

    boost::asio::awaitable<std::optional<tool_result>>
    call(const http_request&, std::string_view name, const json_value& arguments) const override {
        if (name == "fail")
            throw std::runtime_error("broken");
        if (name != "echo")
            co_return std::nullopt;
        co_return tool_result{.text = arguments.member("words").text_or()};
    }
};

http_request request_by(const std::string& act, std::string body = {}) {
    http_request req;
    ores::security::jwt::jwt_claims claims;
    claims.subject = "person-1";
    if (!act.empty())
        claims.act = act;
    req.authenticated_user = claims;
    req.body = std::move(body);
    return req;
}

http_response posted(const endpoint& served, const http_request& req) {
    boost::asio::io_context io;
    auto answer = boost::asio::co_spawn(io, served.post(req), boost::asio::use_future);
    io.run();
    return answer.get();
}

endpoint make_endpoint() {
    return endpoint(std::make_shared<const fake_tools>(),
                    endpoint_options{.server_version = "1.2.3"});
}

std::string open_session(const endpoint& served, const std::string& act = "agent:grant-1") {
    const auto answer = posted(
        served,
        request_by(
            act,
            R"({"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18"}})"));
    REQUIRE(answer.status == http_status::ok);
    return answer.headers.at("Mcp-Session-Id");
}

http_response in_session(const endpoint& served,
                         const std::string& session,
                         std::string body,
                         const std::string& act = "agent:grant-1") {
    auto req = request_by(act, std::move(body));
    req.headers["mcp-session-id"] = session;
    return posted(served, req);
}

json_document read(const http_response& answer) {
    auto doc = json_document::parse(answer.body);
    REQUIRE(doc.has_value());
    return std::move(*doc);
}

}

TEST_CASE("initialize_opens_a_session_at_the_version_the_client_asks", "[mcp]") {
    const auto served = make_endpoint();
    const auto answer = posted(
        served,
        request_by(
            "agent:grant-1",
            R"({"jsonrpc":"2.0","id":7,"method":"initialize","params":{"protocolVersion":"2025-06-18"}})"));
    REQUIRE(answer.status == http_status::ok);
    CHECK(answer.headers.at("Mcp-Session-Id").size() == 64);
    const auto doc = read(answer);
    CHECK(doc.root().member("id").integer() == 7);
    const auto result = doc.root().member("result");
    CHECK(result.member("protocolVersion").text_or() == "2025-06-18");
    CHECK(result.member("serverInfo").member("version").text_or() == "1.2.3");
    CHECK(result.member("capabilities").member("tools").is_object());
}

TEST_CASE("initialize_answers_the_latest_version_for_one_it_does_not_serve", "[mcp]") {
    const auto served = make_endpoint();
    const auto answer = posted(
        served,
        request_by(
            "agent:grant-1",
            R"({"jsonrpc":"2.0","id":"a","method":"initialize","params":{"protocolVersion":"1999-01-01"}})"));
    const auto doc = read(answer);
    CHECK(doc.root().member("id").text_or() == "a");
    CHECK(doc.root().member("result").member("protocolVersion").text_or() ==
          std::string(endpoint::latest_version));
}

TEST_CASE("a_token_that_names_no_agent_is_refused", "[mcp]") {
    const auto served = make_endpoint();
    const auto person =
        posted(served, request_by("", R"({"jsonrpc":"2.0","id":1,"method":"initialize"})"));
    CHECK(person.status == http_status::unauthorized);
    CHECK(person.headers.at("WWW-Authenticate") == "Bearer");
    const auto service = posted(
        served, request_by("step:runner", R"({"jsonrpc":"2.0","id":1,"method":"initialize"})"));
    CHECK(service.status == http_status::unauthorized);
}

TEST_CASE("a_request_outside_a_session_is_refused", "[mcp]") {
    const auto served = make_endpoint();
    const auto missing =
        posted(served, request_by("agent:grant-1", R"({"jsonrpc":"2.0","id":1,"method":"ping"})"));
    CHECK(missing.status == http_status::bad_request);
    const auto unknown = in_session(served, "nope", R"({"jsonrpc":"2.0","id":1,"method":"ping"})");
    CHECK(unknown.status == http_status::not_found);
}

TEST_CASE("another_agents_session_is_not_open_for_it", "[mcp]") {
    const auto served = make_endpoint();
    const auto session = open_session(served, "agent:grant-1");
    const auto stranger =
        in_session(served, session, R"({"jsonrpc":"2.0","id":1,"method":"ping"})", "agent:grant-2");
    CHECK(stranger.status == http_status::not_found);
    const auto owner = in_session(served, session, R"({"jsonrpc":"2.0","id":1,"method":"ping"})");
    CHECK(owner.status == http_status::ok);
    CHECK(read(owner).root().member("result").is_object());
}

TEST_CASE("a_notification_is_accepted_without_an_answer", "[mcp]") {
    const auto served = make_endpoint();
    const auto session = open_session(served);
    const auto answer =
        in_session(served, session, R"({"jsonrpc":"2.0","method":"notifications/initialized"})");
    CHECK(answer.status == http_status::accepted);
    CHECK(answer.body.empty());
}

TEST_CASE("tools_list_states_each_tool_with_its_schema", "[mcp]") {
    const auto served = make_endpoint();
    const auto session = open_session(served);
    const auto doc =
        read(in_session(served, session, R"({"jsonrpc":"2.0","id":2,"method":"tools/list"})"));
    const auto tools = doc.root().member("result").member("tools");
    REQUIRE(tools.size() == 1);
    const auto echo = tools.element(0);
    CHECK(echo.member("name").text_or() == "echo");
    CHECK(echo.member("inputSchema").member("properties").member("words").is_object());
    CHECK(echo.member("annotations").member("readOnlyHint").flag() == true);
}

TEST_CASE("tools_call_answers_the_tools_text_or_why_it_failed", "[mcp]") {
    const auto served = make_endpoint();
    const auto session = open_session(served);

    const auto echoed = read(in_session(
        served,
        session,
        R"({"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"echo","arguments":{"words":"hello"}}})"));
    const auto result = echoed.root().member("result");
    CHECK(result.member("content").element(0).member("text").text_or() == "hello");
    CHECK(result.member("isError").flag() == false);

    const auto failed = read(
        in_session(served,
                   session,
                   R"({"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"fail"}})"));
    CHECK(failed.root().member("result").member("isError").flag() == true);

    const auto unknown = read(
        in_session(served,
                   session,
                   R"({"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"nope"}})"));
    CHECK(unknown.root().member("error").member("code").integer() == -32602);
}

TEST_CASE("a_message_that_is_not_one_request_is_refused_as_json_rpc_says", "[mcp]") {
    const auto served = make_endpoint();
    const auto session = open_session(served);
    const auto unparsed = in_session(served, session, "{not json");
    CHECK(unparsed.status == http_status::bad_request);
    CHECK(read(unparsed).root().member("error").member("code").integer() == -32700);
    const auto batch = in_session(served, session, R"([{"jsonrpc":"2.0","id":1,"method":"ping"}])");
    CHECK(batch.status == http_status::bad_request);
    CHECK(read(batch).root().member("error").member("code").integer() == -32600);
    const auto unknown =
        read(in_session(served, session, R"({"jsonrpc":"2.0","id":6,"method":"resources/list"})"));
    CHECK(unknown.root().member("error").member("code").integer() == -32601);
}

TEST_CASE("a_foreign_origin_or_an_unknown_revision_is_refused_and_a_local_one_served", "[mcp]") {
    const auto served = make_endpoint();
    auto from_page =
        request_by("agent:grant-1", R"({"jsonrpc":"2.0","id":1,"method":"initialize"})");
    from_page.headers["origin"] = "https://evil.example";
    CHECK(posted(served, from_page).status == http_status::forbidden);
    from_page.headers["origin"] = "http://localhost.evil.example";
    CHECK(posted(served, from_page).status == http_status::forbidden);
    for (const auto* local : {"http://localhost:3000", "http://127.0.0.1", "http://[::1]:8080"}) {
        from_page.headers["origin"] = local;
        CHECK(posted(served, from_page).status == http_status::ok);
    }

    auto old_client =
        request_by("agent:grant-1", R"({"jsonrpc":"2.0","id":1,"method":"initialize"})");
    old_client.headers["mcp-protocol-version"] = "1999-01-01";
    CHECK(posted(served, old_client).status == http_status::bad_request);
}

TEST_CASE("get_opens_no_stream_and_delete_ends_the_session", "[mcp]") {
    const auto served = make_endpoint();
    const auto session = open_session(served);

    const auto stream = served.get(request_by("agent:grant-1"));
    CHECK(stream.status == http_status::method_not_allowed);
    CHECK(stream.headers.at("Allow") == "POST, DELETE");

    CHECK(served.remove(request_by("agent:grant-1")).status == http_status::bad_request);
    auto end = request_by("agent:grant-1");
    end.headers["mcp-session-id"] = session;
    CHECK(served.remove(end).status == http_status::no_content);
    CHECK(served.remove(end).status == http_status::not_found);
    CHECK(in_session(served, session, R"({"jsonrpc":"2.0","id":1,"method":"ping"})").status ==
          http_status::not_found);
}

TEST_CASE("the_mcp_routes_are_a_transport_the_catalogue_accepts", "[mcp]") {
    auto router = std::make_shared<ores::http::net::router>();
    auto registry = std::make_shared<ores::http::openapi::endpoint_registry>();
    register_routes(std::make_shared<const endpoint>(make_endpoint()), router, registry);
    CHECK(router->uncatalogued().empty());
    for (const auto& route : router->routes()) {
        CHECK(route.catalogue.transport);
        CHECK_FALSE(route.catalogue.writes);
    }
}
