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
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http/routes/refdata/party_page_routes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <memory>
#include <optional>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::net::router;
using ores::http::openapi::endpoint_registry;
using ores::http::routes::refdata::party_page_routes;
using ores::nats::service::nats_client;

namespace {

http_request page_request(std::map<std::string, std::string> query, std::string tenant) {
    http_request req;
    for (auto& [k, v] : query)
        req.query_params[k] = v;
    ores::security::jwt::jwt_claims claims;
    claims.tenant_id = std::move(tenant);
    req.authenticated_user = claims;
    return req;
}

http_response answer_of(nats_client& session, const http_request& req) {
    auto table = std::make_shared<router>();
    party_page_routes::register_routes(table, std::make_shared<endpoint_registry>(), session);
    const auto& route = table->routes().front();
    REQUIRE(route.method == http_method::get);
    REQUIRE(route.pattern == "/api/parties");
    REQUIRE(route.requires_auth);
    boost::asio::io_context io;
    std::optional<http_response> answer;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> { answer = co_await route.handler(req); },
        boost::asio::detached);
    io.run();
    REQUIRE(answer.has_value());
    return *answer;
}

ores::refdata::domain::party party_of(std::string id,
                                      std::string code,
                                      std::string name,
                                      std::optional<std::string> parent,
                                      std::string centre) {
    boost::uuids::string_generator uuid;
    ores::refdata::domain::party p;
    p.id = uuid(id);
    p.short_code = std::move(code);
    p.full_name = std::move(name);
    p.party_type = "Corporate";
    p.status = "Active";
    if (parent)
        p.parent_party_id = uuid(*parent);
    p.business_center_code = std::move(centre);
    return p;
}

const std::string tenant = "11111111-1111-1111-1111-111111111111";
const std::string root = "00000000-0000-0000-0000-00000000000a";
const std::string child = "00000000-0000-0000-0000-00000000000b";
const std::string orphan = "00000000-0000-0000-0000-00000000000c";
const std::string absent = "00000000-0000-0000-0000-00000000000d";

}

TEST_CASE("party_page_routes_read_the_page_within_the_bff_bounds", "[party_page]") {
    const auto plain = party_page_routes::read_page_query(page_request({}, tenant));
    REQUIRE(plain);
    CHECK(plain->offset == 0);
    CHECK(plain->limit == 20);
    CHECK_FALSE(plain->descending);
    const auto asked = party_page_routes::read_page_query(page_request({{"offset", "40"},
                                                                        {"limit", "1000"},
                                                                        {"search", "acme"},
                                                                        {"sort", "short_code"},
                                                                        {"descending", "true"}},
                                                                       tenant));
    REQUIRE(asked);
    CHECK(asked->offset == 40);
    CHECK(asked->limit == 1000);
    CHECK(asked->search == "acme");
    CHECK(asked->sort == "short_code");
    CHECK(asked->descending);
    for (const auto& [name, value] : std::map<std::string, std::string>{
             {"limit", "0"}, {"limit", "1001"}, {"offset", "-1"}, {"offset", "x"}}) {
        INFO(name << "=" << value);
        CHECK_FALSE(party_page_routes::read_page_query(page_request({{name, value}}, tenant)));
    }
    CHECK_FALSE(party_page_routes::read_page_query(
        page_request({{"search", std::string(201, 'a')}}, tenant)));
    CHECK_FALSE(party_page_routes::read_page_query(
        page_request({{"sort", std::string(101, 'a')}}, tenant)));
}

TEST_CASE("party_page_routes_refuse_before_forwarding", "[party_page]") {
    nats_client session;
    const auto system = answer_of(
        session, page_request({}, std::string(ores::http::net::record_routes::system_tenant_id)));
    CHECK(system.status == http_status::forbidden);
    CHECK(system.body.find("read from inside the tenant") != std::string::npos);
    const auto malformed = answer_of(session, page_request({{"limit", "0"}}, tenant));
    CHECK(malformed.status == http_status::bad_request);
    CHECK(malformed.body.find("whole numbers") != std::string::npos);
    // The client is not connected, so an accepted page fails at the forward step.
    CHECK(answer_of(session, page_request({}, tenant)).status == http_status::bad_gateway);
}

TEST_CASE("party_page_routes_answer_the_page_in_the_screen_shape", "[party_page]") {
    CHECK(party_page_routes::page_json({}, 0, {}, {}) == R"({"parties":[],"totalCount":0})");
    const std::vector parties{party_of(root, "ACME", "Acme \"Group\"", std::nullopt, "GBLO"),
                              party_of(child, "ACUK", "Acme UK", root, "USNY"),
                              party_of(orphan, "ORPH", "Orphan", absent, "")};
    const std::map<std::string, std::string> names{{root, "Acme \"Group\""}, {child, "Acme UK"}};
    const std::map<std::string, std::string> flags{{"GBLO", "flag-gb"}};
    CHECK(
        party_page_routes::page_json(parties, 7, names, flags) ==
        R"({"parties":[)"
        R"({"id":")" +
            root +
            R"(","code":"ACME","name":"Acme \"Group\"","category":"Operational",)"
            R"("type":"Corporate","status":"Active","parentId":null,"parentName":null,)"
            R"("businessCentreCode":"GBLO","flagImageId":"flag-gb"},)"
            R"({"id":")" +
            child +
            R"(","code":"ACUK","name":"Acme UK","category":"Operational",)"
            R"("type":"Corporate","status":"Active","parentId":")" +
            root +
            R"(","parentName":"Acme \"Group\"",)"
            R"("businessCentreCode":"USNY","flagImageId":null},)"
            R"({"id":")" +
            orphan +
            R"(","code":"ORPH","name":"Orphan","category":"Operational",)"
            R"("type":"Corporate","status":"Active","parentId":")" +
            absent +
            R"(","parentName":null,"businessCentreCode":"","flagImageId":null}],"totalCount":7})");
}
