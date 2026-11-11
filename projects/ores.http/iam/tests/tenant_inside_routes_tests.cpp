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
#include "ores.http/routes/iam/tenant_inside_routes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
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
using ores::http::routes::iam::tenant_inside_routes;
using ores::nats::service::nats_client;

namespace {

http_response inside_answer(const std::string& pattern,
                            std::map<std::string, std::string> query,
                            std::map<std::string, std::string> path) {
    http_request req;
    for (auto& [k, v] : query)
        req.query_params[k] = v;
    for (auto& [k, v] : path)
        req.path_params[k] = v;
    return ores::http::iam::tests::route_answer<tenant_inside_routes>(
        http_method::get, pattern, req);
}

const std::string image_id = "00000000-0000-0000-0000-0000000000a1";

}

TEST_CASE("tenant_inside_routes_carry_the_tenant_token_and_nothing_else", "[tenant_inside]") {
    http_request req;
    req.headers["authorization"] = "Bearer caller";
    req.headers["cookie"] = "ores_session=s";
    req.path_params["code"] = "acme";
    const auto inside = tenant_inside_routes::as_token(req, "tenant");
    CHECK(inside.get_bearer_token() == "tenant");
    CHECK(inside.get_header("cookie") == "ores_session=s");
    CHECK(inside.get_path_param("code") == "acme");
    CHECK(req.get_bearer_token() == "caller");
}

TEST_CASE("tenant_inside_routes_refuse_a_malformed_page_before_entering", "[tenant_inside]") {
    const std::map<std::string, std::string> path{{"code", "acme"}, {"username", "u"}};
    for (const auto& [pattern, query] :
         std::vector<std::pair<std::string, std::map<std::string, std::string>>>{
             {"/api/tenants/{code}/parties", {{"limit", "0"}}},
             {"/api/tenants/{code}/parties", {{"limit", "1001"}}},
             {"/api/tenants/{code}/people", {{"limit", "0"}}},
             {"/api/tenants/{code}/people", {{"offset", "x"}}},
             {"/api/tenants/{code}/accounts/{username}/sign-ins", {{"limit", "201"}}}}) {
        INFO(pattern);
        const auto answer = inside_answer(pattern, query, path);
        CHECK(answer.status == http_status::bad_request);
        CHECK(answer.body.find("whole numbers") != std::string::npos);
    }
    const auto image = inside_answer(
        "/api/tenants/{code}/images/{id}", {}, {{"code", "acme"}, {"id", "not-an-id"}});
    CHECK(image.status == http_status::not_found);
    CHECK(image.body.find("No image has this identifier.") != std::string::npos);
}

TEST_CASE("tenant_inside_routes_forward_an_accepted_read", "[tenant_inside]") {
    // The client is not connected, so the deployment read fails at the forward step.
    const std::map<std::string, std::string> path{
        {"code", "acme"}, {"username", "u"}, {"id", image_id}};
    for (const auto* pattern : {"/api/tenants/{code}/parties",
                                "/api/tenants/{code}/people",
                                "/api/tenants/{code}/accounts/{username}/sign-ins",
                                "/api/tenants/{code}/images/{id}"}) {
        INFO(pattern);
        CHECK(inside_answer(pattern, {}, path).status == http_status::bad_gateway);
    }
}
