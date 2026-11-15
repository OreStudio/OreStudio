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
#include "ores.http/routes/iam/deployment_access.hpp"
#include "ores.http/routes/iam/tenant_roster_routes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.utility/codec/json_codec.hpp"
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
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::net::router;
using ores::http::openapi::endpoint_registry;
using ores::http::routes::iam::deployment_access;
using ores::http::routes::iam::tenant_roster_routes;
using ores::nats::service::nats_client;

namespace {

const std::string acme_id = "00000000-0000-0000-0000-0000000000a1";

ores::iam::domain::tenant acme() {
    ores::iam::domain::tenant t;
    t.id = boost::uuids::string_generator()(acme_id);
    t.code = "acme";
    t.name = "Acme \"Corp\"";
    t.type = "production";
    t.hostname = "acme.io";
    t.status = "active";
    t.version = 3;
    t.modified_by = "admin";
    return t;
}

const std::string acme_fields =
    R"("id":")" + acme_id +
    R"(","code":"acme","name":"Acme \"Corp\"","type":"production",)"
    R"("description":"","hostname":"acme.io","status":"active","registrationDefault":false)";

const tenant_roster_routes::tenant_setup running{"run-1", "in_progress", 2, 5, ""};
const std::string running_json =
    R"({"instanceId":"run-1","status":"in_progress","stepsDone":2,"stepCount":5,"error":""})";

http_request query_of(std::map<std::string, std::string> query) {
    http_request req;
    for (auto& [k, v] : query)
        req.query_params[k] = v;
    return req;
}

}

TEST_CASE("deployment_access_covers_only_the_installation_grants", "[tenant_roster]") {
    using v = std::vector<std::string>;
    CHECK(deployment_access::covers(v{"installation::deployment:read"}));
    CHECK(deployment_access::covers(v{"installation::*"}));
    CHECK_FALSE(deployment_access::covers(v{"*"}));
    CHECK_FALSE(deployment_access::covers(v{"iam::*", "iam::tenants:read"}));
    CHECK_FALSE(deployment_access::covers(v{}));
}

TEST_CASE("tenant_roster_routes_read_the_page_within_the_bff_bounds", "[tenant_roster]") {
    const auto plain = tenant_roster_routes::read_roster_query(query_of({}));
    REQUIRE(plain);
    CHECK(plain->offset == 0);
    CHECK(plain->limit == 100);
    CHECK_FALSE(plain->include_test);
    const auto asked = tenant_roster_routes::read_roster_query(query_of({{"includeTest", "true"},
                                                                         {"descending", "true"},
                                                                         {"limit", "1000"},
                                                                         {"type", "evaluation"}}));
    REQUIRE(asked);
    CHECK(asked->include_test);
    CHECK(asked->descending);
    CHECK(asked->limit == 1000);
    CHECK(asked->type == "evaluation");
    for (const auto& [name, value] :
         std::map<std::string, std::string>{{"limit", "0"},
                                            {"limit", "1001"},
                                            {"offset", "x"},
                                            {"type", std::string(101, 't')},
                                            {"search", std::string(201, 's')}}) {
        INFO(name);
        CHECK_FALSE(tenant_roster_routes::read_roster_query(query_of({{name, value}})));
    }
}

TEST_CASE("tenant_roster_routes_answer_the_roster_in_the_screen_shape", "[tenant_roster]") {
    CHECK(tenant_roster_routes::summary_json(acme(), std::nullopt) ==
          "{" + acme_fields + R"(,"setup":null})");
    CHECK(tenant_roster_routes::page_json({}, {}, 0, false, 0) ==
          R"({"tenants":[],"totalCount":0,"setupUnavailable":false,"hiddenTestCount":0})");
    CHECK(tenant_roster_routes::page_json({acme()}, {{acme_id, running}}, 9, true, 2) ==
          R"({"tenants":[{)" + acme_fields + R"(,"setup":)" + running_json +
              R"(}],"totalCount":9,"setupUnavailable":true,"hiddenTestCount":2})");
}

TEST_CASE("tenant_roster_routes_answer_one_tenant_with_its_provenance", "[tenant_roster]") {
    const auto t = acme();
    CHECK(tenant_roster_routes::detail_json(t, running, false) ==
          R"({"tenant":{)" + acme_fields + R"(,"setup":)" + running_json +
              R"(,"version":3,"modifiedBy":"admin","performedBy":"","changeReasonCode":"",)"
              R"("changeCommentary":"","recordedAt":)" +
              ores::utility::codec::json::write(t.recorded_at) + R"(},"setupUnavailable":false})");
}

TEST_CASE("tenant_roster_routes_answer_the_overview_in_the_home_shape", "[tenant_roster]") {
    tenant_roster_routes::overview empty;
    CHECK(tenant_roster_routes::overview_json(empty) ==
          R"({"inService":0,"onEvaluation":0,"settingUp":0,"attention":[],"tenants":[],)"
          R"("totalCount":0,"activity":[],"activityUnavailable":false})");
    tenant_roster_routes::overview o;
    o.in_service = 4;
    o.on_evaluation = 1;
    o.setting_up = 2;
    o.attention.push_back({acme(), running, "setup-failed"});
    o.attention.push_back({acme(), std::nullopt, "suspended"});
    o.tenants = {acme()};
    o.total_count = 7;
    o.activity.push_back({"run-1", "Acme", "completed", 5, 5, "", "2026-11-11T07:00:00Z"});
    o.activity_unavailable = true;
    CHECK(tenant_roster_routes::overview_json(o) ==
          R"({"inService":4,"onEvaluation":1,"settingUp":2,"attention":[)"
          R"({"tenant":{)" +
              acme_fields + R"(,"setup":)" + running_json +
              R"(},"reason":"setup-failed"},)"
              R"({"tenant":{)" +
              acme_fields +
              R"(,"setup":null},"reason":"suspended"}],)"
              R"("tenants":[{)" +
              acme_fields +
              R"(,"setup":null}],"totalCount":7,)"
              R"("activity":[{"instanceId":"run-1","tenantName":"Acme","status":"completed",)"
              R"("stepsDone":5,"stepCount":5,"error":"","at":"2026-11-11T07:00:00Z"}],)"
              R"("activityUnavailable":true})");
}

TEST_CASE("tenant_roster_routes_check_the_deployment_read_before_anything_else",
          "[tenant_roster]") {
    nats_client session;
    auto table = std::make_shared<router>();
    tenant_roster_routes::register_routes(table, std::make_shared<endpoint_registry>(), session);
    const std::vector<std::pair<http_method, std::string>> expected{
        {http_method::get, "/api/tenants"},
        {http_method::get, "/api/overview"},
        {http_method::get, "/api/tenants/{code}"},
        {http_method::delete_, "/api/tenants/{code}"}};
    for (const auto& [method, pattern] : expected) {
        INFO(pattern);
        const auto route = std::ranges::find_if(table->routes(), [&](const auto& r) {
            return r.method == method && r.pattern == pattern;
        });
        REQUIRE(route != table->routes().end());
        CHECK(route->requires_auth);
        // A malformed page is not refused before the read: the BFF checked the
        // deployment read first, and the client is not connected.
        auto req = query_of({{"limit", "0"}});
        req.path_params["code"] = "acme";
        req.body = R"({"confirmCode":"other"})";
        boost::asio::io_context io;
        std::optional<http_response> answer;
        boost::asio::co_spawn(
            io,
            [&]() -> boost::asio::awaitable<void> { answer = co_await route->handler(req); },
            boost::asio::detached);
        io.run();
        REQUIRE(answer.has_value());
        CHECK(answer->status == http_status::bad_gateway);
    }
}
