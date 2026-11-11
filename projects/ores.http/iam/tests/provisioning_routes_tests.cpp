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
#include "ores.http/routes/iam/provisioning_routes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::net::router;
using ores::http::openapi::endpoint_registry;
using ores::http::routes::iam::provisioning_routes;
using ores::nats::service::nats_client;

namespace {

http_response
provisioning_answer(http_method method, const std::string& pattern, std::string body = {}) {
    http_request req;
    req.body = std::move(body);
    return ores::http::iam::tests::route_answer<provisioning_routes>(method, pattern, req);
}

provisioning_routes::seed_profile_reads profile_of(std::string code, int order) {
    provisioning_routes::seed_profile_reads read;
    read.profile.code = std::move(code);
    read.profile.name = "Name";
    read.profile.display_order = order;
    return read;
}

ores::refdata::domain::party party_of(std::string id, std::string category, bool has_parent) {
    boost::uuids::string_generator uuid;
    ores::refdata::domain::party p;
    p.id = uuid(id);
    p.party_category = std::move(category);
    if (has_parent)
        p.parent_party_id = uuid("00000000-0000-0000-0000-0000000000ff");
    return p;
}

const std::string full_tenant =
    R"({"profileCode":"eval","tenantCode":"acme","tenantName":"Acme","tenantHostname":"acme.io",)"
    R"("adminUsername":"admin","adminEmail":"a@acme.io","adminPassword":"pw")";

}

TEST_CASE("provisioning_routes_answer_a_starting_point_in_the_card_shape", "[provisioning]") {
    CHECK(provisioning_routes::profiles_json({}) == R"({"profiles":[]})");
    auto read = profile_of("eval", 2);
    read.profile.summary = "A \"trial\"";
    read.profile.bullets_json = R"(["One","Two"])";
    read.profile.tenant_code = "acme";
    read.profile.inherits_admin_password = true;
    ores::iam::domain::seed_profile_step step;
    step.step_kind = "publish";
    step.display_order = 1;
    read.steps.push_back(step);
    ores::iam::domain::seed_profile_parameter parameter;
    parameter.name = "region";
    parameter.label = "Region";
    parameter.data_type = "choice";
    parameter.choices_json = R"(["EU","US"])";
    parameter.is_required = true;
    parameter.display_order = 3;
    read.parameters.push_back(parameter);
    CHECK(provisioning_routes::profiles_json({read}) ==
          R"({"profiles":[{"code":"eval","name":"Name","summary":"A \"trial\"","audience":"",)"
          R"("bullets":["One","Two"],"tenant":{"name":"","code":"acme","hostname":"",)"
          R"("adminUsername":"","adminEmail":""},"inheritsAdminPassword":true,)"
          R"("forcePasswordChange":false,"order":2,"steps":[{"kind":"publish","order":1}],)"
          R"("parameters":[{"name":"region","label":"Region","dataType":"choice",)"
          R"("choices":["EU","US"],"defaultValue":"","required":true,"hint":"","order":3}]}]})");
}

TEST_CASE("provisioning_routes_order_the_cards_by_their_order_then_code", "[provisioning]") {
    const auto json = provisioning_routes::profiles_json(
        {profile_of("c", 2), profile_of("b", 1), profile_of("a", 2)});
    CHECK(json.find(R"("code":"b")") < json.find(R"("code":"a")"));
    CHECK(json.find(R"("code":"a")") < json.find(R"("code":"c")"));
}

TEST_CASE("provisioning_routes_read_a_json_list_column", "[provisioning]") {
    CHECK(provisioning_routes::texts_of("") == std::vector<std::string>{});
    CHECK(provisioning_routes::texts_of(R"(["a","b"])") == std::vector<std::string>{"a", "b"});
    CHECK_FALSE(provisioning_routes::texts_of("not json"));
    CHECK_FALSE(provisioning_routes::texts_of("[1]"));
    auto bad = profile_of("bad", 0);
    bad.profile.bullets_json = "{}";
    CHECK_THROWS_AS(provisioning_routes::profiles_json({bad}), std::runtime_error);
}

TEST_CASE("provisioning_routes_read_the_tenant_command_as_the_bff_did", "[provisioning]") {
    const auto plain = provisioning_routes::read_tenant_command(full_tenant + "}");
    REQUIRE(plain);
    CHECK(plain->profile_code == "eval");
    CHECK(plain->tenant_description.empty());
    CHECK(plain->parameters.empty());
    const auto with = provisioning_routes::read_tenant_command(
        full_tenant + R"(,"tenantDescription":"d","parameters":{"region":"EU","size":"s"}})");
    REQUIRE(with);
    CHECK(with->tenant_description == "d");
    CHECK(with->parameters == std::vector<std::string>{"region=EU", "size=s"});
    CHECK_FALSE(provisioning_routes::read_tenant_command("{}"));
    CHECK_FALSE(provisioning_routes::read_tenant_command("not json"));
    auto empty_code = full_tenant + "}";
    empty_code.replace(empty_code.find("\"eval\""), 6, "\"\"");
    CHECK_FALSE(provisioning_routes::read_tenant_command(empty_code));
}

TEST_CASE("provisioning_routes_place_a_party_under_the_tenant_root", "[provisioning]") {
    const std::string system = "00000000-0000-0000-0000-000000000001";
    const std::string child = "00000000-0000-0000-0000-000000000002";
    const std::string root = "00000000-0000-0000-0000-000000000003";
    CHECK_FALSE(provisioning_routes::root_of({}));
    CHECK_FALSE(provisioning_routes::root_of({party_of(system, "System", false)}));
    const auto found = provisioning_routes::root_of({party_of(system, "System", false),
                                                     party_of(child, "Operational", true),
                                                     party_of(root, "Operational", false)});
    REQUIRE(found);
    CHECK(*found == boost::uuids::string_generator()(root));
}

TEST_CASE("provisioning_routes_refuse_before_forwarding", "[provisioning]") {
    const auto tenant = provisioning_answer(http_method::post, "/api/provision-tenant", "{}");
    CHECK(tenant.status == http_status::bad_request);
    CHECK(tenant.body.find("administrator fields are required") != std::string::npos);
    const auto party = provisioning_answer(
        http_method::post, "/api/provision-party", R"({"fullName":"","shortCode":"X"})");
    CHECK(party.status == http_status::bad_request);
    CHECK(party.body.find("legal name and a short code") != std::string::npos);
    // The client is not connected, so an accepted request fails at the forward step.
    CHECK(provisioning_answer(http_method::get, "/api/seed-profiles").status ==
          http_status::bad_gateway);
    CHECK(
        provisioning_answer(http_method::post, "/api/provision-tenant", full_tenant + "}").status ==
        http_status::bad_gateway);
    CHECK(provisioning_answer(
              http_method::post, "/api/provision-party", R"({"fullName":"A","shortCode":"A"})")
              .status == http_status::bad_gateway);
}
