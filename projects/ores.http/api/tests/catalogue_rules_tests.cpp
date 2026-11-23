/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.http.api/domain/catalogue_rules.hpp"
#include "ores.http.api/net/router.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>

using namespace ores::http::domain;

TEST_CASE("an_entity_route_takes_its_area_from_its_path_or_its_component", "[catalogue]") {
    const auto listed = entity_route_catalogue(
        http_method::get, "/api/v1/reference-data/currencies", "refdata", "currencies", "list");
    CHECK(listed.area == "reference-data");
    CHECK(listed.entity == "currencies");
    CHECK(listed.tool == "list_currencies");
    CHECK_FALSE(listed.writes);

    const auto screen = entity_route_catalogue(
        http_method::put, "/api/synthetic/folders", "synthetic", "folder", "screen_put");
    CHECK(screen.area == "simulation");
    CHECK(screen.writes);
}

TEST_CASE("a_bulk_get_is_a_post_that_only_reads", "[catalogue]") {
    CHECK_FALSE(entity_route_catalogue(http_method::post,
                                       "/api/v1/reference-data/currencies/get-many",
                                       "refdata",
                                       "currencies",
                                       "get_many")
                    .writes);
    CHECK(entity_route_catalogue(http_method::post,
                                 "/api/v1/reference-data/currencies/put-many",
                                 "refdata",
                                 "currencies",
                                 "put_many")
              .writes);
}

TEST_CASE("an_operation_writes_unless_its_name_only_reads", "[catalogue]") {
    CHECK_FALSE(
        operation_route_catalogue("iam", "agent_grant_operations", "list_own_agent_grants").writes);
    CHECK(operation_route_catalogue("iam", "agent_grant_operations", "create_agent_grant").writes);
    CHECK(operation_route_catalogue("refdata", "party_operations", "get_or_create_party").writes);
    CHECK(operation_route_catalogue("iam", "agent_grant_operations", "x").area == "access");
}

TEST_CASE("the_router_names_every_undeclared_route_and_every_shared_tool", "[catalogue]") {
    ores::http::net::router r;
    const auto handler = [](const http_request&) -> boost::asio::awaitable<http_response> {
        co_return http_response::json("{}");
    };
    r.add_route(r.get("/a").auth_optional().handler(handler).build());
    r.add_route(
        r.get("/b").catalogue("one", "qa", "b", false).auth_optional().handler(handler).build());
    r.add_route(
        r.get("/c").catalogue("one", "qa", "c", false).auth_optional().handler(handler).build());

    const auto undeclared = r.uncatalogued();
    REQUIRE(undeclared.size() == 2);
    CHECK(undeclared[0] == "GET /a");
    CHECK(undeclared[1].find("shares the tool one") != std::string::npos);
}

TEST_CASE("every_generated_method_and_name_reads_or_writes_as_stated", "[catalogue]") {
    CHECK_FALSE(entity_route_catalogue(http_method::head,
                                       "/api/v1/reference-data/currencies/{iso_code}",
                                       "refdata",
                                       "currencies",
                                       "head")
                    .writes);
    CHECK(entity_route_catalogue(http_method::delete_,
                                 "/api/v1/reference-data/currencies/{iso_code}",
                                 "refdata",
                                 "currencies",
                                 "delete")
              .writes);
    CHECK(operation_only_reads("search_trades"));
    CHECK(operation_only_reads("lookup_country"));
    CHECK_FALSE(operation_only_reads("find_or_create_book"));
}

TEST_CASE("a_component_the_table_does_not_name_is_not_a_published_area", "[catalogue]") {
    CHECK(area_of_component("nowhere") == "nowhere");
    CHECK_FALSE(is_published_area("nowhere"));
    CHECK(is_published_area("reference-data"));
}

TEST_CASE("the_router_refuses_a_write_declared_out_of_the_hold", "[catalogue]") {
    ores::http::net::router r;
    const auto handler = [](const http_request&) -> boost::asio::awaitable<http_response> {
        co_return http_response::json("{}");
    };
    r.add_route(r.post("/mark")
                    .catalogue("web_post_mark", "workflow", "marks", false)
                    .auth_optional()
                    .handler(handler)
                    .build());
    r.add_route(r.post("/bulk")
                    .catalogue("get_many_marks", "workflow", "marks", false)
                    .auth_optional()
                    .handler(handler)
                    .build());
    r.add_route(r.get("/no-entity")
                    .catalogue("tool_only", "workflow", "", false)
                    .auth_optional()
                    .handler(handler)
                    .build());
    r.add_route(r.get("/stray")
                    .catalogue("stray", "nowhere", "strays", false)
                    .auth_optional()
                    .handler(handler)
                    .build());

    const auto refused = r.uncatalogued();
    REQUIRE(refused.size() == 3);
    CHECK(refused[0].starts_with("POST /mark declares that it does not write"));
    CHECK(refused[1] == "GET /no-entity");
    CHECK(refused[2].find("not a published one") != std::string::npos);
}
