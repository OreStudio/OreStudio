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
#include "ores.http.api/mcp/catalogue_tools.hpp"
#include "ores.http.api/net/router.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/use_future.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>

namespace {

using Catch::Matchers::ContainsSubstring;
using namespace ores::http::mcp;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::utility::codec::json_document;

struct currency_body final {
    std::string code;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<currency_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"code", &currency_body::code}};
}

/// Answers what it was asked, so a test sees how the tool bound its arguments.
boost::asio::awaitable<http_response> echo(const http_request& req) {
    auto doc = json_document::make_object();
    auto root = doc.root();
    root.set("target", req.target);
    root.set("code", req.get_path_param("code"));
    root.set("body", req.body);
    root.set("authorization", req.get_header("authorization"));
    co_return http_response::json(doc.write());
}

std::shared_ptr<const operation_catalogue> make_catalogue() {
    ores::http::net::router r;
    r.add_route(r.get("/api/v1/reference-data/currencies")
                    .catalogue("list_currencies", "reference-data", "currencies", false)
                    .summary("List the currencies")
                    .query_param("limit", "integer", "", false, "How many")
                    .auth_required()
                    .handler(echo)
                    .build());
    r.add_route(r.get("/api/v1/reference-data/currencies/{code}")
                    .catalogue("get_currency", "reference-data", "currencies", false)
                    .summary("Read one currency by its ISO code")
                    .auth_required()
                    .handler(echo)
                    .build());
    r.add_route(r.post("/api/v1/reference-data/currencies")
                    .catalogue("put_currency", "reference-data", "currencies", true)
                    .summary("Write a currency")
                    .body<currency_body>()
                    .auth_required()
                    .handler(echo)
                    .build());
    r.add_route(r.get("/api/v1/reference-data/currency_currency_groups")
                    .catalogue("list_currency_currency_groups",
                               "reference-data",
                               "currency_currency_groups",
                               false)
                    .summary("List currency currency groups")
                    .auth_required()
                    .handler(echo)
                    .build());
    r.add_route(r.get("/api/v1/reference-data/greeting")
                    .catalogue("get_greeting", "reference-data", "greetings", false)
                    .summary("Greet in a word with accents")
                    .auth_required()
                    .handler([](const http_request&) -> boost::asio::awaitable<http_response> {
                        co_return http_response::json("{\"word\":\"\u00e9\u00e9\u00e9\"}");
                    })
                    .build());
    r.add_route(r.get("/api/v1/trading/trades")
                    .catalogue("list_trades", "trading", "trades", false)
                    .summary("List the trades of a book")
                    .auth_required()
                    .handler(echo)
                    .build());
    return std::make_shared<const operation_catalogue>(r.routes());
}

http_request caller(std::vector<std::string> permissions) {
    http_request req;
    ores::security::jwt::jwt_claims claims;
    claims.subject = "person-1";
    claims.act = "agent:grant-1";
    claims.roles = std::move(permissions);
    req.authenticated_user = claims;
    req.headers["authorization"] = "Bearer exchanged";
    return req;
}

tool_result run(const catalogue_tools& tools,
                const http_request& req,
                std::string_view name,
                std::string_view arguments) {
    const auto doc = json_document::parse(arguments);
    REQUIRE(doc.has_value());
    boost::asio::io_context io;
    auto answer =
        boost::asio::co_spawn(io, tools.call(req, name, doc->root()), boost::asio::use_future);
    io.run();
    auto result = answer.get();
    REQUIRE(result.has_value());
    return *result;
}

const std::vector<std::string> reader{"refdata::currencies:read"};
const std::vector<std::string> writer{"refdata::currencies:read", "refdata::currencies:write"};

}

TEST_CASE("the_five_tools_are_the_same_whatever_the_catalogue_holds", "[mcp]") {
    const catalogue_tools empty(
        std::make_shared<const operation_catalogue>(std::vector<ores::http::domain::route>{}), {});
    const catalogue_tools full(make_catalogue(), {});
    std::vector<std::string> names;
    for (const auto& t : full.list(caller(reader)))
        names.push_back(t.name);
    CHECK(
        names ==
        std::vector<std::string>{
            "list_areas", "list_entities", "list_operations", "find_operations", "call_operation"});
    CHECK(empty.list(caller(reader)).size() == 5);
}

TEST_CASE("a_grant_that_only_reads_sees_no_write", "[mcp]") {
    const catalogue_tools tools(make_catalogue(), {});
    const auto read_only = run(tools,
                               caller(reader),
                               "list_operations",
                               R"({"area":"reference-data","entity":"currencies"})");
    CHECK_THAT(read_only.text, ContainsSubstring("list_currencies"));
    CHECK_THAT(read_only.text, !ContainsSubstring("put_currency"));
    const auto writing = run(tools,
                             caller(writer),
                             "list_operations",
                             R"({"area":"reference-data","entity":"currencies"})");
    CHECK_THAT(writing.text, ContainsSubstring("put_currency [writes]"));

    const auto call = run(tools,
                          caller(reader),
                          "call_operation",
                          R"({"name":"put_currency","arguments":{"body":{"code":"EUR"}}})");
    CHECK(call.is_error);
}

TEST_CASE("the_areas_and_entities_are_listed_with_their_counts", "[mcp]") {
    const catalogue_tools tools(make_catalogue(), {});
    const auto areas = run(tools, caller(writer), "list_areas", "{}");
    CHECK_THAT(areas.text, ContainsSubstring("reference-data: "));
    CHECK_THAT(areas.text, ContainsSubstring("(5)"));
    CHECK_THAT(areas.text, !ContainsSubstring("access"));
    const auto entities =
        run(tools, caller(reader), "list_entities", R"({"area":"reference-data"})");
    CHECK_THAT(entities.text, ContainsSubstring("currencies (2)"));
    CHECK(run(tools, caller(reader), "list_entities", R"({"area":"nowhere"})").is_error);
}

TEST_CASE("a_long_list_is_answered_a_page_at_a_time", "[mcp]") {
    const catalogue_tools tools(make_catalogue(), {.page_size = 1});
    const auto first = run(tools,
                           caller(writer),
                           "list_operations",
                           R"({"area":"reference-data","entity":"currencies"})");
    CHECK_THAT(first.text, ContainsSubstring("page 1 of 3"));
    const auto last = run(tools,
                          caller(writer),
                          "list_operations",
                          R"({"area":"reference-data","entity":"currencies","page":3})");
    CHECK_THAT(last.text, ContainsSubstring("page 3 of 3"));
    CHECK_THAT(last.text, ContainsSubstring("put_currency"));
}

TEST_CASE("find_operations_ranks_the_best_match_first", "[mcp]") {
    const catalogue_tools tools(make_catalogue(), {});
    const auto found =
        run(tools, caller(reader), "find_operations", R"({"words":"trades of a book"})");
    CHECK(found.text.find("list_trades") < found.text.find("currenc"));
    const auto one =
        run(tools, caller(reader), "find_operations", R"({"words":"read currency ISO code"})");
    CHECK(one.text.find("get_currency") < one.text.find("list_currencies"));
    CHECK(run(tools, caller(reader), "find_operations", R"({"words":"   "})").is_error);
    const auto named =
        run(tools, caller(reader), "find_operations", R"({"words":"list currencies"})");
    CHECK(named.text.find("list_currencies") < named.text.find("list_currency_currency_groups"));
}

TEST_CASE("call_operation_binds_its_arguments_to_the_route", "[mcp]") {
    const catalogue_tools tools(make_catalogue(), {});
    const auto read = run(tools,
                          caller(reader),
                          "call_operation",
                          R"({"name":"get_currency","arguments":{"code":"EUR"}})");
    REQUIRE_FALSE(read.is_error);
    CHECK(read.text.starts_with("HTTP 200"));
    CHECK_THAT(read.text, ContainsSubstring(R"("code":"EUR")"));
    CHECK_THAT(read.text, ContainsSubstring(R"("authorization":"Bearer exchanged")"));
    const auto listed = run(tools,
                            caller(reader),
                            "call_operation",
                            R"({"name":"list_currencies","arguments":{"limit":5}})");
    CHECK_THAT(listed.text, ContainsSubstring("/api/v1/reference-data/currencies?limit=5"));
}

TEST_CASE("call_operation_without_its_arguments_answers_their_schema_and_an_example", "[mcp]") {
    const catalogue_tools tools(make_catalogue(), {});
    const auto bare = run(tools, caller(reader), "call_operation", R"({"name":"get_currency"})");
    CHECK_THAT(bare.text, ContainsSubstring(R"("required":["code"])"));
    CHECK_THAT(bare.text, ContainsSubstring(R"(For example: {"code":"text"})"));
    const auto bodiless = run(tools,
                              caller(writer),
                              "call_operation",
                              R"({"name":"put_currency","arguments":{"other":1}})");
    CHECK(bodiless.is_error);
    CHECK_THAT(bodiless.text, ContainsSubstring(R"("body":{"code":"text"})"));
}

TEST_CASE("call_operation_offers_a_write_to_the_hold_and_records_each_call", "[mcp]") {
    std::vector<std::string> recorded;
    std::string held_route;
    catalogue_tools_options options;
    options.hold = [&](const http_request& req,
                       const std::string& route) -> std::optional<http_response> {
        held_route = route + " " + req.body;
        return http_response::json(R"({"requestId":"r-1"})", http_status::accepted);
    };
    options.record = [&](const ores::http::domain::exchanged_call& call) {
        recorded.push_back(call.method + " " + call.route + " " + std::to_string(call.status));
    };
    const catalogue_tools tools(make_catalogue(), options);

    const auto held = run(tools,
                          caller(writer),
                          "call_operation",
                          R"({"name":"put_currency","arguments":{"body":{"code":"EUR"}}})");
    CHECK_FALSE(held.is_error);
    CHECK_THAT(held.text, ContainsSubstring("waits for your person's approval"));
    CHECK_THAT(held.text, ContainsSubstring("r-1"));
    CHECK(held_route == R"(/api/v1/reference-data/currencies {"code":"EUR"})");

    run(tools,
        caller(reader),
        "call_operation",
        R"({"name":"get_currency","arguments":{"code":"EUR"}})");
    run(tools,
        caller(reader),
        "call_operation",
        R"({"name":"put_currency","arguments":{"body":{"code":"EUR"}}})");
    CHECK(recorded == std::vector<std::string>{"POST /api/v1/reference-data/currencies 202",
                                               "GET /api/v1/reference-data/currencies/{code} 200",
                                               "POST /api/v1/reference-data/currencies 403"});
}

TEST_CASE("a_long_answer_is_cut_to_what_a_model_can_read", "[mcp]") {
    const catalogue_tools tools(make_catalogue(), {.answer_limit = 20});
    const auto cut = run(tools,
                         caller(reader),
                         "call_operation",
                         R"({"name":"get_currency","arguments":{"code":"EUR"}})");
    CHECK_THAT(cut.text, ContainsSubstring("[cut: "));
}

TEST_CASE("an_input_schema_states_its_definitions_where_its_references_point", "[mcp]") {
    const auto catalogue = make_catalogue();
    const auto* op = catalogue->find("put_currency");
    REQUIRE(op != nullptr);
    const auto schema = json_document::parse(input_schema_of(*op));
    REQUIRE(schema.has_value());
    const auto root = schema->root();
    const auto body = root.member("properties").member("body");
    CHECK_FALSE(body.has("$defs"));
    const auto ref = body.member("$ref").text_or();
    REQUIRE(ref.starts_with("#/$defs/"));
    CHECK(root.member("$defs").member(ref.substr(8)).is_object());
}

TEST_CASE("a_cut_never_splits_a_character", "[mcp]") {
    const catalogue_tools tools(make_catalogue(), {.answer_limit = 10});
    const auto cut =
        run(tools, caller(reader), "call_operation", R"({"name":"get_greeting","arguments":{}})");
    CHECK_THAT(cut.text, ContainsSubstring("{\"word\":\"\n[cut: 8 more"));
}

TEST_CASE("call_operation_refuses_a_caller_without_a_token_and_an_unreadable_argument", "[mcp]") {
    const catalogue_tools tools(make_catalogue(), {});
    http_request anonymous;
    CHECK(run(tools, anonymous, "call_operation", R"({"name":"list_currencies","arguments":{}})")
              .is_error);
    const auto listed = run(tools,
                            caller(reader),
                            "call_operation",
                            R"({"name":"list_currencies","arguments":{"limit":[1,2]}})");
    CHECK(listed.is_error);
    CHECK_THAT(listed.text, ContainsSubstring("limit is missing, or is not text"));
}
