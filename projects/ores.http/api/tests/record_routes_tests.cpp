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
#include "ores.http.api/net/record_routes.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <optional>
#include <string>

namespace rr = ores::http::net::record_routes;
using ores::http::domain::http_status;
using ores::utility::domain::outcome;
using ores::utility::domain::result;

namespace {

/// The refusal a call states, or nothing when it states none.
template <typename F>
std::optional<rr::refusal> record_routes_refusal_of(F&& call) {
    try {
        call();
    } catch (const rr::refusal& r) {
        return r;
    }
    return std::nullopt;
}

}

TEST_CASE("record_routes_maps_each_refused_outcome_to_its_status", "[record_routes]") {
    const auto status_of = [](outcome o, std::string message = "words") {
        return record_routes_refusal_of([&] { rr::require_ok(result{o, "", message, {}}); });
    };
    CHECK_FALSE(status_of(outcome::ok).has_value());
    CHECK(status_of(outcome::conflict)->status == http_status::conflict);
    CHECK(status_of(outcome::invalid)->status == http_status::bad_request);
    CHECK(status_of(outcome::denied)->status == http_status::forbidden);
    CHECK(status_of(outcome::missing)->status == http_status::not_found);
    CHECK(status_of(outcome::unavailable)->status == http_status::bad_gateway);
    CHECK(status_of(outcome::failed)->code == "upstream-unavailable");
}

TEST_CASE("record_routes_says_what_a_conflict_means_when_the_service_does_not", "[record_routes]") {
    const auto refused =
        record_routes_refusal_of([] { rr::require_ok(result{outcome::conflict, "", "", {}}); });
    REQUIRE(refused.has_value());
    CHECK(refused->code == "conflict");
    CHECK(refused->message.find("Reload") != std::string::npos);
}

TEST_CASE("record_routes_trims_every_blank_before_it_measures", "[record_routes]") {
    CHECK(rr::trimmed(" \t\r\nabc\r\n ") == "abc");
    CHECK(rr::trimmed(" \n ").empty());
    CHECK(rr::has_reason("  why  "));
    CHECK_FALSE(rr::has_reason(" \r\n "));
    CHECK(rr::has_reason(std::string(200, 'a') + "   "));
    CHECK_FALSE(rr::has_reason(std::string(201, 'a')));
}

TEST_CASE("record_routes_refuses_a_code_that_names_nothing_or_too_much", "[record_routes]") {
    CHECK(rr::code("  USD ", "bad") == "USD");
    const auto empty = record_routes_refusal_of([] { rr::code("   ", "A code is needed."); });
    REQUIRE(empty.has_value());
    CHECK(empty->status == http_status::bad_request);
    CHECK(empty->message == "A code is needed.");
    CHECK(record_routes_refusal_of([] { rr::code(std::string(101, 'a'), "long"); }).has_value());
}

TEST_CASE("record_routes_reads_a_parameter_as_its_type_or_refuses_it_by_name", "[record_routes]") {
    CHECK(rr::read<std::uint32_t>("42", "limit") == 42u);
    CHECK(rr::read<bool>("true", "flag"));
    const auto refused = record_routes_refusal_of([] { rr::read<std::uint32_t>("ten", "limit"); });
    REQUIRE(refused.has_value());
    CHECK(refused->message == "Invalid value for limit: ten");
    CHECK(record_routes_refusal_of([] { rr::read<bool>("yes", "flag"); }).has_value());
}

TEST_CASE("record_routes_answers_a_refusal_with_the_code_and_message_body", "[record_routes]") {
    const auto response =
        rr::answer(rr::refusal{http_status::forbidden, "forbidden", "Read only here."});
    CHECK(response.status == http_status::forbidden);
    CHECK(response.body == R"({"code":"forbidden","message":"Read only here."})");
    CHECK(rr::no_content().status == http_status::no_content);
    CHECK(rr::no_content().body.empty());
}

TEST_CASE("record_routes_refuses_with_the_status_code_and_message_it_is_given", "[record_routes]") {
    const auto refused = record_routes_refusal_of(
        [] { rr::refuse(http_status::forbidden, "forbidden", "Read only here."); });
    REQUIRE(refused.has_value());
    CHECK(refused->status == http_status::forbidden);
    CHECK(refused->code == "forbidden");
    CHECK(refused->message == "Read only here.");

    const auto invalid = record_routes_refusal_of([] { rr::invalid("Bad page."); });
    REQUIRE(invalid.has_value());
    CHECK(invalid->status == http_status::bad_request);
    CHECK(invalid->code == "invalid-request");
    CHECK(invalid->message == "Bad page.");
}

TEST_CASE("record_routes_reads_text_verbatim_and_false_as_false", "[record_routes]") {
    CHECK(rr::read<std::string>(" as given ", "name") == " as given ");
    CHECK_FALSE(rr::read<bool>("false", "flag"));
}

TEST_CASE("record_routes_adds_a_default_only_where_a_write_leaves_the_member_out",
          "[record_routes]") {
    const auto one = rr::with_write_defaults(R"({"write":{"a":"set","c":null},"version":null})",
                                             {{"a", R"("fx")"}, {"b", "1.0"}, {"c", "2"}});
    CHECK(one == R"({"write":{"a":"set","c":null,"b":1.0},"version":null})");

    const auto many =
        rr::with_write_defaults(R"({"changes":[{"write":{}},{"write":{"b":3}}]})", {{"b", "1.0"}});
    CHECK(many == R"({"changes":[{"write":{"b":1.0}},{"write":{"b":3}}]})");

    CHECK(rr::with_write_defaults("not json", {{"b", "1"}}) == "not json");
}

TEST_CASE("record_routes_reads_an_identifier_only_in_its_canonical_form", "[record_routes]") {
    CHECK(rr::is_uuid("00000000-0000-0000-0000-00000000000a"));
    CHECK(rr::is_uuid("ABCDEF01-2345-6789-abcd-ef0123456789"));
    CHECK_FALSE(rr::is_uuid(""));
    CHECK_FALSE(rr::is_uuid("children"));
    CHECK_FALSE(rr::is_uuid("00000000000000000000000000000000"));
    CHECK_FALSE(rr::is_uuid("00000000-0000-0000-0000-00000000000g"));
    CHECK_FALSE(rr::is_uuid(" 00000000-0000-0000-0000-00000000000a"));
}

TEST_CASE("record_routes_refuses_a_query_parameter_the_read_does_not_name", "[record_routes]") {
    ores::http::domain::http_request req;
    req.query_params["limit"] = "5";
    CHECK_FALSE(record_routes_refusal_of([&] { rr::refuse_unknown_params(req, {"limit"}); }));
    req.query_params["colour"] = "red";
    const auto refused =
        record_routes_refusal_of([&] { rr::refuse_unknown_params(req, {"limit"}); });
    REQUIRE(refused);
    CHECK(refused->status == http_status::bad_request);
    CHECK(refused->message == "This read cannot take colour.");
}

TEST_CASE("record_routes_refuses_a_body_member_the_shape_does_not_name", "[record_routes]") {
    const auto refusal_for = [](const std::string& body) {
        return record_routes_refusal_of(
            [&] { rr::refuse_unknown_members(body, {"mode"}, "A switch states a mode."); });
    };
    CHECK_FALSE(refusal_for(R"({"mode":"auto"})"));
    CHECK_FALSE(refusal_for("{}"));
    // A body that is not an object is the read's to refuse.
    CHECK_FALSE(refusal_for("[]"));
    CHECK_FALSE(refusal_for(""));
    const auto refused = refusal_for(R"({"mode":"auto","by":"me"})");
    REQUIRE(refused);
    CHECK(refused->status == http_status::bad_request);
    CHECK(refused->message == "A switch states a mode.");
}

TEST_CASE("record_routes_states_a_reply_without_the_protocols_own_words", "[record_routes]") {
    const auto payload = rr::object_without_members(
        R"({"success":true,"message":"","points":[],"total":2})", {"success", "message"});

    CHECK(payload == R"({"points":[],"total":2})");
    CHECK(rr::object_without_members(R"({"success":false})", {"success"}) == "{}");
    // A document that is not an object is a caller's own, and comes back as it was.
    CHECK(rr::object_without_members("[]", {"success"}) == "[]");
    CHECK(rr::object_without_members("", {"success"}).empty());
}

TEST_CASE("record_routes_states_a_screens_write_refusal_by_its_outcome", "[record_routes]") {
    // A screen's write keeps the BFF's answer, which withheld no outcome's status.
    const auto status_of = [](outcome o) {
        return record_routes_refusal_of(
            [&] { rr::require_done(result{o, "", "The service said why.", {}}); });
    };
    CHECK_FALSE(status_of(outcome::ok).has_value());
    CHECK(status_of(outcome::conflict)->status == http_status::conflict);
    CHECK(status_of(outcome::conflict)->code == "invalid-request");
    CHECK(status_of(outcome::invalid)->status == http_status::bad_request);
    CHECK(status_of(outcome::denied)->status == http_status::forbidden);
    CHECK(status_of(outcome::missing)->status == http_status::not_found);
    CHECK(status_of(outcome::unavailable)->status == http_status::bad_gateway);
    CHECK(status_of(outcome::failed)->code == "upstream-unavailable");
    CHECK(status_of(outcome::missing)->message == "The service said why.");
}
