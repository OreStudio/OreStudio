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
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <string_view>

// Reading the actor out of a message's headers, and the pair of headers that
// forwards that actor onto a fire-and-forget publish. The workflow engine
// attributes a run with the first and every publisher of a start request
// carries the second, so the two are pinned together here.
namespace {

ores::nats::message with_header(std::string_view name, const std::string& value) {
    ores::nats::message msg;
    msg.headers[std::string(name)] = value;
    return msg;
}

std::string bearer(const std::string& token) {
    return std::string(ores::nats::headers::bearer_prefix) + token;
}

}

TEST_CASE("the_actor_bearer_prefers_the_delegated_header", "[nats][headers]") {
    auto msg = with_header(ores::nats::headers::authorization, bearer("last-hop"));
    msg.headers[std::string(ores::nats::headers::delegated_authorization)] = bearer("caller");
    CHECK(ores::nats::service::extract_actor_bearer(msg) == "caller");
}

TEST_CASE("the_actor_bearer_falls_back_to_the_authorization_header",
          "[nats][headers]") {
    const auto msg = with_header(ores::nats::headers::authorization, bearer("caller"));
    CHECK(ores::nats::service::extract_actor_bearer(msg) == "caller");
}

TEST_CASE("a_header_that_is_not_a_bearer_names_nobody", "[nats][headers]") {
    const auto msg = with_header(ores::nats::headers::authorization, "Basic abc");
    CHECK(ores::nats::service::extract_actor_bearer(msg).empty());
}

TEST_CASE("a_message_with_no_authorization_header_names_nobody", "[nats][headers]") {
    CHECK(ores::nats::service::extract_actor_bearer(ores::nats::message{}).empty());
}

TEST_CASE("forwarding_headers_carry_the_caller_as_delegated", "[nats][headers]") {
    const auto msg = with_header(ores::nats::headers::authorization, bearer("caller"));
    const auto hdrs = ores::nats::service::forwarded_caller_headers(msg);
    REQUIRE(hdrs.size() == 1);
    CHECK(hdrs.at(std::string(ores::nats::headers::delegated_authorization)) == bearer("caller"));
}

TEST_CASE("forwarding_headers_are_empty_when_there_is_no_caller", "[nats][headers]") {
    // A publisher that has no caller must not invent one: the receiver's
    // fallback for "no caller" is deliberately not any token.
    CHECK(ores::nats::service::forwarded_caller_headers(ores::nats::message{}).empty());
}
