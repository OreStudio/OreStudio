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
#include "ores.nats/service/refusal.hpp"
#include "ores.nats/service/request_refused_error.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <string_view>

// A refused request is answered with no body and an X-Error header naming
// why. A caller that reads only the body reports a malformed payload, which
// sends the reader after a wire defect that does not exist, so the code the
// server stated is read out here and every refusal is named.
namespace {

ores::nats::message with_x_error(std::string_view code) {
    ores::nats::message msg;
    msg.headers[std::string(ores::nats::headers::x_error)] = std::string(code);
    return msg;
}

}

TEST_CASE("a_forbidden_reply_reports_the_code_the_server_sent", "[nats][refusal]") {
    const auto code = ores::nats::service::refusal_code(with_x_error("forbidden"));
    REQUIRE(code.has_value());
    CHECK(*code == "forbidden");
}

TEST_CASE("an_unauthorized_reply_reports_its_code", "[nats][refusal]") {
    const auto code = ores::nats::service::refusal_code(with_x_error("unauthorized"));
    REQUIRE(code.has_value());
    CHECK(*code == "unauthorized");
}

TEST_CASE("a_bad_request_reply_reports_its_code", "[nats][refusal]") {
    const auto code = ores::nats::service::refusal_code(with_x_error("bad_request"));
    REQUIRE(code.has_value());
    CHECK(*code == "bad_request");
}

TEST_CASE("an_expired_token_is_not_a_refusal", "[nats][refusal]") {
    CHECK_FALSE(ores::nats::service::refusal_code(with_x_error("token_expired")).has_value());
}

TEST_CASE("an_exceeded_session_is_not_a_refusal", "[nats][refusal]") {
    CHECK_FALSE(
        ores::nats::service::refusal_code(with_x_error("max_session_exceeded")).has_value());
}

TEST_CASE("a_reply_with_no_error_header_is_not_a_refusal", "[nats][refusal]") {
    CHECK_FALSE(ores::nats::service::refusal_code(ores::nats::message{}).has_value());
}

TEST_CASE("the_error_names_the_refusal_and_the_subject", "[nats][refusal]") {
    const ores::nats::service::request_refused_error error("forbidden", "iam.v1.accounts.list");
    CHECK(std::string(error.what()) == "the server refused 'iam.v1.accounts.list': forbidden");
    CHECK(error.code() == "forbidden");
}

TEST_CASE("a_refusal_is_thrown_so_no_caller_decodes_it", "[nats][refusal]") {
    CHECK_THROWS_AS(ores::nats::service::throw_if_refused(with_x_error("forbidden"),
                                                          "iam.v1.accounts.list"),
                    ores::nats::service::request_refused_error);
}

TEST_CASE("a_successful_reply_is_not_thrown", "[nats][refusal]") {
    CHECK_NOTHROW(ores::nats::service::throw_if_refused(ores::nats::message{},
                                                        "iam.v1.accounts.list"));
}

TEST_CASE("an_expired_token_is_left_to_the_caller", "[nats][refusal]") {
    CHECK_NOTHROW(ores::nats::service::throw_if_refused(with_x_error("token_expired"),
                                                        "iam.v1.accounts.list"));
}
