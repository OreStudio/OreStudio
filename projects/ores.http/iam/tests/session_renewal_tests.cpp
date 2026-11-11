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
#include "ores.http/routes/iam/session_renewal.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>

using ores::http::domain::token_renewal;
using ores::http::routes::iam::session_renewal;
using outcome = token_renewal::outcome;

TEST_CASE("session_renewal_reads_iam_s_answer", "[session_renewal]") {
    ores::security::jwt::jwt_claims claims;
    claims.subject = "acc-1";

    const auto renewed = session_renewal::outcome_of({}, true, "t2", claims);
    CHECK(renewed.result == outcome::renewed);
    CHECK(renewed.token == "t2");
    REQUIRE(renewed.claims);
    CHECK(renewed.claims->subject == "acc-1");

    // A session past its maximum, stated in the header or as a failed answer.
    CHECK(session_renewal::outcome_of("max_session_exceeded", false, {}, std::nullopt).result ==
          outcome::refused);
    CHECK(session_renewal::outcome_of({}, false, {}, std::nullopt).result == outcome::refused);
    // A success that carries no token is malformed, so the current token is kept.
    CHECK(session_renewal::outcome_of({}, true, "", claims).result == outcome::unavailable);

    // A token that does not verify is not used, and the current one is kept.
    CHECK(session_renewal::outcome_of({}, true, "t2", std::nullopt).result == outcome::unavailable);
}
