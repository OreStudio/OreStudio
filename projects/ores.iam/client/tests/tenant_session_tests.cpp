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
#include "ores.iam.client/client/tenant_session.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>

namespace {

using ores::iam::client::tenant_session;
using std::chrono::seconds;
using std::chrono::system_clock;

const system_clock::time_point origin = system_clock::time_point{} + std::chrono::hours{24 * 365};

const std::string tags("[tenant_session]");

}

TEST_CASE("a_session_with_no_token_is_entered", tags) {
    const tenant_session session(std::string{}, seconds{60});
    CHECK(session.needs_entry(origin));
    CHECK(session.token().empty());
}

TEST_CASE("an_entered_session_is_reused_until_its_margin", tags) {
    tenant_session session(std::string{}, seconds{60});
    session.entered("token-1", 600, origin);

    CHECK_FALSE(session.needs_entry(origin));
    CHECK_FALSE(session.needs_entry(origin + seconds{539}));
    // 540 seconds in, the margin of a minute is reached and the session is
    // entered again rather than used until it fails.
    CHECK(session.needs_entry(origin + seconds{540}));
    CHECK(session.needs_entry(origin + seconds{600}));
    CHECK(session.token() == "token-1");
}

TEST_CASE("a_short_session_is_renewed_before_it_runs_out", tags) {
    tenant_session session(std::string{}, seconds{60});
    session.entered("token-1", 100, origin);

    // The margin is capped at half the lifetime, so a hundred second session
    // is renewed at fifty rather than at forty.
    CHECK_FALSE(session.needs_entry(origin + seconds{49}));
    CHECK(session.needs_entry(origin + seconds{50}));
    CHECK(session.margin() == seconds{50});
}

TEST_CASE("a_session_says_which_party_it_serves", tags) {
    const tenant_session session("party-1", seconds{60});
    CHECK(session.party_id() == "party-1");
}
