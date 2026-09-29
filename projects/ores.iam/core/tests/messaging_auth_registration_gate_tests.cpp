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

#include "ores.iam.core/messaging/auth_handler.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[signup]");

using ores::iam::messaging::auth_registration_refusal;

}

/*
 * The decision the registration gate makes, apart from the settings read that
 * supplies its two flags. The read is exercised by the running deployment and
 * by the HTTP recipe that captures a refused registration.
 */

TEST_CASE("registration_is_refused_when_the_deployment_does_not_accept_signups", tags) {
    const auto refusal = auth_registration_refusal(false, false);

    REQUIRE(refusal.has_value());
    CHECK(refusal->first == "signup_disabled");
    CHECK_FALSE(refusal->second.empty());
}

TEST_CASE("registration_is_refused_when_the_deployment_approves_accounts_by_hand", tags) {
    const auto refusal = auth_registration_refusal(true, true);

    REQUIRE(refusal.has_value());
    CHECK(refusal->first == "signup_requires_authorization");
    CHECK_FALSE(refusal->second.empty());
}

TEST_CASE("registration_is_admitted_when_the_deployment_accepts_it", tags) {
    CHECK_FALSE(auth_registration_refusal(true, false).has_value());
}

TEST_CASE("a_deployment_that_does_not_accept_signups_answers_the_same_either_way", tags) {
    const auto without_approval = auth_registration_refusal(false, false);
    const auto with_approval = auth_registration_refusal(false, true);

    REQUIRE(without_approval.has_value());
    REQUIRE(with_approval.has_value());
    CHECK(without_approval->first == with_approval->first);
}
