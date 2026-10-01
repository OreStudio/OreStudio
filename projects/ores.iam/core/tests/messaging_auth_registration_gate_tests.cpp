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
#include "ores.utility/serialization/error_code.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[signup]");

using ores::iam::messaging::auth_registration_refusal;
using ores::iam::messaging::auth_registration_destination_refusal;
using ores::utility::serialization::error_code;
using ores::utility::serialization::to_string;

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

/*
 * The destination's decision, apart from the reads that supply it. The reads
 * are exercised by the running deployment.
 */

TEST_CASE("registration_is_refused_when_the_address_names_no_tenant_and_none_is_nominated", tags) {
    const auto refusal = auth_registration_destination_refusal(false, false);

    REQUIRE(refusal.has_value());
    CHECK(refusal->first == "no_registration_destination");
    CHECK_FALSE(refusal->second.empty());
}

TEST_CASE("registration_is_refused_when_the_tenant_nominates_no_role", tags) {
    const auto refusal = auth_registration_destination_refusal(true, false);

    REQUIRE(refusal.has_value());
    CHECK(refusal->first == "no_default_role");
    CHECK_FALSE(refusal->second.empty());
}

TEST_CASE("registration_has_a_destination_when_the_tenant_and_the_role_are_known", tags) {
    CHECK_FALSE(auth_registration_destination_refusal(true, true).has_value());
}

/*
 * The strings a screen branches on. A client matches the code, not the
 * sentence, so the enum name is part of the protocol and a rename is a
 * breaking change.
 */

TEST_CASE("every_registration_and_sign_in_refusal_names_the_code_a_client_branches_on", tags) {
    CHECK(to_string(error_code::signup_disabled) == "signup_disabled");
    CHECK(to_string(error_code::signup_requires_authorization) == "signup_requires_authorization");
    CHECK(to_string(error_code::username_taken) == "username_taken");
    CHECK(to_string(error_code::email_taken) == "email_taken");
    CHECK(to_string(error_code::weak_password) == "weak_password");
    CHECK(to_string(error_code::no_registration_destination) == "no_registration_destination");
    CHECK(to_string(error_code::no_default_role) == "no_default_role");
    CHECK(to_string(error_code::invalid_credentials) == "invalid_credentials");
    CHECK(to_string(error_code::account_locked) == "account_locked");
    CHECK(to_string(error_code::account_pending) == "account_pending");
    CHECK(to_string(error_code::no_party_assignment) == "no_party_assignment");
    CHECK(to_string(error_code::tenant_inactive) == "tenant_inactive");
}
