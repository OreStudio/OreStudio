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
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[login]");

using ores::iam::messaging::auth_bootstrap_party_id;
using ores::iam::messaging::party_summary;

const std::string system_id = "11111111-1111-1111-1111-111111111111";
const std::string trading_id = "22222222-2222-2222-2222-222222222222";

party_summary party(const std::string& id, const std::string& category) {
    return party_summary{
        .id = id, .name = id, .party_category = category, .business_center_code = ""};
}

}

/*
 * Which party a sign-in opens on, apart from the database reads that supply the
 * tenant's flag and the account's parties. A tenant being set up writes
 * tenant-wide settings, which are scoped to its system party, so the session
 * must sit there and no choice may be offered.
 */

TEST_CASE("a_tenant_being_set_up_signs_in_to_its_system_party", tags) {
    const auto chosen =
        auth_bootstrap_party_id(true, {party(trading_id, "Trading"), party(system_id, "System")});

    REQUIRE(chosen.has_value());
    CHECK(boost::uuids::to_string(*chosen) == system_id);
}

TEST_CASE("a_tenant_being_set_up_whose_only_party_is_the_system_one_signs_in_to_it", tags) {
    const auto chosen = auth_bootstrap_party_id(true, {party(system_id, "System")});

    REQUIRE(chosen.has_value());
    CHECK(boost::uuids::to_string(*chosen) == system_id);
}

TEST_CASE(
    "a_tenant_being_set_up_without_a_system_party_among_the_accounts_parties_offers_the_choice",
    tags) {
    CHECK_FALSE(auth_bootstrap_party_id(true, {party(trading_id, "Trading")}).has_value());
}

TEST_CASE("an_ordinary_sign_in_offers_the_choice_even_when_the_account_holds_the_system_party",
          tags) {
    CHECK_FALSE(
        auth_bootstrap_party_id(false, {party(trading_id, "Trading"), party(system_id, "System")})
            .has_value());
}
