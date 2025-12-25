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
#include <set>
#include <catch2/catch_test_macros.hpp>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/account.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/account_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/generators/account_generator.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[generators]");

}

using namespace ores::iam::generators;
using namespace ores::telemetry::log;

TEST_CASE("generate_single_account", tags) {
    auto lg(make_logger(test_suite));

    auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, debug) << "Generated account: " << account;

    CHECK(!account.username.empty());
    CHECK(!account.email.empty());
    CHECK(!account.password_hash.empty());
    CHECK(!account.password_salt.empty());
    CHECK(!account.totp_secret.empty());
    CHECK(!account.recorded_by.empty());
    CHECK(account.recorded_at != std::chrono::system_clock::time_point{});
}

TEST_CASE("generate_multiple_accounts", tags) {
    auto lg(make_logger(test_suite));

    auto accounts = generate_synthetic_accounts(3);
    BOOST_LOG_SEV(lg, debug) << "Generated accounts: " << accounts;

    CHECK(accounts.size() == 3);
}

