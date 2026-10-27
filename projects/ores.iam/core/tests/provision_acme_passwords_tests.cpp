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
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/service/account_operations_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/faker/internet.hpp"
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/internet.h>
#include <fstream>
#include <iterator>
#include <regex>
#include <string>
#include <vector>

/**
 * @file provision_acme_passwords_tests.cpp
 * @brief The password an Acme staff account signs in with.
 *
 * The Acme accounts are published without credentials; the provisioning step
 * that follows load_staff sets each staff account's password through the
 * server's own reset-password operation. This test holds the two ends of that
 * arrangement together. The starting point states the demonstration password
 * in one place, an account whose password is set that way authenticates with
 * it through the same service a sign-in uses, and the seed artefacts carry no
 * password material at all.
 */

namespace {

// The demonstration password, as the contract states it. The profile row is
// asserted against this literal, so the two disagree loudly.
const std::string development_password("Secure-Password-123");

const std::vector<std::string> account_artefact_scripts{
    "projects/ores.sql/populate/acme/acme_group_accounts_artefact_populate.sql",
    "projects/ores.sql/populate/acme/acme_uk_accounts_artefact_populate.sql",
    "projects/ores.sql/populate/acme/acme_us_accounts_artefact_populate.sql",
    "projects/ores.sql/populate/acme/acme_hk_accounts_artefact_populate.sql",
};

std::string read_repo_file(const std::string& relative_path) {
    std::ifstream in(ores::testing::project_root::resolve(relative_path));
    REQUIRE(in.good());
    return {std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>()};
}

}

TEST_CASE("the acme profile states the demonstration password once", "[seed][acme][passwords]") {
    const auto profile =
        read_repo_file("projects/ores.sql/populate/iam/iam_seed_profiles_populate.sql");
    const std::regex row(
        R"re(\(\s*'acme_demo'\s*,\s*'set_staff_passwords'\s*,\s*\d+\s*,\s*'\{"password":\s*"([^"]+)"\})re");

    std::vector<std::string> passwords;
    for (std::sregex_iterator it(profile.begin(), profile.end(), row), end; it != end; ++it)
        passwords.push_back((*it)[1].str());

    REQUIRE(passwords.size() == 1);
    CHECK(passwords.front() == development_password);
}

TEST_CASE("an account whose password is provisioned this way authenticates",
          "[seed][acme][passwords]") {
    ores::testing::scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    ores::iam::service::account_operations_service sut(h.context());

    const auto expected = ores::iam::generators::generate_synthetic_account(ctx);
    const auto account = sut.create_account(
        expected.username, expected.email, faker::internet::password(), expected.modified_by);

    // What the set_staff_passwords step calls: the account service's own reset,
    // which validates and hashes the plaintext with the server's hasher.
    CHECK(sut.reset_password(account.id, development_password).empty());

    const auto ip = ores::utility::faker::internet::ipv4();
    const auto session = sut.login(expected.username, development_password, ip);
    CHECK(session.account.id == account.id);

    // Re-running the step is a no-op the service states, which the step reads
    // as "already set" rather than as a failure.
    CHECK(sut.reset_password(account.id, development_password) ==
          ores::iam::service::password_unchanged_message);

    CHECK_THROWS_AS(sut.login(expected.username, "not-the-development-password", ip),
                    std::runtime_error);
}

TEST_CASE("the acme account artefacts carry no password material", "[seed][acme][passwords]") {
    for (const auto& script : account_artefact_scripts) {
        const auto text = read_repo_file(script);
        INFO("artefact script: " << script);
        CHECK(text.find("$scrypt$") == std::string::npos);
    }
}
