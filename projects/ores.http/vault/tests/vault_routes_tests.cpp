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
#include "ores.security/crypto/sealed_box.hpp"
#include "ores.vault.api/messaging/custody_context.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using ores::security::crypto::sealed_box;
using ores::vault::messaging::custody_context;

/// The two ends of a write, without a gateway or a vault: the caller seals to
/// the vault's key under the context the api part states, and the vault opens
/// it. A change to either side of that contract breaks this test.
TEST_CASE("a_write_sealed_for_the_vault_opens_with_the_vaults_key", "[vault][routes]") {
    const auto vault = sealed_box::generate();
    const std::string value = "the-provider-client-secret";

    const auto sealed =
        sealed_box::seal(vault.public_key, value, custody_context::write("a.name", "ores.http"));

    CHECK(sealed_box::open(
              vault.private_key, sealed, custody_context::write("a.name", "ores.http")) == value);
    CHECK_THROWS(sealed_box::open(
        vault.private_key, sealed, custody_context::write("another", "ores.http")));
}

/// The two ends of a reveal: the vault seals to the gateway's key under the
/// delivery context, and the gateway opens it. The subject comes from the
/// resolve answer, which is why a gateway need not read its own token.
TEST_CASE("a_value_sealed_to_the_gateway_opens_with_its_key", "[vault][routes]") {
    const auto gateway = sealed_box::generate();
    const std::string subject = "ores.http";
    const std::string value = "the-provider-client-secret";

    const auto sealed = sealed_box::seal(
        gateway.public_key, value, custody_context::delivery(subject, "a.name", 3));

    CHECK(sealed_box::open(gateway.private_key,
                           sealed,
                           custody_context::delivery(subject, "a.name", 3)) == value);
    CHECK_THROWS(sealed_box::open(
        gateway.private_key, sealed, custody_context::delivery(subject, "a.name", 2)));
}

}
