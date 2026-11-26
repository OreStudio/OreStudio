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
#include "ores.logging/logger.hpp"
#include "ores.platform/filesystem/scoped_temp_directory.hpp"
#include "ores.security/crypto/sealed_box.hpp"
#include "ores.security/crypto/secret_sealer.hpp"
#include "ores.vault.core/security/credential_source.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

using ores::platform::filesystem::scoped_temp_directory;
using ores::security::crypto::sealed_box;
using ores::security::crypto::secret_sealer;
using ores::vault::security::credential_source;

/// Writes raw bytes to a credential file, as the deployment does.
void write_credential(const std::filesystem::path& path,
                      const std::vector<unsigned char>& bytes) {
    std::ofstream out(path, std::ios::binary);
    out.write(reinterpret_cast<const char*>(bytes.data()),
              static_cast<std::streamsize>(bytes.size()));
}

/// A key of the length a credential holds, with a counter in its bytes.
std::vector<unsigned char> make_key(unsigned char seed, std::size_t len) {
    std::vector<unsigned char> key(len);
    for (std::size_t i = 0; i < key.size(); ++i) {
        key[i] = static_cast<unsigned char>(seed + i);
    }
    return key;
}

/// Writes both credentials into a directory the test owns.
void provision(const std::filesystem::path& directory,
               const std::vector<unsigned char>& kek,
               const std::vector<unsigned char>& transport) {
    write_credential(directory / credential_source::kek_credential, kek);
    write_credential(directory / credential_source::transport_credential, transport);
}

}

TEST_CASE("the_vault_reads_its_two_credentials", "[vault][security]") {
    auto lg(ores::logging::make_logger(test_suite));

    scoped_temp_directory directory;
    const auto kek = make_key(0, secret_sealer::KEY_LEN);
    auto transport = make_key(7, sealed_box::KEY_LEN);
    transport = sealed_box::generate().private_key;
    provision(directory.path(), kek, transport);

    const auto keys = credential_source::load_from(directory.path());
    ORES_LOG_SEV(lg, info) << "Read both credentials, and derived the public half";

    CHECK(keys.key_encryption_key == kek);
    CHECK(keys.transport_private_key == transport);
    CHECK(keys.transport_public_key == sealed_box::public_key_of(transport));
}

TEST_CASE("a_missing_credential_is_refused", "[vault][security]") {
    scoped_temp_directory directory;
    provision(directory.path(),
              make_key(0, secret_sealer::KEY_LEN),
              sealed_box::generate().private_key);
    std::filesystem::remove(directory.path() / credential_source::transport_credential);

    CHECK_THROWS_AS(credential_source::load_from(directory.path()), std::runtime_error);
}

TEST_CASE("a_credential_of_the_wrong_length_is_refused", "[vault][security]") {
    scoped_temp_directory directory;
    provision(directory.path(),
              make_key(0, secret_sealer::KEY_LEN - 1),
              sealed_box::generate().private_key);

    CHECK_THROWS_AS(credential_source::load_from(directory.path()), std::runtime_error);
}

TEST_CASE("no_credentials_directory_is_refused", "[vault][security]") {
    unsetenv(std::string(credential_source::directory_variable).c_str());

    CHECK_THROWS_AS(credential_source::load_from_credentials_directory(), std::runtime_error);
}
