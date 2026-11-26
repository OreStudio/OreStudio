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
#include "ores.vault.core/security/credential_source.hpp"
#include "ores.logging/logger.hpp"
#include "ores.security/crypto/sealed_box.hpp"
#include "ores.security/crypto/secret_sealer.hpp"
#include <cstdlib>
#include <fstream>
#include <iterator>
#include <stdexcept>
#include <string>

namespace ores::vault::security {

using namespace ores::logging;

namespace {

[[nodiscard]] auto& lg() {
    static auto instance = make_logger("ores.vault.security.credential_source");
    return instance;
}

/// Reads a credential file as raw bytes, or says which file is missing.
[[nodiscard]] std::vector<unsigned char> read_credential(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    if (!in) {
        ORES_LOG_SEV(lg(), error) << "No credential at " << path;
        throw std::runtime_error("The vault has no credential at " + path.string());
    }

    const std::string bytes{std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>()};
    return std::vector<unsigned char>(bytes.begin(), bytes.end());
}

/// Refuses a key whose length the cipher that takes it would refuse anyway.
void check_length(const std::vector<unsigned char>& key,
                  std::size_t expected,
                  std::string_view what,
                  const std::filesystem::path& path) {
    if (key.size() != expected) {
        ORES_LOG_SEV(lg(), error) << "The credential " << path << " holds " << key.size()
                                  << " bytes and " << what << " takes " << expected;
        throw std::runtime_error("The credential " + path.string() + " holds " +
                                 std::to_string(key.size()) + " bytes and " + std::string(what) +
                                 " takes " + std::to_string(expected));
    }
}

}

key_material credential_source::load_from_credentials_directory() {
    const std::string directory = std::getenv(std::string(directory_variable).c_str()) != nullptr
                                      ? std::getenv(std::string(directory_variable).c_str())
                                      : std::string();

    if (directory.empty()) {
        ORES_LOG_SEV(lg(), error) << "The unit was started without " << directory_variable
                                  << ", so the vault has no keys";
        throw std::runtime_error("The vault has no key material: the unit was started without " +
                                 std::string(directory_variable));
    }

    return load_from(directory);
}

key_material credential_source::load_from(const std::filesystem::path& directory) {
    key_material keys;

    keys.key_encryption_key = read_credential(directory / kek_credential);
    check_length(keys.key_encryption_key,
                 ores::security::crypto::secret_sealer::KEY_LEN,
                 "the key encryption key",
                 directory / kek_credential);

    keys.transport_private_key = read_credential(directory / transport_credential);
    check_length(keys.transport_private_key,
                 ores::security::crypto::sealed_box::KEY_LEN,
                 "the transport key",
                 directory / transport_credential);

    keys.transport_public_key =
        ores::security::crypto::sealed_box::public_key_of(keys.transport_private_key);

    ORES_LOG_SEV(lg(), info) << "Read the vault's two credentials from " << directory;
    return keys;
}

}
