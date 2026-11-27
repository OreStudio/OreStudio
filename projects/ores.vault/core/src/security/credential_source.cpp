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
#include "ores.platform/credentials/credential_store.hpp"
#include "ores.security/crypto/sealed_box.hpp"
#include "ores.security/crypto/secret_sealer.hpp"
#include <cctype>
#include <stdexcept>
#include <string>

namespace ores::vault::security {

using namespace ores::logging;
using ores::platform::credentials::credential_store;

namespace {

[[nodiscard]] auto& lg() {
    static auto instance = make_logger("ores.vault.security.credential_source");
    return instance;
}

}

const std::vector<unsigned char>& key_material::key_of(int version) const {
    const auto found = key_encryption_keys.find(version);
    if (found == key_encryption_keys.end()) {
        ORES_LOG_SEV(lg(), error) << "No key of version " << version
                                  << " is retained, so no row sealed under it can open";
        throw std::runtime_error("The deployment retains no key encryption key of version " +
                                 std::to_string(version));
    }
    return found->second;
}

const std::vector<unsigned char>& key_material::current_key() const {
    return key_of(current_version);
}

key_material credential_source::load_from_credentials_directory() {
    key_material keys;

    const auto version_text = credential_store::read(kek_version_credential);
    std::string version_string(version_text.begin(), version_text.end());
    while (!version_string.empty() &&
           std::isspace(static_cast<unsigned char>(version_string.back()))) {
        version_string.pop_back();
    }
    if (version_string.empty() ||
        version_string.find_first_not_of("0123456789") != std::string::npos) {
        ORES_LOG_SEV(lg(), error) << "The credential " << kek_version_credential
                                  << " does not state a version";
        throw std::runtime_error("The credential " + std::string(kek_version_credential) +
                                 " does not state a key version");
    }
    keys.current_version = std::stoi(version_string);

    // Every retained version, because a row records the one that sealed it. The
    // versions are contiguous: a rotation adds the next and keeps the last.
    for (int version = 1; version <= keys.current_version; ++version) {
        const auto name = std::string(kek_credential_prefix) + std::to_string(version);
        auto key = credential_store::read(name, ores::security::crypto::secret_sealer::KEY_LEN);
        keys.key_encryption_keys.emplace(version, std::move(key));
    }

    if (keys.key_encryption_keys.find(keys.current_version) == keys.key_encryption_keys.end()) {
        ORES_LOG_SEV(lg(), error) << "The deployment states version " << keys.current_version
                                  << " and holds no key of it";
        throw std::runtime_error("The deployment holds no key encryption key of the version it "
                                 "states, " +
                                 std::to_string(keys.current_version));
    }

    keys.transport_private_key =
        credential_store::read(transport_credential, ores::security::crypto::sealed_box::KEY_LEN);
    keys.transport_public_key =
        ores::security::crypto::sealed_box::public_key_of(keys.transport_private_key);

    ORES_LOG_SEV(lg(), info) << "Read the vault's keys: versions "
                             << keys.key_encryption_keys.size() << ", current "
                             << keys.current_version;
    return keys;
}

}
