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
#include <cctype>
#include <cstdlib>
#include <fstream>
#include <iterator>
#include <optional>
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

/// The version a credential's name carries, or nothing when it carries none.
[[nodiscard]] std::optional<int> version_of(const std::filesystem::path& path) {
    const std::string suffix = path.filename().string().substr(
        std::string(credential_source::kek_credential_prefix).size());
    if (suffix.empty() || suffix.find_first_not_of("0123456789") != std::string::npos) {
        return std::nullopt;
    }
    return std::stoi(suffix);
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
    const char* stated = std::getenv(std::string(directory_variable).c_str());
    const std::string directory = stated != nullptr ? stated : std::string();

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

    const auto version_text = read_credential(directory / kek_version_credential);
    std::string version_string(version_text.begin(), version_text.end());
    while (!version_string.empty() &&
           std::isspace(static_cast<unsigned char>(version_string.back()))) {
        version_string.pop_back();
    }
    if (version_string.find_first_not_of("0123456789") != std::string::npos ||
        version_string.empty()) {
        ORES_LOG_SEV(lg(), error) << "The credential " << (directory / kek_version_credential)
                                  << " does not state a version";
        throw std::runtime_error("The credential " + (directory / kek_version_credential).string() +
                                 " does not state a key version");
    }
    keys.current_version = std::stoi(version_string);

    // Every retained version, because a row records the one that sealed it.
    for (const auto& entry : std::filesystem::directory_iterator(directory)) {
        if (!entry.is_regular_file()) {
            continue;
        }
        const auto name = entry.path().filename().string();
        if (name.rfind(kek_credential_prefix, 0) != 0) {
            continue;
        }
        const auto version = version_of(entry.path());
        if (!version.has_value()) {
            continue;
        }
        auto key = read_credential(entry.path());
        check_length(key,
                     ores::security::crypto::secret_sealer::KEY_LEN,
                     "the key encryption key",
                     entry.path());
        keys.key_encryption_keys.emplace(*version, std::move(key));
    }

    if (keys.key_encryption_keys.find(keys.current_version) == keys.key_encryption_keys.end()) {
        ORES_LOG_SEV(lg(), error) << "The deployment states version " << keys.current_version
                                  << " and holds no key of it";
        throw std::runtime_error("The deployment holds no key encryption key of the version it "
                                 "states, " +
                                 std::to_string(keys.current_version));
    }

    keys.transport_private_key = read_credential(directory / transport_credential);
    check_length(keys.transport_private_key,
                 ores::security::crypto::sealed_box::KEY_LEN,
                 "the transport key",
                 directory / transport_credential);

    keys.transport_public_key =
        ores::security::crypto::sealed_box::public_key_of(keys.transport_private_key);

    ORES_LOG_SEV(lg(), info) << "Read the vault's keys from " << directory << ": versions "
                             << keys.key_encryption_keys.size() << ", current "
                             << keys.current_version;
    return keys;
}

}
