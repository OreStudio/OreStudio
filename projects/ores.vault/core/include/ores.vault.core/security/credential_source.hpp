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
#ifndef ORES_VAULT_CORE_SECURITY_CREDENTIAL_SOURCE_HPP
#define ORES_VAULT_CORE_SECURITY_CREDENTIAL_SOURCE_HPP

#include "ores.vault.core/export.hpp"
#include <cstddef>
#include <filesystem>
#include <map>
#include <stdexcept>
#include <string_view>
#include <vector>

namespace ores::vault::security {

/**
 * @brief The keys the vault cannot start without.
 *
 * Every retained key encryption key is here, not only the current one: a row
 * records the version that sealed it, so opening that row needs the key of that
 * version, which a rotation must therefore keep.
 *
 * The transport key pair is how a caller seals a value to the vault and how the
 * vault answers. The private half stays in this process, and the public half is
 * derived from it rather than read from a second file, so the two cannot
 * disagree.
 */
struct ORES_VAULT_CORE_EXPORT key_material {
    /// The AES-256 keys that seal stored values, by their version.
    std::map<int, std::vector<unsigned char>> key_encryption_keys;

    /// The version a write seals under, stated by the deployment.
    int current_version = 0;

    /// The X25519 private key that opens what a caller sealed, at 32 bytes.
    std::vector<unsigned char> transport_private_key;

    /// The public half of the transport key, at 32 bytes. Not a secret.
    std::vector<unsigned char> transport_public_key;

    /**
     * @brief The key of one version.
     *
     * @param version The version a row records.
     * @return The key that opens that row.
     *
     * @throws std::runtime_error If the deployment retains no key of that
     * version, which is a row nothing can open.
     */
    [[nodiscard]] const std::vector<unsigned char>& key_of(int version) const;

    /// The key a write seals under. Throws under the same condition as key_of.
    [[nodiscard]] const std::vector<unsigned char>& current_key() const;
};

/**
 * @brief Reads the vault's keys from the credentials systemd hands the unit.
 *
 * A systemd credential is a file the unit's manager places for it and the unit
 * cannot write, which is why the keys are read and never generated here: a key
 * this process could mint is a key an attacker who owns this process could
 * replace.
 *
 * The directory is stated by the environment and never by configuration, so no
 * checkout, environment file or table names a key.
 */
class ORES_VAULT_CORE_EXPORT credential_source {
public:
    /// The variable systemd sets to the directory of credentials it placed.
    static constexpr std::string_view directory_variable = "CREDENTIALS_DIRECTORY";

    /// The credential holding the version a write seals under.
    static constexpr std::string_view kek_version_credential = "vault.kek.version";

    /// The prefix of a credential holding one version of the key encryption key.
    static constexpr std::string_view kek_credential_prefix = "vault.kek.";

    /// The credential holding the transport private key: 32 raw bytes.
    static constexpr std::string_view transport_credential = "vault.transport.key";

    /**
     * @brief Loads the keys from the store the platform resolves.
     *
     * @return The keys, with every retained version and the public half derived
     * from the private one.
     *
     * @throws std::runtime_error If the environment names no directory, a
     * credential is absent, or a key is not the length its cipher takes.
     */
    [[nodiscard]] static key_material load_from_credentials_directory();
};

}

#endif
