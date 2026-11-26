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
#include <string_view>
#include <vector>

namespace ores::vault::security {

/**
 * @brief The keys the vault cannot start without.
 *
 * The key encryption key protects every stored value and never leaves the
 * process that reads it. The transport key pair is how a caller seals a value
 * to the vault and how the vault answers: the private half stays here, and the
 * public half is published so that a caller can seal to it.
 *
 * The public half is derived from the private half rather than read from a
 * second file, so the two cannot disagree.
 */
struct ORES_VAULT_CORE_EXPORT key_material {
    /// The AES-256 key that seals every stored value, at 32 bytes.
    std::vector<unsigned char> key_encryption_key;

    /// The X25519 private key that opens what a caller sealed, at 32 bytes.
    std::vector<unsigned char> transport_private_key;

    /// The public half of the transport key, at 32 bytes. Not a secret.
    std::vector<unsigned char> transport_public_key;
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

    /// The credential holding the key encryption key: 32 raw bytes.
    static constexpr std::string_view kek_credential = "vault.kek";

    /// The credential holding the transport private key: 32 raw bytes.
    static constexpr std::string_view transport_credential = "vault.transport.key";

    /**
     * @brief Loads the keys from the directory the environment names.
     *
     * @return The three keys, with the public half derived from the private.
     *
     * @throws std::runtime_error If the environment names no directory, a
     * credential is absent, or a key is not the length its cipher takes.
     */
    [[nodiscard]] static key_material load_from_credentials_directory();

    /**
     * @brief Loads the keys from a named directory.
     *
     * @param directory The directory holding the two credential files.
     * @return The three keys, with the public half derived from the private.
     *
     * @throws std::runtime_error If a credential is absent, or a key is not the
     * length its cipher takes.
     */
    [[nodiscard]] static key_material load_from(const std::filesystem::path& directory);
};

}

#endif
