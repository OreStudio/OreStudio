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
#ifndef ORES_PLATFORM_CREDENTIALS_CREDENTIAL_STORE_HPP
#define ORES_PLATFORM_CREDENTIALS_CREDENTIAL_STORE_HPP

#include "ores.platform/export.hpp"
#include <cstddef>
#include <filesystem>
#include <optional>
#include <span>
#include <string_view>
#include <vector>

namespace ores::platform::credentials {

/**
 * @brief The keys a deployment placed for this process, read by name.
 *
 * The contract is deliberately the smallest one that keeps the caller out of a
 * platform decision: a name in, the bytes out. Where those bytes came from, and
 * what protects them on the way, is this component's business, because it is
 * the one thing that differs by operating system and the one thing a caller has
 * no business choosing.
 *
 * Each backend answers the same question with the strongest mechanism its host
 * offers:
 *
 * - *A host whose manager places credentials* states the directory in
 *   =CREDENTIALS_DIRECTORY=, and the files arrive read-only, inside a directory
 *   the unit cannot write. This is what systemd does for a service, and it is
 *   checked first on every platform because a launcher that offers it is
 *   offering the strongest guarantee there is.
 * - *Windows* keeps the bytes under =%LOCALAPPDATA%\ORES\credentials=,
 *   encrypted with DPAPI for the current user, so the protection travels with
 *   the file rather than with a directory permission.
 * - *macOS* keeps them under
 *   =~/Library/Application Support/ORES/credentials=, in a directory only the
 *   user can enter and files only the user can read. The Keychain is the
 *   stronger store and is the step after this one; nothing in the caller
 *   changes when it arrives.
 * - *Linux without a manager* has no store of its own and refuses, because a
 *   key this process can find in a checkout is a key an attacker who owns the
 *   process can replace.
 */
class ORES_PLATFORM_EXPORT credential_store {
public:
    /// The variable a launcher may set to a directory of credentials. Every
    /// platform checks it first; a platform that has its own store falls back
    /// to it when the variable is absent.
    static constexpr std::string_view directory_variable = "CREDENTIALS_DIRECTORY";

    /**
     * @brief The bytes of one credential.
     *
     * @param name The credential's name, which is also its file name.
     * @return The bytes, of whatever length the credential has.
     *
     * @throws std::runtime_error If this host holds no credential of that name.
     */
    [[nodiscard]] static std::vector<unsigned char> read(std::string_view name);

    /**
     * @brief The bytes of one credential, refusing a length the cipher cannot take.
     *
     * @param name The credential's name.
     * @param expected_length The only length the caller's cipher accepts.
     * @return The bytes.
     *
     * @throws std::runtime_error If the credential is absent, or its length is
     * not the expected one.
     */
    [[nodiscard]] static std::vector<unsigned char> read(std::string_view name,
                                                         std::size_t expected_length);

    /**
     * @brief Places one credential, for the deployment and for tests.
     *
     * Production placements belong to the deployment: a process that writes the
     * key which protects its own data has already lost the argument. This
     * exists so a provisioning step and a test can put a key where the
     * platform's backend will look for it.
     *
     * @param name The credential's name.
     * @param bytes The bytes to place.
     *
     * @throws std::runtime_error If this host has nowhere to place one.
     */
    static void write(std::string_view name, std::span<const unsigned char> bytes);

    /**
     * @brief Where this platform keeps its own store, when it has one.
     *
     * Nothing on Linux, where the store is the launcher's; the Windows and
     * macOS directories otherwise.
     */
    [[nodiscard]] static std::optional<std::filesystem::path> store_directory();
};

}

#endif
