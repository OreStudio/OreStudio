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
#ifndef ORES_SECURITY_CRYPTO_KEY_FILE_HPP
#define ORES_SECURITY_CRYPTO_KEY_FILE_HPP

#include "ores.security/export.hpp"
#include <cstddef>
#include <filesystem>
#include <optional>
#include <vector>

namespace ores::security::crypto {

/**
 * @brief A raw key, read from the file a deployment placed for a unit.
 *
 * systemd hands a service its credentials as files under a directory it names
 * in the environment, and the service cannot write them. A key read from there
 * is a key no build generated and no environment variable carried, which is why
 * both the vault and the gateway read their keys this way.
 */
class ORES_SECURITY_EXPORT key_file {
public:
    /// The variable systemd sets to the directory of credentials it placed.
    static constexpr std::string_view directory_variable = "CREDENTIALS_DIRECTORY";

    /**
     * @brief The directory the environment names, or nothing when it names none.
     */
    [[nodiscard]] static std::optional<std::filesystem::path> credential_directory();

    /**
     * @brief Reads one credential as raw bytes.
     *
     * @param path The credential file.
     * @param expected_length The length the cipher that takes it accepts.
     * @return The bytes.
     *
     * @throws std::runtime_error If the file is absent, or its length is not
     * the one the cipher accepts.
     */
    [[nodiscard]] static std::vector<unsigned char> read(const std::filesystem::path& path,
                                                         std::size_t expected_length);
};

}

#endif
