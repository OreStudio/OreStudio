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
#ifndef ORES_UTILITY_CRYPTO_SHA256_HPP
#define ORES_UTILITY_CRYPTO_SHA256_HPP

#include "ores.utility/export.hpp"
#include <filesystem>
#include <string>
#include <string_view>

namespace ores::utility::crypto {

/**
 * @brief SHA256 digest helper, hex-encoded lowercase output.
 */
class ORES_UTILITY_EXPORT sha256 {
public:
    /**
     * @brief Hex-encoded SHA256 digest of @p data.
     */
    static std::string hex_digest(std::string_view data);

    /**
     * @brief Hex-encoded SHA256 digest of the file at @p path, streamed in
     * fixed-size chunks so the whole file need not fit in memory.
     */
    static std::string hex_digest_of_file(const std::filesystem::path& path);
};

}

#endif
