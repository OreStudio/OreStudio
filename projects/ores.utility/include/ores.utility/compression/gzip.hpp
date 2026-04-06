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
#ifndef ORES_UTILITY_COMPRESSION_GZIP_HPP
#define ORES_UTILITY_COMPRESSION_GZIP_HPP

#include <span>
#include <vector>

namespace ores::utility::compression {

/**
 * @brief Compresses data using gzip.
 *
 * @param input Raw bytes to compress.
 * @return Gzip-compressed bytes.
 * @throws std::runtime_error on compression failure.
 */
[[nodiscard]] std::vector<char> gzip_compress(std::span<const char> input);

/**
 * @brief Decompresses gzip data.
 *
 * @param input Gzip-compressed bytes.
 * @return Decompressed raw bytes.
 * @throws std::runtime_error on decompression failure.
 */
[[nodiscard]] std::vector<char> gzip_decompress(std::span<const char> input);

}

#endif
