/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_COMMS_MESSAGING_COMPRESSION_HPP
#define ORES_COMMS_MESSAGING_COMPRESSION_HPP

#include <span>
#include <vector>
#include <cstddef>
#include <expected>
#include "ores.comms/messaging/message_types.hpp"

namespace ores::comms::messaging {

/**
 * @brief Compress data using the specified algorithm.
 *
 * @param data The data to compress
 * @param type The compression algorithm to use
 * @return The compressed data, or error_code on failure
 */
std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>
compress(std::span<const std::byte> data, compression_type type);

/**
 * @brief Decompress data using the specified algorithm.
 *
 * @param data The compressed data
 * @param type The compression algorithm used
 * @return The decompressed data, or error_code on failure
 */
std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>
decompress(std::span<const std::byte> data, compression_type type);

/**
 * @brief Check if compression is supported for the given type.
 *
 * @param type The compression type to check
 * @return true if the compression type is supported
 */
bool is_compression_supported(compression_type type);

}

#endif
