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
#ifndef ORES_COMMS_PROTOCOL_CRC_HPP
#define ORES_COMMS_PROTOCOL_CRC_HPP

#include <cstdint>
#include <span>

namespace ores::comms::protocol {

/**
 * @brief Calculate CRC32 checksum for data integrity validation.
 *
 * Uses the standard CRC32 polynomial (0xEDB88320) for data validation.
 */
class crc32 final {
public:
    crc32();

    /**
     * @brief Calculate CRC32 for a span of bytes.
     */
    static uint32_t calculate(std::span<const uint8_t> data);

    /**
     * @brief Update CRC32 with additional data.
     */
    void update(std::span<const uint8_t> data);

    /**
     * @brief Get the final CRC32 value.
     */
    uint32_t finalize() const;

    /**
     * @brief Reset the CRC32 calculator.
     */
    void reset();

private:
    uint32_t crc_;
    static constexpr uint32_t polynomial_ = 0xEDB88320;
    static uint32_t table_[256];
    static bool table_initialized_;

    static void initialize_table();
};

}

#endif
