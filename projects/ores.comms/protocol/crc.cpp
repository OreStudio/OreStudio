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
#include "ores.comms/protocol/crc.hpp"

namespace ores::comms::protocol {

uint32_t crc32::table_[256];
bool crc32::table_initialized_ = false;

void crc32::initialize_table() {
    for (uint32_t i = 0; i < 256; ++i) {
        uint32_t crc = i;
        for (uint32_t j = 0; j < 8; ++j) {
            if (crc & 1) {
                crc = (crc >> 1) ^ polynomial_;
            } else {
                crc >>= 1;
            }
        }
        table_[i] = crc;
    }
    table_initialized_ = true;
}

crc32::crc32() : crc_(0xFFFFFFFF) {
    if (!table_initialized_) {
        initialize_table();
    }
}

uint32_t crc32::calculate(std::span<const uint8_t> data) {
    crc32 calc;
    calc.update(data);
    return calc.finalize();
}

void crc32::update(std::span<const uint8_t> data) {
    for (const uint8_t byte : data) {
        crc_ = (crc_ >> 8) ^ table_[(crc_ ^ byte) & 0xFF];
    }
}

uint32_t crc32::finalize() const {
    return crc_ ^ 0xFFFFFFFF;
}

void crc32::reset() {
    crc_ = 0xFFFFFFFF;
}

}
