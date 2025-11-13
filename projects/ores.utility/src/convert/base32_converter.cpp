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
#include "ores.utility/convert/base32_converter.hpp"

#include <array>

namespace {

static const std::array<char, 33> alphabet("ABCDEFGHIJKLMNOPQRSTUVWXYZ234567");

}

namespace ores::utility::converter {

std::string base32_converter::convert(const std::vector<uint8_t>& data) {
    std::string result;
    size_t bit_index = 0;
    size_t current_byte = 0;

    for (uint8_t byte : data) {
        current_byte = (current_byte << 8) | byte;
        bit_index += 8;
        while (bit_index >= 5) {
            bit_index -= 5;
            size_t idx = (current_byte >> bit_index) & 0x1F;
            result += alphabet[idx];
        }
    }
    if (bit_index > 0) {
        result += alphabet[(current_byte << (5 - bit_index)) & 0x1F];
    }
    return result;
}

}
