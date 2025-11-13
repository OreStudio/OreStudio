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
#include "ores.utility/faker/totp.hpp"

#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/convert/base32_converter.hpp"

namespace ores::utility::faker {

std::string totp::totp_secret(std::size_t num_bytes) {
    std::vector<uint8_t> bytes(num_bytes);
    for (auto& b : bytes) {
        b = static_cast<uint8_t>(::faker::number::integer(0, 255));
    }
    return converter::base32_converter::convert(bytes);
}

}
