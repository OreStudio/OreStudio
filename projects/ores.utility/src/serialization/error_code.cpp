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
#include "ores.utility/serialization/error_code.hpp"
#include <magic_enum/magic_enum.hpp>

namespace ores::utility::serialization {

std::ostream& operator<<(std::ostream& os, error_code ec) {
    return os << magic_enum::enum_name(ec) << " (0x" << std::hex
              << static_cast<std::uint16_t>(ec) << std::dec << ")";
}

std::string to_string(error_code ec) {
    return std::string(magic_enum::enum_name(ec));
}

}
