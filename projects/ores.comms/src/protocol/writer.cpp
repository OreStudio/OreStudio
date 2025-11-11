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
#include <cstring>
#include <expected>
#include "ores.comms/protocol/writer.hpp"

namespace ores::comms::protocol {

void writer::
write_uint16(std::vector<std::uint8_t>& buffer, std::uint16_t value) {
    buffer.push_back(static_cast<std::uint8_t>(value >> 8));
    buffer.push_back(static_cast<std::uint8_t>(value & 0xFF));
}

void writer::
write_uint32(std::vector<std::uint8_t>& buffer, std::uint32_t value) {
    buffer.push_back(static_cast<std::uint8_t>(value >> 24));
    buffer.push_back(static_cast<std::uint8_t>((value >> 16) & 0xFF));
    buffer.push_back(static_cast<std::uint8_t>((value >> 8) & 0xFF));
    buffer.push_back(static_cast<std::uint8_t>(value & 0xFF));
}

void writer::
write_string(std::vector<std::uint8_t>& buffer, const std::string& str) {
    auto len = static_cast<std::uint16_t>(std::min(str.size(), size_t(65535)));
    write_uint16(buffer, len);
    buffer.insert(buffer.end(), str.begin(), str.begin() + len);
}

void writer::
write_uuid(std::vector<std::uint8_t>& buffer, const boost::uuids::uuid& uuid) {
    buffer.insert(buffer.end(), uuid.begin(), uuid.end());
}

void writer::write_bool(std::vector<std::uint8_t>& buffer, bool value) {
    buffer.push_back(value ? 1 : 0);
}

}
