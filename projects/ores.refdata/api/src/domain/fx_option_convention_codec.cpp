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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_codec.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.refdata.api/domain/fx_option_convention_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::refdata::domain::fx_option_convention>::write_json(
    const ores::refdata::domain::fx_option_convention& v) {
    return object::write_json(v);
}

ores::refdata::domain::fx_option_convention
wire_traits<ores::refdata::domain::fx_option_convention>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::domain::fx_option_convention>(text);
}

std::string wire_traits<ores::refdata::domain::fx_option_convention>::write_msgpack(
    const ores::refdata::domain::fx_option_convention& v) {
    return object::write_msgpack(v);
}

ores::refdata::domain::fx_option_convention
wire_traits<ores::refdata::domain::fx_option_convention>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::domain::fx_option_convention>(data);
}

}
