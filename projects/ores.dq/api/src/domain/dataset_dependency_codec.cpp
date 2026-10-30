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
#include "ores.dq.api/domain/dataset_dependency_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"

namespace ores::dq::domain {

std::string dataset_dependency_codec::to_json(const dataset_dependency& v) {
    return utility::codec::object::write_json(v);
}

dataset_dependency dataset_dependency_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<dataset_dependency>(text);
}

std::string dataset_dependency_codec::to_msgpack(const dataset_dependency& v) {
    return utility::codec::object::write_msgpack(v);
}

dataset_dependency dataset_dependency_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<dataset_dependency>(data);
}

}
