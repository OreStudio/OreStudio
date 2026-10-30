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
#include "ores.dq.api/domain/methodology_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of methodology, named as the wire names them and
 * in the order the type declares them.
 */
constexpr auto methodology_codec_fields = std::tuple{
    utility::codec::field{"version", &methodology::version},
    utility::codec::field{"tenant_id", &methodology::tenant_id},
    utility::codec::field{"id", &methodology::id},
    utility::codec::field{"name", &methodology::name},
    utility::codec::field{"description", &methodology::description},
    utility::codec::field{"logic_reference", &methodology::logic_reference},
    utility::codec::field{"implementation_details", &methodology::implementation_details},
    utility::codec::field{"modified_by", &methodology::modified_by},
    utility::codec::field{"performed_by", &methodology::performed_by},
    utility::codec::field{"change_reason_code", &methodology::change_reason_code},
    utility::codec::field{"change_commentary", &methodology::change_commentary},
    utility::codec::field{"recorded_at", &methodology::recorded_at},
};

}

std::string methodology_codec::to_json(const methodology& v) {
    return utility::codec::object::write_json(v, methodology_codec_fields);
}

methodology methodology_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<methodology>(text, methodology_codec_fields);
}

std::string methodology_codec::to_msgpack(const methodology& v) {
    return utility::codec::object::write_msgpack(v, methodology_codec_fields);
}

methodology methodology_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<methodology>(data, methodology_codec_fields);
}

}
