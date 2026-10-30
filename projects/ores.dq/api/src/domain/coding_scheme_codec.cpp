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
#include "ores.dq.api/domain/coding_scheme_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of coding_scheme, named as the wire names them and
 * in the order the type declares them.
 */
constexpr auto coding_scheme_codec_fields = std::tuple{
    utility::codec::field{"version", &coding_scheme::version},
    utility::codec::field{"tenant_id", &coding_scheme::tenant_id},
    utility::codec::field{"code", &coding_scheme::code},
    utility::codec::field{"name", &coding_scheme::name},
    utility::codec::field{"authority_type", &coding_scheme::authority_type},
    utility::codec::field{"subject_area_name", &coding_scheme::subject_area_name},
    utility::codec::field{"domain_name", &coding_scheme::domain_name},
    utility::codec::field{"uri", &coding_scheme::uri},
    utility::codec::field{"description", &coding_scheme::description},
    utility::codec::field{"modified_by", &coding_scheme::modified_by},
    utility::codec::field{"performed_by", &coding_scheme::performed_by},
    utility::codec::field{"change_reason_code", &coding_scheme::change_reason_code},
    utility::codec::field{"change_commentary", &coding_scheme::change_commentary},
    utility::codec::field{"recorded_at", &coding_scheme::recorded_at},
};

}

std::string coding_scheme_codec::to_json(const coding_scheme& v) {
    return utility::codec::object::write_json(v, coding_scheme_codec_fields);
}

coding_scheme coding_scheme_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<coding_scheme>(text, coding_scheme_codec_fields);
}

std::string coding_scheme_codec::to_msgpack(const coding_scheme& v) {
    return utility::codec::object::write_msgpack(v, coding_scheme_codec_fields);
}

coding_scheme coding_scheme_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<coding_scheme>(data, coding_scheme_codec_fields);
}

}
