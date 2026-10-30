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
#include "ores.dq.api/domain/subject_area_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of subject_area, named as the wire names them and
 * in the order the type declares them.
 */
constexpr auto subject_area_codec_fields = std::tuple{
    utility::codec::field{"version", &subject_area::version},
    utility::codec::field{"tenant_id", &subject_area::tenant_id},
    utility::codec::field{"name", &subject_area::name},
    utility::codec::field{"domain_name", &subject_area::domain_name},
    utility::codec::field{"description", &subject_area::description},
    utility::codec::field{"modified_by", &subject_area::modified_by},
    utility::codec::field{"performed_by", &subject_area::performed_by},
    utility::codec::field{"change_reason_code", &subject_area::change_reason_code},
    utility::codec::field{"change_commentary", &subject_area::change_commentary},
    utility::codec::field{"recorded_at", &subject_area::recorded_at},
};

}

std::string subject_area_codec::to_json(const subject_area& v) {
    return utility::codec::object::write_json(v, subject_area_codec_fields);
}

subject_area subject_area_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<subject_area>(text, subject_area_codec_fields);
}

std::string subject_area_codec::to_msgpack(const subject_area& v) {
    return utility::codec::object::write_msgpack(v, subject_area_codec_fields);
}

subject_area subject_area_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<subject_area>(data, subject_area_codec_fields);
}

}
