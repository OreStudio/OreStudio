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
#include "ores.dq.api/domain/badge_mapping_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of badge_mapping, named as the wire names them and in
 * the order the type declares them.
 */
constexpr auto badge_mapping_codec_fields = std::tuple{
    utility::codec::field{"version", &badge_mapping::version},
    utility::codec::field{"tenant_id", &badge_mapping::tenant_id},
    utility::codec::field{"code_domain_code", &badge_mapping::code_domain_code},
    utility::codec::field{"entity_code", &badge_mapping::entity_code},
    utility::codec::field{"badge_code", &badge_mapping::badge_code},
    utility::codec::field{"modified_by", &badge_mapping::modified_by},
    utility::codec::field{"performed_by", &badge_mapping::performed_by},
    utility::codec::field{"change_reason_code", &badge_mapping::change_reason_code},
    utility::codec::field{"change_commentary", &badge_mapping::change_commentary},
    utility::codec::field{"recorded_at", &badge_mapping::recorded_at},
};

}

std::string badge_mapping_codec::to_json(const badge_mapping& v) {
    return utility::codec::object::write_json(v, badge_mapping_codec_fields);
}

badge_mapping badge_mapping_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<badge_mapping>(text, badge_mapping_codec_fields);
}

std::string badge_mapping_codec::to_msgpack(const badge_mapping& v) {
    return utility::codec::object::write_msgpack(v, badge_mapping_codec_fields);
}

badge_mapping badge_mapping_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<badge_mapping>(data, badge_mapping_codec_fields);
}

}
