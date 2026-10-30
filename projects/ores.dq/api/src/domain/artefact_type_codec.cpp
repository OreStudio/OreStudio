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
#include "ores.dq.api/domain/artefact_type_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of artefact_type, named as the wire names them and
 * in the order the type declares them.
 */
constexpr auto artefact_type_codec_fields = std::tuple{
    utility::codec::field{"version", &artefact_type::version},
    utility::codec::field{"tenant_id", &artefact_type::tenant_id},
    utility::codec::field{"code", &artefact_type::code},
    utility::codec::field{"name", &artefact_type::name},
    utility::codec::field{"description", &artefact_type::description},
    utility::codec::field{"artefact_table", &artefact_type::artefact_table},
    utility::codec::field{"target_table", &artefact_type::target_table},
    utility::codec::field{"target_subject", &artefact_type::target_subject},
    utility::codec::field{"display_order", &artefact_type::display_order},
    utility::codec::field{"modified_by", &artefact_type::modified_by},
    utility::codec::field{"performed_by", &artefact_type::performed_by},
    utility::codec::field{"change_reason_code", &artefact_type::change_reason_code},
    utility::codec::field{"change_commentary", &artefact_type::change_commentary},
    utility::codec::field{"recorded_at", &artefact_type::recorded_at},
};

}

std::string artefact_type_codec::to_json(const artefact_type& v) {
    return utility::codec::object::write_json(v, artefact_type_codec_fields);
}

artefact_type artefact_type_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<artefact_type>(text, artefact_type_codec_fields);
}

std::string artefact_type_codec::to_msgpack(const artefact_type& v) {
    return utility::codec::object::write_msgpack(v, artefact_type_codec_fields);
}

artefact_type artefact_type_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<artefact_type>(data, artefact_type_codec_fields);
}

}
