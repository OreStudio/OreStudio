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
#include "ores.dq.api/domain/nature_dimension_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of nature_dimension, named as the wire names them and
 * in the order the type declares them.
 */
constexpr auto nature_dimension_codec_fields = std::tuple{
    utility::codec::field{"version", &nature_dimension::version},
    utility::codec::field{"tenant_id", &nature_dimension::tenant_id},
    utility::codec::field{"code", &nature_dimension::code},
    utility::codec::field{"name", &nature_dimension::name},
    utility::codec::field{"description", &nature_dimension::description},
    utility::codec::field{"modified_by", &nature_dimension::modified_by},
    utility::codec::field{"performed_by", &nature_dimension::performed_by},
    utility::codec::field{"change_reason_code", &nature_dimension::change_reason_code},
    utility::codec::field{"change_commentary", &nature_dimension::change_commentary},
    utility::codec::field{"recorded_at", &nature_dimension::recorded_at},
};

}

std::string nature_dimension_codec::to_json(const nature_dimension& v) {
    return utility::codec::object::write_json(v, nature_dimension_codec_fields);
}

nature_dimension nature_dimension_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<nature_dimension>(text, nature_dimension_codec_fields);
}

std::string nature_dimension_codec::to_msgpack(const nature_dimension& v) {
    return utility::codec::object::write_msgpack(v, nature_dimension_codec_fields);
}

nature_dimension nature_dimension_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<nature_dimension>(data,
                                                                  nature_dimension_codec_fields);
}

}
