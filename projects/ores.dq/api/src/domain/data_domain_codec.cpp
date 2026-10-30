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
#include "ores.dq.api/domain/data_domain_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of data_domain, named as the wire names them and
 * in the order the type declares them.
 */
constexpr auto data_domain_codec_fields = std::tuple{
    utility::codec::field{"version", &data_domain::version},
    utility::codec::field{"tenant_id", &data_domain::tenant_id},
    utility::codec::field{"name", &data_domain::name},
    utility::codec::field{"description", &data_domain::description},
    utility::codec::field{"modified_by", &data_domain::modified_by},
    utility::codec::field{"performed_by", &data_domain::performed_by},
    utility::codec::field{"change_reason_code", &data_domain::change_reason_code},
    utility::codec::field{"change_commentary", &data_domain::change_commentary},
    utility::codec::field{"recorded_at", &data_domain::recorded_at},
};

}

std::string data_domain_codec::to_json(const data_domain& v) {
    return utility::codec::object::write_json(v, data_domain_codec_fields);
}

data_domain data_domain_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<data_domain>(text, data_domain_codec_fields);
}

std::string data_domain_codec::to_msgpack(const data_domain& v) {
    return utility::codec::object::write_msgpack(v, data_domain_codec_fields);
}

data_domain data_domain_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<data_domain>(data, data_domain_codec_fields);
}

}
