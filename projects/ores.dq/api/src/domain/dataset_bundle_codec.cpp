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
#include "ores.dq.api/domain/dataset_bundle_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of dataset_bundle, named as the wire names them and
 * in the order the type declares them.
 */
constexpr auto dataset_bundle_codec_fields = std::tuple{
    utility::codec::field{"version", &dataset_bundle::version},
    utility::codec::field{"tenant_id", &dataset_bundle::tenant_id},
    utility::codec::field{"id", &dataset_bundle::id},
    utility::codec::field{"code", &dataset_bundle::code},
    utility::codec::field{"name", &dataset_bundle::name},
    utility::codec::field{"description", &dataset_bundle::description},
    utility::codec::field{"modified_by", &dataset_bundle::modified_by},
    utility::codec::field{"performed_by", &dataset_bundle::performed_by},
    utility::codec::field{"change_reason_code", &dataset_bundle::change_reason_code},
    utility::codec::field{"change_commentary", &dataset_bundle::change_commentary},
    utility::codec::field{"recorded_at", &dataset_bundle::recorded_at},
};

}

std::string dataset_bundle_codec::to_json(const dataset_bundle& v) {
    return utility::codec::object::write_json(v, dataset_bundle_codec_fields);
}

dataset_bundle dataset_bundle_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<dataset_bundle>(text, dataset_bundle_codec_fields);
}

std::string dataset_bundle_codec::to_msgpack(const dataset_bundle& v) {
    return utility::codec::object::write_msgpack(v, dataset_bundle_codec_fields);
}

dataset_bundle dataset_bundle_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<dataset_bundle>(data, dataset_bundle_codec_fields);
}

}
