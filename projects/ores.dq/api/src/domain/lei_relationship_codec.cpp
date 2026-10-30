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
#include "ores.dq.api/domain/lei_relationship_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of lei_relationship, named as the wire names them and
 * in the order the type declares them.
 */
constexpr auto lei_relationship_codec_fields = std::tuple{
    utility::codec::field{"tenant_id", &lei_relationship::tenant_id},
    utility::codec::field{"relationship_start_node_node_id",
                          &lei_relationship::relationship_start_node_node_id},
    utility::codec::field{"relationship_start_node_node_id_type",
                          &lei_relationship::relationship_start_node_node_id_type},
    utility::codec::field{"relationship_end_node_node_id",
                          &lei_relationship::relationship_end_node_node_id},
    utility::codec::field{"relationship_end_node_node_id_type",
                          &lei_relationship::relationship_end_node_node_id_type},
    utility::codec::field{"relationship_relationship_type",
                          &lei_relationship::relationship_relationship_type},
    utility::codec::field{"relationship_relationship_status",
                          &lei_relationship::relationship_relationship_status},
    utility::codec::field{"relationship_period_1_start_date",
                          &lei_relationship::relationship_period_1_start_date},
    utility::codec::field{"relationship_period_1_end_date",
                          &lei_relationship::relationship_period_1_end_date},
    utility::codec::field{"registration_initial_registration_date",
                          &lei_relationship::registration_initial_registration_date},
    utility::codec::field{"registration_last_update_date",
                          &lei_relationship::registration_last_update_date},
    utility::codec::field{"registration_registration_status",
                          &lei_relationship::registration_registration_status},
    utility::codec::field{"registration_validation_sources",
                          &lei_relationship::registration_validation_sources},
};

}

std::string lei_relationship_codec::to_json(const lei_relationship& v) {
    return utility::codec::object::write_json(v, lei_relationship_codec_fields);
}

lei_relationship lei_relationship_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<lei_relationship>(text, lei_relationship_codec_fields);
}

std::string lei_relationship_codec::to_msgpack(const lei_relationship& v) {
    return utility::codec::object::write_msgpack(v, lei_relationship_codec_fields);
}

lei_relationship lei_relationship_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<lei_relationship>(data,
                                                                  lei_relationship_codec_fields);
}

}
