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
 * Template: cpp_domain_type_codec.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_DQ_API_DOMAIN_LEI_RELATIONSHIP_CODEC_HPP
#define ORES_DQ_API_DOMAIN_LEI_RELATIONSHIP_CODEC_HPP

#include "ores.dq.api/domain/lei_relationship.hpp"
#include "ores.dq.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::dq::domain {

/**
 * @brief The wire form of lei_relationship, in each format the wire states.
 *
 * The four functions are this type's own: an overload set in the domain
 * namespace would collide on the read direction, where two types differ only
 * in what they return, so they are named here instead.
 */
struct ORES_DQ_API_EXPORT lei_relationship_codec final {
    /**
     * @brief Writes @p v as JSON: an object whose members are the members of
     * the type.
     *
     * The bytes are reflect-cpp's for the same value, because this states the
     * encoding that reflector derives, and the type's own test compares the
     * two.
     */
    static std::string to_json(const lei_relationship& v);

    /**
     * @brief Reads a lei_relationship back out of its JSON form.
     */
    static lei_relationship from_json(std::string_view text);

    /**
     * @brief Writes @p v as MessagePack: a map whose keys are the members of
     * the type.
     */
    static std::string to_msgpack(const lei_relationship& v);

    /**
     * @brief Reads a lei_relationship back out of its MessagePack form.
     */
    static lei_relationship from_msgpack(std::span<const std::byte> data);
};

/**
 * @brief The members of lei_relationship, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const lei_relationship&) {
    return std::tuple{
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

}

namespace ores::utility::codec {

/**
 * @brief The generated codec, which is how the wire codec reaches this type.
 *
 * A translation unit that only sends the type includes this header and is
 * done: the wire codec names wire_traits, and this states what it resolves to
 * here.
 */
template <>
struct wire_traits<ores::dq::domain::lei_relationship> {
    /**
     * @brief States that this is a generated codec.
     *
     * A test asserts this rather than the encoding, because the encoding is
     * what the type's own case already compares with the reflector: what it
     * cannot see is whether the seam resolved to this specialisation or fell
     * through to the primary template, and a marker that only this
     * specialisation declares is that answer at compile time.
     */
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::domain::lei_relationship& v) {
        return ores::dq::domain::lei_relationship_codec::to_json(v);
    }

    static ores::dq::domain::lei_relationship read_json(std::string_view text) {
        return ores::dq::domain::lei_relationship_codec::from_json(text);
    }

    static std::string write_msgpack(const ores::dq::domain::lei_relationship& v) {
        return ores::dq::domain::lei_relationship_codec::to_msgpack(v);
    }

    static ores::dq::domain::lei_relationship read_msgpack(std::span<const std::byte> data) {
        return ores::dq::domain::lei_relationship_codec::from_msgpack(data);
    }
};

}

#endif
