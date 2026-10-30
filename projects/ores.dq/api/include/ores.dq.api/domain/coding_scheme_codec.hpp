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
#ifndef ORES_DQ_API_DOMAIN_CODING_SCHEME_CODEC_HPP
#define ORES_DQ_API_DOMAIN_CODING_SCHEME_CODEC_HPP

#include "ores.dq.api/domain/coding_scheme.hpp"
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
 * @brief The wire form of coding_scheme, in each format the wire states.
 *
 * The four functions are this type's own: an overload set in the domain
 * namespace would collide on the read direction, where two types differ only
 * in what they return, so they are named here instead.
 */
struct ORES_DQ_API_EXPORT coding_scheme_codec final {
    /**
     * @brief Writes @p v as JSON: an object whose members are the members of
     * the type.
     *
     * The bytes are reflect-cpp's for the same value, because this states the
     * encoding that reflector derives, and the type's own test compares the
     * two.
     */
    static std::string to_json(const coding_scheme& v);

    /**
     * @brief Reads a coding_scheme back out of its JSON form.
     */
    static coding_scheme from_json(std::string_view text);

    /**
     * @brief Writes @p v as MessagePack: a map whose keys are the members of
     * the type.
     */
    static std::string to_msgpack(const coding_scheme& v);

    /**
     * @brief Reads a coding_scheme back out of its MessagePack form.
     */
    static coding_scheme from_msgpack(std::span<const std::byte> data);
};

/**
 * @brief The members of coding_scheme, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const coding_scheme&) {
    return std::tuple{
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
struct wire_traits<ores::dq::domain::coding_scheme> {
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

    static std::string write_json(const ores::dq::domain::coding_scheme& v) {
        return ores::dq::domain::coding_scheme_codec::to_json(v);
    }

    static ores::dq::domain::coding_scheme read_json(std::string_view text) {
        return ores::dq::domain::coding_scheme_codec::from_json(text);
    }

    static std::string write_msgpack(const ores::dq::domain::coding_scheme& v) {
        return ores::dq::domain::coding_scheme_codec::to_msgpack(v);
    }

    static ores::dq::domain::coding_scheme read_msgpack(std::span<const std::byte> data) {
        return ores::dq::domain::coding_scheme_codec::from_msgpack(data);
    }
};

}

#endif
