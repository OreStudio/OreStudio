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
#ifndef ORES_MARKETDATA_API_DOMAIN_FX_FORWARD_PUBLISH_POINT_CODEC_HPP
#define ORES_MARKETDATA_API_DOMAIN_FX_FORWARD_PUBLISH_POINT_CODEC_HPP

#include "ores.marketdata.api/domain/fx_forward_publish_point.hpp"
#include "ores.marketdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::marketdata::domain {

/**
 * @brief The members of fx_forward_publish_point, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_forward_publish_point>) {
    return std::tuple{
        utility::codec::field{"version", &fx_forward_publish_point::version},
        utility::codec::field{"tenant_id", &fx_forward_publish_point::tenant_id},
        utility::codec::field{"id", &fx_forward_publish_point::id},
        utility::codec::field{"publish_id", &fx_forward_publish_point::publish_id},
        utility::codec::field{"sequence_index", &fx_forward_publish_point::sequence_index},
        utility::codec::field{"tenor_code", &fx_forward_publish_point::tenor_code},
        utility::codec::field{"near_date", &fx_forward_publish_point::near_date},
        utility::codec::field{"value_date", &fx_forward_publish_point::value_date},
        utility::codec::field{"base_discount", &fx_forward_publish_point::base_discount},
        utility::codec::field{"quote_discount", &fx_forward_publish_point::quote_discount},
        utility::codec::field{"forward_rate", &fx_forward_publish_point::forward_rate},
        utility::codec::field{"forward_points", &fx_forward_publish_point::forward_points},
        utility::codec::field{"swap_points", &fx_forward_publish_point::swap_points},
        utility::codec::field{"modified_by", &fx_forward_publish_point::modified_by},
        utility::codec::field{"performed_by", &fx_forward_publish_point::performed_by},
        utility::codec::field{"change_reason_code", &fx_forward_publish_point::change_reason_code},
        utility::codec::field{"change_commentary", &fx_forward_publish_point::change_commentary},
        utility::codec::field{"recorded_at", &fx_forward_publish_point::recorded_at},
    };
}

}

namespace ores::utility::codec {

/**
 * @brief The generated codec: what the wire codec resolves to for this type.
 *
 * The functions are declared here and defined in this type's own translation
 * unit, so a translation unit that only sends the type compiles a declaration
 * rather than a second copy of the encoder.
 */
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::domain::fx_forward_publish_point> {
    /**
     * @brief States that this is a generated codec.
     *
     * A test asserts this rather than the encoding, because the encoding is
     * what the type's own case already round-trips: what it
     * cannot see is whether the seam resolved to this specialisation or fell
     * through to the primary template, and a marker that only this
     * specialisation declares is that answer at compile time.
     */
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::domain::fx_forward_publish_point& v);

    static ores::marketdata::domain::fx_forward_publish_point read_json(std::string_view text);

    static std::string write_msgpack(const ores::marketdata::domain::fx_forward_publish_point& v);

    static ores::marketdata::domain::fx_forward_publish_point
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
