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
#ifndef ORES_ANALYTICS_API_DOMAIN_PRICING_PAIR_SETTING_CODEC_HPP
#define ORES_ANALYTICS_API_DOMAIN_PRICING_PAIR_SETTING_CODEC_HPP

#include "ores.analytics.api/domain/pricing_pair_setting.hpp"
#include "ores.analytics.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::analytics::domain {

/**
 * @brief The members of pricing_pair_setting, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<pricing_pair_setting>) {
    return std::tuple{
        utility::codec::field{"version", &pricing_pair_setting::version},
        utility::codec::field{"tenant_id", &pricing_pair_setting::tenant_id},
        utility::codec::field{"code", &pricing_pair_setting::code},
        utility::codec::field{"model", &pricing_pair_setting::model},
        utility::codec::field{"engine", &pricing_pair_setting::engine},
        utility::codec::field{"pricing_engine_type_code",
                              &pricing_pair_setting::pricing_engine_type_code},
        utility::codec::field{"scope", &pricing_pair_setting::scope},
        utility::codec::field{"name", &pricing_pair_setting::name},
        utility::codec::field{"usual_value", &pricing_pair_setting::usual_value},
        utility::codec::field{"is_required", &pricing_pair_setting::is_required},
        utility::codec::field{"note", &pricing_pair_setting::note},
        utility::codec::field{"modified_by", &pricing_pair_setting::modified_by},
        utility::codec::field{"performed_by", &pricing_pair_setting::performed_by},
        utility::codec::field{"change_reason_code", &pricing_pair_setting::change_reason_code},
        utility::codec::field{"change_commentary", &pricing_pair_setting::change_commentary},
        utility::codec::field{"recorded_at", &pricing_pair_setting::recorded_at},
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
struct ORES_ANALYTICS_API_EXPORT wire_traits<ores::analytics::domain::pricing_pair_setting> {
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

    static std::string write_json(const ores::analytics::domain::pricing_pair_setting& v);

    static ores::analytics::domain::pricing_pair_setting read_json(std::string_view text);

    static std::string write_msgpack(const ores::analytics::domain::pricing_pair_setting& v);

    static ores::analytics::domain::pricing_pair_setting
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
