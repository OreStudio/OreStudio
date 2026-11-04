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
#ifndef ORES_REPORTING_API_DOMAIN_ENGINE_SETTINGS_BUILDING_CODEC_HPP
#define ORES_REPORTING_API_DOMAIN_ENGINE_SETTINGS_BUILDING_CODEC_HPP

#include "ores.reporting.api/domain/engine_settings_building.hpp"
#include "ores.reporting.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::reporting::domain {

/**
 * @brief The members of engine_settings_building, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const engine_settings_building&) {
    return std::tuple{
        utility::codec::field{"version", &engine_settings_building::version},
        utility::codec::field{"tenant_id", &engine_settings_building::tenant_id},
        utility::codec::field{"id", &engine_settings_building::id},
        utility::codec::field{"engine_settings_id", &engine_settings_building::engine_settings_id},
        utility::codec::field{"party_id", &engine_settings_building::party_id},
        utility::codec::field{"observation_model", &engine_settings_building::observation_model},
        utility::codec::field{"lazy_market_building",
                              &engine_settings_building::lazy_market_building},
        utility::codec::field{"build_failed_trades",
                              &engine_settings_building::build_failed_trades},
        utility::codec::field{"continue_on_error", &engine_settings_building::continue_on_error},
        utility::codec::field{"use_at_par_coupons_curves",
                              &engine_settings_building::use_at_par_coupons_curves},
        utility::codec::field{"use_at_par_coupons_trades",
                              &engine_settings_building::use_at_par_coupons_trades},
        utility::codec::field{"modified_by", &engine_settings_building::modified_by},
        utility::codec::field{"performed_by", &engine_settings_building::performed_by},
        utility::codec::field{"change_reason_code", &engine_settings_building::change_reason_code},
        utility::codec::field{"change_commentary", &engine_settings_building::change_commentary},
        utility::codec::field{"recorded_at", &engine_settings_building::recorded_at},
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
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::domain::engine_settings_building> {
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

    static std::string write_json(const ores::reporting::domain::engine_settings_building& v);

    static ores::reporting::domain::engine_settings_building read_json(std::string_view text);

    static std::string write_msgpack(const ores::reporting::domain::engine_settings_building& v);

    static ores::reporting::domain::engine_settings_building
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
