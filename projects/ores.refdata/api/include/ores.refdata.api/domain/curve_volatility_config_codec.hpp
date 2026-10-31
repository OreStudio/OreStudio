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
#ifndef ORES_REFDATA_API_DOMAIN_CURVE_VOLATILITY_CONFIG_CODEC_HPP
#define ORES_REFDATA_API_DOMAIN_CURVE_VOLATILITY_CONFIG_CODEC_HPP

#include "ores.refdata.api/domain/curve_volatility_config.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::refdata::domain {

/**
 * @brief The members of curve_volatility_config, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_volatility_config&) {
    return std::tuple{
        utility::codec::field{"version", &curve_volatility_config::version},
        utility::codec::field{"tenant_id", &curve_volatility_config::tenant_id},
        utility::codec::field{"id", &curve_volatility_config::id},
        utility::codec::field{"party_id", &curve_volatility_config::party_id},
        utility::codec::field{"curve_definition_id", &curve_volatility_config::curve_definition_id},
        utility::codec::field{"kind", &curve_volatility_config::kind},
        utility::codec::field{"is_wrapped", &curve_volatility_config::is_wrapped},
        utility::codec::field{"priority", &curve_volatility_config::priority},
        utility::codec::field{"quote_type", &curve_volatility_config::quote_type},
        utility::codec::field{"volatility_type", &curve_volatility_config::volatility_type},
        utility::codec::field{"exercise_type", &curve_volatility_config::exercise_type},
        utility::codec::field{"strikes", &curve_volatility_config::strikes},
        utility::codec::field{"expiries", &curve_volatility_config::expiries},
        utility::codec::field{"time_interpolation", &curve_volatility_config::time_interpolation},
        utility::codec::field{"strike_interpolation",
                              &curve_volatility_config::strike_interpolation},
        utility::codec::field{"extrapolation", &curve_volatility_config::extrapolation},
        utility::codec::field{"time_extrapolation", &curve_volatility_config::time_extrapolation},
        utility::codec::field{"time_extrapolation_variance",
                              &curve_volatility_config::time_extrapolation_variance},
        utility::codec::field{"strike_extrapolation",
                              &curve_volatility_config::strike_extrapolation},
        utility::codec::field{"calendar", &curve_volatility_config::calendar},
        utility::codec::field{"quote", &curve_volatility_config::quote},
        utility::codec::field{"interpolation", &curve_volatility_config::interpolation},
        utility::codec::field{"enforce_monotone_variance",
                              &curve_volatility_config::enforce_monotone_variance},
        utility::codec::field{"delta_type", &curve_volatility_config::delta_type},
        utility::codec::field{"atm_type", &curve_volatility_config::atm_type},
        utility::codec::field{"atm_delta_type", &curve_volatility_config::atm_delta_type},
        utility::codec::field{"put_deltas", &curve_volatility_config::put_deltas},
        utility::codec::field{"call_deltas", &curve_volatility_config::call_deltas},
        utility::codec::field{"future_price_correction",
                              &curve_volatility_config::future_price_correction},
        utility::codec::field{"proxy_volatility_curve",
                              &curve_volatility_config::proxy_volatility_curve},
        utility::codec::field{"fx_volatility_curve", &curve_volatility_config::fx_volatility_curve},
        utility::codec::field{"correlation_curve", &curve_volatility_config::correlation_curve},
        utility::codec::field{"cds_volatility_curve",
                              &curve_volatility_config::cds_volatility_curve},
        utility::codec::field{"position", &curve_volatility_config::position},
        utility::codec::field{"modified_by", &curve_volatility_config::modified_by},
        utility::codec::field{"performed_by", &curve_volatility_config::performed_by},
        utility::codec::field{"change_reason_code", &curve_volatility_config::change_reason_code},
        utility::codec::field{"change_commentary", &curve_volatility_config::change_commentary},
        utility::codec::field{"recorded_at", &curve_volatility_config::recorded_at},
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
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::domain::curve_volatility_config> {
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

    static std::string write_json(const ores::refdata::domain::curve_volatility_config& v);

    static ores::refdata::domain::curve_volatility_config read_json(std::string_view text);

    static std::string write_msgpack(const ores::refdata::domain::curve_volatility_config& v);

    static ores::refdata::domain::curve_volatility_config
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
