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
#ifndef ORES_MARKETDATA_API_DOMAIN_FX_VOL_SURFACE_CALIBRATION_CODEC_HPP
#define ORES_MARKETDATA_API_DOMAIN_FX_VOL_SURFACE_CALIBRATION_CODEC_HPP

#include "ores.marketdata.api/domain/fx_vol_surface_calibration.hpp"
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
 * @brief The members of fx_vol_surface_calibration, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_surface_calibration>) {
    return std::tuple{
        utility::codec::field{"version", &fx_vol_surface_calibration::version},
        utility::codec::field{"tenant_id", &fx_vol_surface_calibration::tenant_id},
        utility::codec::field{"id", &fx_vol_surface_calibration::id},
        utility::codec::field{"blueprint_id", &fx_vol_surface_calibration::blueprint_id},
        utility::codec::field{"as_of", &fx_vol_surface_calibration::as_of},
        utility::codec::field{"party_id", &fx_vol_surface_calibration::party_id},
        utility::codec::field{"blueprint_version", &fx_vol_surface_calibration::blueprint_version},
        utility::codec::field{"output_series_id", &fx_vol_surface_calibration::output_series_id},
        utility::codec::field{"forward_publish_id",
                              &fx_vol_surface_calibration::forward_publish_id},
        utility::codec::field{"smile_model", &fx_vol_surface_calibration::smile_model},
        utility::codec::field{"rho", &fx_vol_surface_calibration::rho},
        utility::codec::field{"eta", &fx_vol_surface_calibration::eta},
        utility::codec::field{"gamma", &fx_vol_surface_calibration::gamma},
        utility::codec::field{"converged", &fx_vol_surface_calibration::converged},
        utility::codec::field{"max_abs_error", &fx_vol_surface_calibration::max_abs_error},
        utility::codec::field{"pillars_beyond_tolerance",
                              &fx_vol_surface_calibration::pillars_beyond_tolerance},
        utility::codec::field{"pillar_fits", &fx_vol_surface_calibration::pillar_fits},
        utility::codec::field{"violations", &fx_vol_surface_calibration::violations},
        utility::codec::field{"missing_pillars", &fx_vol_surface_calibration::missing_pillars},
        utility::codec::field{"modified_by", &fx_vol_surface_calibration::modified_by},
        utility::codec::field{"performed_by", &fx_vol_surface_calibration::performed_by},
        utility::codec::field{"change_reason_code",
                              &fx_vol_surface_calibration::change_reason_code},
        utility::codec::field{"change_commentary", &fx_vol_surface_calibration::change_commentary},
        utility::codec::field{"recorded_at", &fx_vol_surface_calibration::recorded_at},
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
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::domain::fx_vol_surface_calibration> {
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

    static std::string write_json(const ores::marketdata::domain::fx_vol_surface_calibration& v);

    static ores::marketdata::domain::fx_vol_surface_calibration read_json(std::string_view text);

    static std::string write_msgpack(const ores::marketdata::domain::fx_vol_surface_calibration& v);

    static ores::marketdata::domain::fx_vol_surface_calibration
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
