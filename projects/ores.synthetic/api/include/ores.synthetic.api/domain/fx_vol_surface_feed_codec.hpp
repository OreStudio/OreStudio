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
#ifndef ORES_SYNTHETIC_API_DOMAIN_FX_VOL_SURFACE_FEED_CODEC_HPP
#define ORES_SYNTHETIC_API_DOMAIN_FX_VOL_SURFACE_FEED_CODEC_HPP

#include "ores.synthetic.api/domain/fx_vol_surface_feed.hpp"
#include "ores.synthetic.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::synthetic::domain {

/**
 * @brief The members of fx_vol_surface_feed, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_surface_feed>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &fx_vol_surface_feed::version},
        utility::codec::field{"tenant_id", &fx_vol_surface_feed::tenant_id},
        utility::codec::field{"id", &fx_vol_surface_feed::id},
        utility::codec::field{"party_id", &fx_vol_surface_feed::party_id},
        utility::codec::field{"config_id", &fx_vol_surface_feed::config_id},
        utility::codec::field{"base_currency_code", &fx_vol_surface_feed::base_currency_code},
        utility::codec::field{"quote_currency_code", &fx_vol_surface_feed::quote_currency_code},
        utility::codec::field{"source_name", &fx_vol_surface_feed::source_name},
        utility::codec::field{"ticks_per_hour", &fx_vol_surface_feed::ticks_per_hour},
        utility::codec::field{"enabled", &fx_vol_surface_feed::enabled},
        utility::codec::field{"auto_start", &fx_vol_surface_feed::auto_start},
        utility::codec::field{"folder_id", &fx_vol_surface_feed::folder_id},
        utility::codec::field{"asset_class_code", &fx_vol_surface_feed::asset_class_code},
        utility::codec::field{"arrival_mode", &fx_vol_surface_feed::arrival_mode},
        utility::codec::field{"branching_ratio", &fx_vol_surface_feed::branching_ratio},
        utility::codec::field{"decay", &fx_vol_surface_feed::decay},
        utility::codec::field{"sensitivity", &fx_vol_surface_feed::sensitivity},
        utility::codec::field{"skew_reversion", &fx_vol_surface_feed::skew_reversion},
        utility::codec::field{"skew_volatility", &fx_vol_surface_feed::skew_volatility},
        utility::codec::field{"curvature_reversion", &fx_vol_surface_feed::curvature_reversion},
        utility::codec::field{"curvature_volatility", &fx_vol_surface_feed::curvature_volatility},
        utility::codec::field{"spot_variance_correlation",
                              &fx_vol_surface_feed::spot_variance_correlation},
        utility::codec::field{"spot_skew_correlation", &fx_vol_surface_feed::spot_skew_correlation},
        utility::codec::field{"segment_correlation", &fx_vol_surface_feed::segment_correlation},
        utility::codec::field{"start_skew", &fx_vol_surface_feed::start_skew},
        utility::codec::field{"start_curvature", &fx_vol_surface_feed::start_curvature},
        utility::codec::field{"modified_by", &fx_vol_surface_feed::modified_by},
        utility::codec::field{"performed_by", &fx_vol_surface_feed::performed_by},
        utility::codec::field{"change_reason_code", &fx_vol_surface_feed::change_reason_code},
        utility::codec::field{"change_commentary", &fx_vol_surface_feed::change_commentary},
        utility::codec::field{"recorded_at", &fx_vol_surface_feed::recorded_at},
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
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::domain::fx_vol_surface_feed> {
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

    static std::string write_json(const ores::synthetic::domain::fx_vol_surface_feed& v);

    static ores::synthetic::domain::fx_vol_surface_feed read_json(std::string_view text);

    static std::string write_msgpack(const ores::synthetic::domain::fx_vol_surface_feed& v);

    static ores::synthetic::domain::fx_vol_surface_feed
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
