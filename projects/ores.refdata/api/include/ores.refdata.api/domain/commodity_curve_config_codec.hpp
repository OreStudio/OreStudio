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
#ifndef ORES_REFDATA_API_DOMAIN_COMMODITY_CURVE_CONFIG_CODEC_HPP
#define ORES_REFDATA_API_DOMAIN_COMMODITY_CURVE_CONFIG_CODEC_HPP

#include "ores.refdata.api/domain/commodity_curve_config.hpp"
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
 * @brief The members of commodity_curve_config, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_curve_config&) {
    return std::tuple{
        utility::codec::field{"version", &commodity_curve_config::version},
        utility::codec::field{"tenant_id", &commodity_curve_config::tenant_id},
        utility::codec::field{"id", &commodity_curve_config::id},
        utility::codec::field{"party_id", &commodity_curve_config::party_id},
        utility::codec::field{"curve_definition_id", &commodity_curve_config::curve_definition_id},
        utility::codec::field{"currency", &commodity_curve_config::currency},
        utility::codec::field{"base_price_curve", &commodity_curve_config::base_price_curve},
        utility::codec::field{"base_yield_curve", &commodity_curve_config::base_yield_curve},
        utility::codec::field{"yield_curve", &commodity_curve_config::yield_curve},
        utility::codec::field{"spot_quote", &commodity_curve_config::spot_quote},
        utility::codec::field{"has_quotes", &commodity_curve_config::has_quotes},
        utility::codec::field{"day_counter", &commodity_curve_config::day_counter},
        utility::codec::field{"interpolation_method",
                              &commodity_curve_config::interpolation_method},
        utility::codec::field{"conventions", &commodity_curve_config::conventions},
        utility::codec::field{"extrapolation", &commodity_curve_config::extrapolation},
        utility::codec::field{"has_basis_configuration",
                              &commodity_curve_config::has_basis_configuration},
        utility::codec::field{"basis_base_price_curve",
                              &commodity_curve_config::basis_base_price_curve},
        utility::codec::field{"basis_base_price_conventions",
                              &commodity_curve_config::basis_base_price_conventions},
        utility::codec::field{"basis_conventions", &commodity_curve_config::basis_conventions},
        utility::codec::field{"basis_day_counter", &commodity_curve_config::basis_day_counter},
        utility::codec::field{"basis_interpolation_method",
                              &commodity_curve_config::basis_interpolation_method},
        utility::codec::field{"basis_add_basis", &commodity_curve_config::basis_add_basis},
        utility::codec::field{"basis_month_offset", &commodity_curve_config::basis_month_offset},
        utility::codec::field{"basis_average_base", &commodity_curve_config::basis_average_base},
        utility::codec::field{"basis_price_as_historical_fixing",
                              &commodity_curve_config::basis_price_as_historical_fixing},
        utility::codec::field{"has_price_segments", &commodity_curve_config::has_price_segments},
        utility::codec::field{"modified_by", &commodity_curve_config::modified_by},
        utility::codec::field{"performed_by", &commodity_curve_config::performed_by},
        utility::codec::field{"change_reason_code", &commodity_curve_config::change_reason_code},
        utility::codec::field{"change_commentary", &commodity_curve_config::change_commentary},
        utility::codec::field{"recorded_at", &commodity_curve_config::recorded_at},
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
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::domain::commodity_curve_config> {
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

    static std::string write_json(const ores::refdata::domain::commodity_curve_config& v);

    static ores::refdata::domain::commodity_curve_config read_json(std::string_view text);

    static std::string write_msgpack(const ores::refdata::domain::commodity_curve_config& v);

    static ores::refdata::domain::commodity_curve_config
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
