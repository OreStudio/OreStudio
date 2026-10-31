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
#ifndef ORES_REFDATA_API_DOMAIN_DEFAULT_CURVE_CONFIGURATION_CODEC_HPP
#define ORES_REFDATA_API_DOMAIN_DEFAULT_CURVE_CONFIGURATION_CODEC_HPP

#include "ores.refdata.api/domain/default_curve_configuration.hpp"
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
 * @brief The members of default_curve_configuration, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const default_curve_configuration&) {
    return std::tuple{
        utility::codec::field{"version", &default_curve_configuration::version},
        utility::codec::field{"tenant_id", &default_curve_configuration::tenant_id},
        utility::codec::field{"id", &default_curve_configuration::id},
        utility::codec::field{"party_id", &default_curve_configuration::party_id},
        utility::codec::field{"curve_definition_id",
                              &default_curve_configuration::curve_definition_id},
        utility::codec::field{"is_inline", &default_curve_configuration::is_inline},
        utility::codec::field{"priority", &default_curve_configuration::priority},
        utility::codec::field{"default_curve_type",
                              &default_curve_configuration::default_curve_type},
        utility::codec::field{"discount_curve", &default_curve_configuration::discount_curve},
        utility::codec::field{"day_counter", &default_curve_configuration::day_counter},
        utility::codec::field{"recovery_rate", &default_curve_configuration::recovery_rate},
        utility::codec::field{"start_date", &default_curve_configuration::start_date},
        utility::codec::field{"has_quotes", &default_curve_configuration::has_quotes},
        utility::codec::field{"benchmark_curve", &default_curve_configuration::benchmark_curve},
        utility::codec::field{"reinterpreted_yield_curve",
                              &default_curve_configuration::reinterpreted_yield_curve},
        utility::codec::field{"source_curve", &default_curve_configuration::source_curve},
        utility::codec::field{"pillars", &default_curve_configuration::pillars},
        utility::codec::field{"spot_lag", &default_curve_configuration::spot_lag},
        utility::codec::field{"calendar", &default_curve_configuration::calendar},
        utility::codec::field{"conventions", &default_curve_configuration::conventions},
        utility::codec::field{"extrapolation", &default_curve_configuration::extrapolation},
        utility::codec::field{"running_spread", &default_curve_configuration::running_spread},
        utility::codec::field{"index_term", &default_curve_configuration::index_term},
        utility::codec::field{"imply_default_from_market",
                              &default_curve_configuration::imply_default_from_market},
        utility::codec::field{"allow_negative_rates",
                              &default_curve_configuration::allow_negative_rates},
        utility::codec::field{"price_is_upfront", &default_curve_configuration::price_is_upfront},
        utility::codec::field{"initial_state", &default_curve_configuration::initial_state},
        utility::codec::field{"states", &default_curve_configuration::states},
        utility::codec::field{"position", &default_curve_configuration::position},
        utility::codec::field{"modified_by", &default_curve_configuration::modified_by},
        utility::codec::field{"performed_by", &default_curve_configuration::performed_by},
        utility::codec::field{"change_reason_code",
                              &default_curve_configuration::change_reason_code},
        utility::codec::field{"change_commentary", &default_curve_configuration::change_commentary},
        utility::codec::field{"recorded_at", &default_curve_configuration::recorded_at},
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
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::domain::default_curve_configuration> {
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

    static std::string write_json(const ores::refdata::domain::default_curve_configuration& v);

    static ores::refdata::domain::default_curve_configuration read_json(std::string_view text);

    static std::string write_msgpack(const ores::refdata::domain::default_curve_configuration& v);

    static ores::refdata::domain::default_curve_configuration
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
