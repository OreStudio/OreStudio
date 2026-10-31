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
#ifndef ORES_REFDATA_API_DOMAIN_CURVE_SEGMENT_CODEC_HPP
#define ORES_REFDATA_API_DOMAIN_CURVE_SEGMENT_CODEC_HPP

#include "ores.refdata.api/domain/curve_segment.hpp"
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
 * @brief The members of curve_segment, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment&) {
    return std::tuple{
        utility::codec::field{"version", &curve_segment::version},
        utility::codec::field{"tenant_id", &curve_segment::tenant_id},
        utility::codec::field{"id", &curve_segment::id},
        utility::codec::field{"party_id", &curve_segment::party_id},
        utility::codec::field{"curve_definition_id", &curve_segment::curve_definition_id},
        utility::codec::field{"segment_type", &curve_segment::segment_type},
        utility::codec::field{"position", &curve_segment::position},
        utility::codec::field{"conventions", &curve_segment::conventions},
        utility::codec::field{"pillar_choice", &curve_segment::pillar_choice},
        utility::codec::field{"priority", &curve_segment::priority},
        utility::codec::field{"min_distance", &curve_segment::min_distance},
        utility::codec::field{"projection_curve", &curve_segment::projection_curve},
        utility::codec::field{"discount_curve", &curve_segment::discount_curve},
        utility::codec::field{"spot_rate", &curve_segment::spot_rate},
        utility::codec::field{"projection_curve_domestic",
                              &curve_segment::projection_curve_domestic},
        utility::codec::field{"projection_curve_foreign", &curve_segment::projection_curve_foreign},
        utility::codec::field{"projection_curve_pay", &curve_segment::projection_curve_pay},
        utility::codec::field{"projection_curve_receive", &curve_segment::projection_curve_receive},
        utility::codec::field{"projection_curve_long", &curve_segment::projection_curve_long},
        utility::codec::field{"projection_curve_short", &curve_segment::projection_curve_short},
        utility::codec::field{"reference_curve", &curve_segment::reference_curve},
        utility::codec::field{"reference_curve_2", &curve_segment::reference_curve_2},
        utility::codec::field{"weight_1", &curve_segment::weight_1},
        utility::codec::field{"weight_2", &curve_segment::weight_2},
        utility::codec::field{"ibor_index", &curve_segment::ibor_index},
        utility::codec::field{"rfr_curve", &curve_segment::rfr_curve},
        utility::codec::field{"rfr_index", &curve_segment::rfr_index},
        utility::codec::field{"spread", &curve_segment::spread},
        utility::codec::field{"base_curve", &curve_segment::base_curve},
        utility::codec::field{"base_curve_currency", &curve_segment::base_curve_currency},
        utility::codec::field{"numerator_curve", &curve_segment::numerator_curve},
        utility::codec::field{"numerator_curve_currency", &curve_segment::numerator_curve_currency},
        utility::codec::field{"denominator_curve", &curve_segment::denominator_curve},
        utility::codec::field{"denominator_curve_currency",
                              &curve_segment::denominator_curve_currency},
        utility::codec::field{"extrapolate_flat", &curve_segment::extrapolate_flat},
        utility::codec::field{"modified_by", &curve_segment::modified_by},
        utility::codec::field{"performed_by", &curve_segment::performed_by},
        utility::codec::field{"change_reason_code", &curve_segment::change_reason_code},
        utility::codec::field{"change_commentary", &curve_segment::change_commentary},
        utility::codec::field{"recorded_at", &curve_segment::recorded_at},
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
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::domain::curve_segment> {
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

    static std::string write_json(const ores::refdata::domain::curve_segment& v);

    static ores::refdata::domain::curve_segment read_json(std::string_view text);

    static std::string write_msgpack(const ores::refdata::domain::curve_segment& v);

    static ores::refdata::domain::curve_segment read_msgpack(std::span<const std::byte> data);
};

}

#endif
