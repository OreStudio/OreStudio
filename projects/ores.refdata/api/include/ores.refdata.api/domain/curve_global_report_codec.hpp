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
#ifndef ORES_REFDATA_API_DOMAIN_CURVE_GLOBAL_REPORT_CODEC_HPP
#define ORES_REFDATA_API_DOMAIN_CURVE_GLOBAL_REPORT_CODEC_HPP

#include "ores.refdata.api/domain/curve_global_report.hpp"
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
 * @brief The members of curve_global_report, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_global_report&) {
    return std::tuple{
        utility::codec::field{"version", &curve_global_report::version},
        utility::codec::field{"tenant_id", &curve_global_report::tenant_id},
        utility::codec::field{"id", &curve_global_report::id},
        utility::codec::field{"party_id", &curve_global_report::party_id},
        utility::codec::field{"curve_configuration_id",
                              &curve_global_report::curve_configuration_id},
        utility::codec::field{"family", &curve_global_report::family},
        utility::codec::field{"has_report", &curve_global_report::has_report},
        utility::codec::field{"report_on_delta_grid", &curve_global_report::report_on_delta_grid},
        utility::codec::field{"report_on_moneyness_grid",
                              &curve_global_report::report_on_moneyness_grid},
        utility::codec::field{"report_on_strike_grid", &curve_global_report::report_on_strike_grid},
        utility::codec::field{"report_on_strike_spread_grid",
                              &curve_global_report::report_on_strike_spread_grid},
        utility::codec::field{"deltas", &curve_global_report::deltas},
        utility::codec::field{"moneyness", &curve_global_report::moneyness},
        utility::codec::field{"strikes", &curve_global_report::strikes},
        utility::codec::field{"strike_spreads", &curve_global_report::strike_spreads},
        utility::codec::field{"expiries", &curve_global_report::expiries},
        utility::codec::field{"pillar_dates", &curve_global_report::pillar_dates},
        utility::codec::field{"underlying_tenors", &curve_global_report::underlying_tenors},
        utility::codec::field{"continuation_expiry", &curve_global_report::continuation_expiry},
        utility::codec::field{"position", &curve_global_report::position},
        utility::codec::field{"modified_by", &curve_global_report::modified_by},
        utility::codec::field{"performed_by", &curve_global_report::performed_by},
        utility::codec::field{"change_reason_code", &curve_global_report::change_reason_code},
        utility::codec::field{"change_commentary", &curve_global_report::change_commentary},
        utility::codec::field{"recorded_at", &curve_global_report::recorded_at},
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
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::domain::curve_global_report> {
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

    static std::string write_json(const ores::refdata::domain::curve_global_report& v);

    static ores::refdata::domain::curve_global_report read_json(std::string_view text);

    static std::string write_msgpack(const ores::refdata::domain::curve_global_report& v);

    static ores::refdata::domain::curve_global_report read_msgpack(std::span<const std::byte> data);
};

}

#endif
