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
#ifndef ORES_TRADING_API_DOMAIN_BOND_LEG_RATE_CODEC_HPP
#define ORES_TRADING_API_DOMAIN_BOND_LEG_RATE_CODEC_HPP

#include "ores.trading.api/domain/bond_leg_rate.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::trading::domain {

/**
 * @brief The members of bond_leg_rate, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_leg_rate&) {
    return std::tuple{
        utility::codec::field{"version", &bond_leg_rate::version},
        utility::codec::field{"tenant_id", &bond_leg_rate::tenant_id},
        utility::codec::field{"trade_id", &bond_leg_rate::trade_id},
        utility::codec::field{"leg_role", &bond_leg_rate::leg_role},
        utility::codec::field{"leg_number", &bond_leg_rate::leg_number},
        utility::codec::field{"trade_activity_id", &bond_leg_rate::trade_activity_id},
        utility::codec::field{"rate_kind", &bond_leg_rate::rate_kind},
        utility::codec::field{"index", &bond_leg_rate::index},
        utility::codec::field{"is_in_arrears", &bond_leg_rate::is_in_arrears},
        utility::codec::field{"fixing_days", &bond_leg_rate::fixing_days},
        utility::codec::field{"fixing_calendar", &bond_leg_rate::fixing_calendar},
        utility::codec::field{"last_recent_period", &bond_leg_rate::last_recent_period},
        utility::codec::field{"last_recent_period_calendar",
                              &bond_leg_rate::last_recent_period_calendar},
        utility::codec::field{"lookback", &bond_leg_rate::lookback},
        utility::codec::field{"rate_cutoff", &bond_leg_rate::rate_cutoff},
        utility::codec::field{"is_averaged", &bond_leg_rate::is_averaged},
        utility::codec::field{"has_sub_periods", &bond_leg_rate::has_sub_periods},
        utility::codec::field{"include_spread", &bond_leg_rate::include_spread},
        utility::codec::field{"is_not_resetting_xccy", &bond_leg_rate::is_not_resetting_xccy},
        utility::codec::field{"naked_option", &bond_leg_rate::naked_option},
        utility::codec::field{"local_cap_floor", &bond_leg_rate::local_cap_floor},
        utility::codec::field{"stub_use_original_curve", &bond_leg_rate::stub_use_original_curve},
        utility::codec::field{"observation_shift", &bond_leg_rate::observation_shift},
        utility::codec::field{"front_stub_short_index", &bond_leg_rate::front_stub_short_index},
        utility::codec::field{"front_stub_long_index", &bond_leg_rate::front_stub_long_index},
        utility::codec::field{"front_stub_rounding_type", &bond_leg_rate::front_stub_rounding_type},
        utility::codec::field{"front_stub_rounding_precision",
                              &bond_leg_rate::front_stub_rounding_precision},
        utility::codec::field{"back_stub_short_index", &bond_leg_rate::back_stub_short_index},
        utility::codec::field{"back_stub_long_index", &bond_leg_rate::back_stub_long_index},
        utility::codec::field{"back_stub_rounding_type", &bond_leg_rate::back_stub_rounding_type},
        utility::codec::field{"back_stub_rounding_precision",
                              &bond_leg_rate::back_stub_rounding_precision},
        utility::codec::field{"modified_by", &bond_leg_rate::modified_by},
        utility::codec::field{"performed_by", &bond_leg_rate::performed_by},
        utility::codec::field{"change_reason_code", &bond_leg_rate::change_reason_code},
        utility::codec::field{"change_commentary", &bond_leg_rate::change_commentary},
        utility::codec::field{"recorded_at", &bond_leg_rate::recorded_at},
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
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::domain::bond_leg_rate> {
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

    static std::string write_json(const ores::trading::domain::bond_leg_rate& v);

    static ores::trading::domain::bond_leg_rate read_json(std::string_view text);

    static std::string write_msgpack(const ores::trading::domain::bond_leg_rate& v);

    static ores::trading::domain::bond_leg_rate read_msgpack(std::span<const std::byte> data);
};

}

#endif
