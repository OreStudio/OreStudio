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
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_OPTION_CODEC_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_OPTION_CODEC_HPP

#include "ores.trading.api/domain/instrument_option.hpp"
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
 * @brief The members of instrument_option, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_option&) {
    return std::tuple{
        utility::codec::field{"version", &instrument_option::version},
        utility::codec::field{"tenant_id", &instrument_option::tenant_id},
        utility::codec::field{"trade_id", &instrument_option::trade_id},
        utility::codec::field{"trade_activity_id", &instrument_option::trade_activity_id},
        utility::codec::field{"long_short", &instrument_option::long_short},
        utility::codec::field{"option_type", &instrument_option::option_type},
        utility::codec::field{"payoff_type", &instrument_option::payoff_type},
        utility::codec::field{"payoff_type_2", &instrument_option::payoff_type_2},
        utility::codec::field{"style", &instrument_option::style},
        utility::codec::field{"notice_period", &instrument_option::notice_period},
        utility::codec::field{"notice_calendar", &instrument_option::notice_calendar},
        utility::codec::field{"notice_convention", &instrument_option::notice_convention},
        utility::codec::field{"mid_coupon_exercise", &instrument_option::mid_coupon_exercise},
        utility::codec::field{"settlement", &instrument_option::settlement},
        utility::codec::field{"settlement_method", &instrument_option::settlement_method},
        utility::codec::field{"pay_off_at_expiry", &instrument_option::pay_off_at_expiry},
        utility::codec::field{"premium_amount", &instrument_option::premium_amount},
        utility::codec::field{"premium_currency", &instrument_option::premium_currency},
        utility::codec::field{"premium_pay_date", &instrument_option::premium_pay_date},
        utility::codec::field{"exercise_fee_settlement_period",
                              &instrument_option::exercise_fee_settlement_period},
        utility::codec::field{"exercise_fee_settlement_calendar",
                              &instrument_option::exercise_fee_settlement_calendar},
        utility::codec::field{"exercise_fee_settlement_convention",
                              &instrument_option::exercise_fee_settlement_convention},
        utility::codec::field{"automatic_exercise", &instrument_option::automatic_exercise},
        utility::codec::field{"has_exercise_data", &instrument_option::has_exercise_data},
        utility::codec::field{"exercise_date", &instrument_option::exercise_date},
        utility::codec::field{"exercise_price", &instrument_option::exercise_price},
        utility::codec::field{"has_payment_data", &instrument_option::has_payment_data},
        utility::codec::field{"payment_lag", &instrument_option::payment_lag},
        utility::codec::field{"payment_calendar", &instrument_option::payment_calendar},
        utility::codec::field{"payment_convention", &instrument_option::payment_convention},
        utility::codec::field{"payment_relative_to", &instrument_option::payment_relative_to},
        utility::codec::field{"has_settlement_data", &instrument_option::has_settlement_data},
        utility::codec::field{"settlement_pay_currency",
                              &instrument_option::settlement_pay_currency},
        utility::codec::field{"settlement_fx_index", &instrument_option::settlement_fx_index},
        utility::codec::field{"settlement_fixing_date", &instrument_option::settlement_fixing_date},
        utility::codec::field{"modified_by", &instrument_option::modified_by},
        utility::codec::field{"performed_by", &instrument_option::performed_by},
        utility::codec::field{"change_reason_code", &instrument_option::change_reason_code},
        utility::codec::field{"change_commentary", &instrument_option::change_commentary},
        utility::codec::field{"recorded_at", &instrument_option::recorded_at},
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
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::domain::instrument_option> {
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

    static std::string write_json(const ores::trading::domain::instrument_option& v);

    static ores::trading::domain::instrument_option read_json(std::string_view text);

    static std::string write_msgpack(const ores::trading::domain::instrument_option& v);

    static ores::trading::domain::instrument_option read_msgpack(std::span<const std::byte> data);
};

}

#endif
