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
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_UNDERLYING_CODEC_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_UNDERLYING_CODEC_HPP

#include "ores.trading.api/domain/instrument_underlying.hpp"
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
 * @brief The members of instrument_underlying, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_underlying>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &instrument_underlying::version},
        utility::codec::field{"tenant_id", &instrument_underlying::tenant_id},
        utility::codec::field{"trade_id", &instrument_underlying::trade_id},
        utility::codec::field{"underlying_role", &instrument_underlying::underlying_role},
        utility::codec::field{"sequence_number", &instrument_underlying::sequence_number},
        utility::codec::field{"trade_activity_id", &instrument_underlying::trade_activity_id},
        utility::codec::field{"underlying_type", &instrument_underlying::underlying_type},
        utility::codec::field{"name", &instrument_underlying::name},
        utility::codec::field{"identifier_type", &instrument_underlying::identifier_type},
        utility::codec::field{"currency", &instrument_underlying::currency},
        utility::codec::field{"exchange", &instrument_underlying::exchange},
        utility::codec::field{"weight", &instrument_underlying::weight},
        utility::codec::field{"price_type", &instrument_underlying::price_type},
        utility::codec::field{"future_month_offset", &instrument_underlying::future_month_offset},
        utility::codec::field{"delivery_roll_days", &instrument_underlying::delivery_roll_days},
        utility::codec::field{"delivery_roll_calendar",
                              &instrument_underlying::delivery_roll_calendar},
        utility::codec::field{"future_expiry_date", &instrument_underlying::future_expiry_date},
        utility::codec::field{"future_contract_month",
                              &instrument_underlying::future_contract_month},
        utility::codec::field{"interpolation", &instrument_underlying::interpolation},
        utility::codec::field{"bid_ask_adjustment", &instrument_underlying::bid_ask_adjustment},
        utility::codec::field{"modified_by", &instrument_underlying::modified_by},
        utility::codec::field{"performed_by", &instrument_underlying::performed_by},
        utility::codec::field{"change_reason_code", &instrument_underlying::change_reason_code},
        utility::codec::field{"change_commentary", &instrument_underlying::change_commentary},
        utility::codec::field{"recorded_at", &instrument_underlying::recorded_at},
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
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::domain::instrument_underlying> {
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

    static std::string write_json(const ores::trading::domain::instrument_underlying& v);

    static ores::trading::domain::instrument_underlying read_json(std::string_view text);

    static std::string write_msgpack(const ores::trading::domain::instrument_underlying& v);

    static ores::trading::domain::instrument_underlying
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
