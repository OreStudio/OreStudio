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
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_STRIKE_CODEC_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_STRIKE_CODEC_HPP

#include "ores.trading.api/domain/instrument_strike.hpp"
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
 * @brief The members of instrument_strike, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_strike&) {
    return std::tuple{
        utility::codec::field{"version", &instrument_strike::version},
        utility::codec::field{"tenant_id", &instrument_strike::tenant_id},
        utility::codec::field{"trade_id", &instrument_strike::trade_id},
        utility::codec::field{"strike_role", &instrument_strike::strike_role},
        utility::codec::field{"sequence_number", &instrument_strike::sequence_number},
        utility::codec::field{"trade_activity_id", &instrument_strike::trade_activity_id},
        utility::codec::field{"price_value", &instrument_strike::price_value},
        utility::codec::field{"price_currency", &instrument_strike::price_currency},
        utility::codec::field{"yield_value", &instrument_strike::yield_value},
        utility::codec::field{"yield_compounding", &instrument_strike::yield_compounding},
        utility::codec::field{"bare_value", &instrument_strike::bare_value},
        utility::codec::field{"bare_currency", &instrument_strike::bare_currency},
        utility::codec::field{"modified_by", &instrument_strike::modified_by},
        utility::codec::field{"performed_by", &instrument_strike::performed_by},
        utility::codec::field{"change_reason_code", &instrument_strike::change_reason_code},
        utility::codec::field{"change_commentary", &instrument_strike::change_commentary},
        utility::codec::field{"recorded_at", &instrument_strike::recorded_at},
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
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::domain::instrument_strike> {
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

    static std::string write_json(const ores::trading::domain::instrument_strike& v);

    static ores::trading::domain::instrument_strike read_json(std::string_view text);

    static std::string write_msgpack(const ores::trading::domain::instrument_strike& v);

    static ores::trading::domain::instrument_strike read_msgpack(std::span<const std::byte> data);
};

}

#endif
