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
#ifndef ORES_TRADING_API_DOMAIN_COMMODITY_INSTRUMENT_CODEC_HPP
#define ORES_TRADING_API_DOMAIN_COMMODITY_INSTRUMENT_CODEC_HPP

#include "ores.trading.api/domain/commodity_instrument.hpp"
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
 * @brief The members of commodity_instrument, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_instrument&) {
    return std::tuple{
        utility::codec::field{"identity", &commodity_instrument::identity},
        utility::codec::field{"commodity_code", &commodity_instrument::commodity_code},
        utility::codec::field{"currency", &commodity_instrument::currency},
        utility::codec::field{"quantity", &commodity_instrument::quantity},
        utility::codec::field{"unit", &commodity_instrument::unit},
        utility::codec::field{"start_date", &commodity_instrument::start_date},
        utility::codec::field{"maturity_date", &commodity_instrument::maturity_date},
        utility::codec::field{"fixed_price", &commodity_instrument::fixed_price},
        utility::codec::field{"option_type", &commodity_instrument::option_type},
        utility::codec::field{"strike_price", &commodity_instrument::strike_price},
        utility::codec::field{"exercise_type", &commodity_instrument::exercise_type},
        utility::codec::field{"average_type", &commodity_instrument::average_type},
        utility::codec::field{"averaging_start_date", &commodity_instrument::averaging_start_date},
        utility::codec::field{"averaging_end_date", &commodity_instrument::averaging_end_date},
        utility::codec::field{"spread_commodity_code",
                              &commodity_instrument::spread_commodity_code},
        utility::codec::field{"spread_amount", &commodity_instrument::spread_amount},
        utility::codec::field{"strip_frequency_code", &commodity_instrument::strip_frequency_code},
        utility::codec::field{"variance_strike", &commodity_instrument::variance_strike},
        utility::codec::field{"accumulation_amount", &commodity_instrument::accumulation_amount},
        utility::codec::field{"knock_out_barrier", &commodity_instrument::knock_out_barrier},
        utility::codec::field{"barrier_type", &commodity_instrument::barrier_type},
        utility::codec::field{"lower_barrier", &commodity_instrument::lower_barrier},
        utility::codec::field{"upper_barrier", &commodity_instrument::upper_barrier},
        utility::codec::field{"day_count_fraction_code",
                              &commodity_instrument::day_count_fraction_code},
        utility::codec::field{"payment_frequency_code",
                              &commodity_instrument::payment_frequency_code},
        utility::codec::field{"swaption_expiry_date", &commodity_instrument::swaption_expiry_date},
        utility::codec::field{"description", &commodity_instrument::description},
        utility::codec::field{"audit", &commodity_instrument::audit},
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
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::domain::commodity_instrument> {
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

    static std::string write_json(const ores::trading::domain::commodity_instrument& v);

    static ores::trading::domain::commodity_instrument read_json(std::string_view text);

    static std::string write_msgpack(const ores::trading::domain::commodity_instrument& v);

    static ores::trading::domain::commodity_instrument
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
