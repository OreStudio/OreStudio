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
#ifndef ORES_TRADING_API_DOMAIN_FX_DIGITAL_OPTION_INSTRUMENT_CODEC_HPP
#define ORES_TRADING_API_DOMAIN_FX_DIGITAL_OPTION_INSTRUMENT_CODEC_HPP

#include "ores.trading.api/domain/fx_digital_option_instrument.hpp"
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
 * @brief The members of fx_digital_option_instrument, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const fx_digital_option_instrument&) {
    return std::tuple{
        utility::codec::field{"identity", &fx_digital_option_instrument::identity},
        utility::codec::field{"foreign_currency", &fx_digital_option_instrument::foreign_currency},
        utility::codec::field{"domestic_currency",
                              &fx_digital_option_instrument::domestic_currency},
        utility::codec::field{"payoff_currency", &fx_digital_option_instrument::payoff_currency},
        utility::codec::field{"payoff_amount", &fx_digital_option_instrument::payoff_amount},
        utility::codec::field{"option_type", &fx_digital_option_instrument::option_type},
        utility::codec::field{"expiry_date", &fx_digital_option_instrument::expiry_date},
        utility::codec::field{"long_short", &fx_digital_option_instrument::long_short},
        utility::codec::field{"strike", &fx_digital_option_instrument::strike},
        utility::codec::field{"barrier_type", &fx_digital_option_instrument::barrier_type},
        utility::codec::field{"lower_barrier", &fx_digital_option_instrument::lower_barrier},
        utility::codec::field{"upper_barrier", &fx_digital_option_instrument::upper_barrier},
        utility::codec::field{"description", &fx_digital_option_instrument::description},
        utility::codec::field{"audit", &fx_digital_option_instrument::audit},
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
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::domain::fx_digital_option_instrument> {
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

    static std::string write_json(const ores::trading::domain::fx_digital_option_instrument& v);

    static ores::trading::domain::fx_digital_option_instrument read_json(std::string_view text);

    static std::string write_msgpack(const ores::trading::domain::fx_digital_option_instrument& v);

    static ores::trading::domain::fx_digital_option_instrument
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
