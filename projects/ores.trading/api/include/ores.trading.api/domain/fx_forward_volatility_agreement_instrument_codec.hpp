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
#ifndef ORES_TRADING_API_DOMAIN_FX_FORWARD_VOLATILITY_AGREEMENT_INSTRUMENT_CODEC_HPP
#define ORES_TRADING_API_DOMAIN_FX_FORWARD_VOLATILITY_AGREEMENT_INSTRUMENT_CODEC_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/fx_forward_volatility_agreement_instrument.hpp"
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
 * @brief The members of fx_forward_volatility_agreement_instrument, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<fx_forward_volatility_agreement_instrument>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"identity", &fx_forward_volatility_agreement_instrument::identity},
        utility::codec::field{"fva_date", &fx_forward_volatility_agreement_instrument::fva_date},
        utility::codec::field{"option_expiry",
                              &fx_forward_volatility_agreement_instrument::option_expiry},
        utility::codec::field{"premium_date",
                              &fx_forward_volatility_agreement_instrument::premium_date},
        utility::codec::field{"long_short",
                              &fx_forward_volatility_agreement_instrument::long_short},
        utility::codec::field{"implied_vol_strike",
                              &fx_forward_volatility_agreement_instrument::implied_vol_strike},
        utility::codec::field{"quantity", &fx_forward_volatility_agreement_instrument::quantity},
        utility::codec::field{"pay_currency",
                              &fx_forward_volatility_agreement_instrument::pay_currency},
        utility::codec::field{"settlement_date",
                              &fx_forward_volatility_agreement_instrument::settlement_date},
        utility::codec::field{"underlying_strike",
                              &fx_forward_volatility_agreement_instrument::underlying_strike},
        utility::codec::field{"day_count_fraction",
                              &fx_forward_volatility_agreement_instrument::day_count_fraction},
        utility::codec::field{"dividend_yield",
                              &fx_forward_volatility_agreement_instrument::dividend_yield},
        utility::codec::field{"fixed_rate",
                              &fx_forward_volatility_agreement_instrument::fixed_rate},
        utility::codec::field{"payoff_type",
                              &fx_forward_volatility_agreement_instrument::payoff_type},
        utility::codec::field{"description",
                              &fx_forward_volatility_agreement_instrument::description},
        utility::codec::field{"audit", &fx_forward_volatility_agreement_instrument::audit},
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
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::domain::fx_forward_volatility_agreement_instrument> {
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

    static std::string
    write_json(const ores::trading::domain::fx_forward_volatility_agreement_instrument& v);

    static ores::trading::domain::fx_forward_volatility_agreement_instrument
    read_json(std::string_view text);

    static std::string
    write_msgpack(const ores::trading::domain::fx_forward_volatility_agreement_instrument& v);

    static ores::trading::domain::fx_forward_volatility_agreement_instrument
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
