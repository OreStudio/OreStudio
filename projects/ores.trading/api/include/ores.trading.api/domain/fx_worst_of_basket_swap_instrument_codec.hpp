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
#ifndef ORES_TRADING_API_DOMAIN_FX_WORST_OF_BASKET_SWAP_INSTRUMENT_CODEC_HPP
#define ORES_TRADING_API_DOMAIN_FX_WORST_OF_BASKET_SWAP_INSTRUMENT_CODEC_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/fx_worst_of_basket_swap_instrument.hpp"
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
 * @brief The members of fx_worst_of_basket_swap_instrument, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_worst_of_basket_swap_instrument>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"identity", &fx_worst_of_basket_swap_instrument::identity},
        utility::codec::field{"long_short", &fx_worst_of_basket_swap_instrument::long_short},
        utility::codec::field{"currency", &fx_worst_of_basket_swap_instrument::currency},
        utility::codec::field{"quantity", &fx_worst_of_basket_swap_instrument::quantity},
        utility::codec::field{"fixed_rate", &fx_worst_of_basket_swap_instrument::fixed_rate},
        utility::codec::field{"floating_index",
                              &fx_worst_of_basket_swap_instrument::floating_index},
        utility::codec::field{"floating_day_count_fraction",
                              &fx_worst_of_basket_swap_instrument::floating_day_count_fraction},
        utility::codec::field{"initial_fixed_rate",
                              &fx_worst_of_basket_swap_instrument::initial_fixed_rate},
        utility::codec::field{"initial_fixed_pay_date",
                              &fx_worst_of_basket_swap_instrument::initial_fixed_pay_date},
        utility::codec::field{"knock_in_pay_date",
                              &fx_worst_of_basket_swap_instrument::knock_in_pay_date},
        utility::codec::field{"bermudan_knock_in",
                              &fx_worst_of_basket_swap_instrument::bermudan_knock_in},
        utility::codec::field{"knock_in_level",
                              &fx_worst_of_basket_swap_instrument::knock_in_level},
        utility::codec::field{"accumulating_fixed_coupons",
                              &fx_worst_of_basket_swap_instrument::accumulating_fixed_coupons},
        utility::codec::field{"accruing_fixed_coupons",
                              &fx_worst_of_basket_swap_instrument::accruing_fixed_coupons},
        utility::codec::field{"floating_spread",
                              &fx_worst_of_basket_swap_instrument::floating_spread},
        utility::codec::field{"floating_lookback",
                              &fx_worst_of_basket_swap_instrument::floating_lookback},
        utility::codec::field{"floating_rate_cutoff",
                              &fx_worst_of_basket_swap_instrument::floating_rate_cutoff},
        utility::codec::field{"is_averaged", &fx_worst_of_basket_swap_instrument::is_averaged},
        utility::codec::field{"include_spread",
                              &fx_worst_of_basket_swap_instrument::include_spread},
        utility::codec::field{"strike", &fx_worst_of_basket_swap_instrument::strike},
        utility::codec::field{"description", &fx_worst_of_basket_swap_instrument::description},
        utility::codec::field{"audit", &fx_worst_of_basket_swap_instrument::audit},
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
    wire_traits<ores::trading::domain::fx_worst_of_basket_swap_instrument> {
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
    write_json(const ores::trading::domain::fx_worst_of_basket_swap_instrument& v);

    static ores::trading::domain::fx_worst_of_basket_swap_instrument
    read_json(std::string_view text);

    static std::string
    write_msgpack(const ores::trading::domain::fx_worst_of_basket_swap_instrument& v);

    static ores::trading::domain::fx_worst_of_basket_swap_instrument
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
