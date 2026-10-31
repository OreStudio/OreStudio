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
#ifndef ORES_TRADING_API_DOMAIN_TRADE_SUMMARY_CODEC_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_SUMMARY_CODEC_HPP

#include "ores.trading.api/domain/trade_summary.hpp"
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
 * @brief The members of trade_summary, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_summary&) {
    return std::tuple{
        utility::codec::field{"tenant_id", &trade_summary::tenant_id},
        utility::codec::field{"id", &trade_summary::id},
        utility::codec::field{"row_kind", &trade_summary::row_kind},
        utility::codec::field{"party_id", &trade_summary::party_id},
        utility::codec::field{"counterparty_id", &trade_summary::counterparty_id},
        utility::codec::field{"book_id", &trade_summary::book_id},
        utility::codec::field{"instrument_type", &trade_summary::instrument_type},
        utility::codec::field{"status", &trade_summary::status},
        utility::codec::field{"authorisation_status", &trade_summary::authorisation_status},
        utility::codec::field{"trade_date", &trade_summary::trade_date},
        utility::codec::field{"trade_count", &trade_summary::trade_count},
        utility::codec::field{"structure_id", &trade_summary::structure_id},
        utility::codec::field{"is_leg", &trade_summary::is_leg},
        utility::codec::field{"search_text", &trade_summary::search_text},
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
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::domain::trade_summary> {
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

    static std::string write_json(const ores::trading::domain::trade_summary& v);

    static ores::trading::domain::trade_summary read_json(std::string_view text);

    static std::string write_msgpack(const ores::trading::domain::trade_summary& v);

    static ores::trading::domain::trade_summary read_msgpack(std::span<const std::byte> data);
};

}

#endif
