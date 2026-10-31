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
#ifndef ORES_TRADING_API_DOMAIN_BOND_FORWARD_CODEC_HPP
#define ORES_TRADING_API_DOMAIN_BOND_FORWARD_CODEC_HPP

#include "ores.trading.api/domain/bond_forward.hpp"
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
 * @brief The members of bond_forward, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_forward&) {
    return std::tuple{
        utility::codec::field{"version", &bond_forward::version},
        utility::codec::field{"tenant_id", &bond_forward::tenant_id},
        utility::codec::field{"trade_id", &bond_forward::trade_id},
        utility::codec::field{"trade_activity_id", &bond_forward::trade_activity_id},
        utility::codec::field{"long_in_forward", &bond_forward::long_in_forward},
        utility::codec::field{"forward_maturity_date", &bond_forward::forward_maturity_date},
        utility::codec::field{"forward_settlement_date", &bond_forward::forward_settlement_date},
        utility::codec::field{"settlement", &bond_forward::settlement},
        utility::codec::field{"amount", &bond_forward::amount},
        utility::codec::field{"lock_rate", &bond_forward::lock_rate},
        utility::codec::field{"dv01", &bond_forward::dv01},
        utility::codec::field{"lock_rate_day_counter", &bond_forward::lock_rate_day_counter},
        utility::codec::field{"settlement_dirty", &bond_forward::settlement_dirty},
        utility::codec::field{"premium_amount", &bond_forward::premium_amount},
        utility::codec::field{"premium_date", &bond_forward::premium_date},
        utility::codec::field{"modified_by", &bond_forward::modified_by},
        utility::codec::field{"performed_by", &bond_forward::performed_by},
        utility::codec::field{"change_reason_code", &bond_forward::change_reason_code},
        utility::codec::field{"change_commentary", &bond_forward::change_commentary},
        utility::codec::field{"recorded_at", &bond_forward::recorded_at},
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
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::domain::bond_forward> {
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

    static std::string write_json(const ores::trading::domain::bond_forward& v);

    static ores::trading::domain::bond_forward read_json(std::string_view text);

    static std::string write_msgpack(const ores::trading::domain::bond_forward& v);

    static ores::trading::domain::bond_forward read_msgpack(std::span<const std::byte> data);
};

}

#endif
