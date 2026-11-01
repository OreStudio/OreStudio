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
#ifndef ORES_TRADING_API_DOMAIN_BOND_ISSUE_LEG_CODEC_HPP
#define ORES_TRADING_API_DOMAIN_BOND_ISSUE_LEG_CODEC_HPP

#include "ores.trading.api/domain/bond_issue_leg.hpp"
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
 * @brief The members of bond_issue_leg, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_issue_leg&) {
    return std::tuple{
        utility::codec::field{"version", &bond_issue_leg::version},
        utility::codec::field{"tenant_id", &bond_issue_leg::tenant_id},
        utility::codec::field{"issue_id", &bond_issue_leg::issue_id},
        utility::codec::field{"leg_number", &bond_issue_leg::leg_number},
        utility::codec::field{"payer", &bond_issue_leg::payer},
        utility::codec::field{"leg_type", &bond_issue_leg::leg_type},
        utility::codec::field{"currency", &bond_issue_leg::currency},
        utility::codec::field{"payment_convention", &bond_issue_leg::payment_convention},
        utility::codec::field{"payment_lag", &bond_issue_leg::payment_lag},
        utility::codec::field{"payment_calendar", &bond_issue_leg::payment_calendar},
        utility::codec::field{"day_counter", &bond_issue_leg::day_counter},
        utility::codec::field{"last_period_day_counter", &bond_issue_leg::last_period_day_counter},
        utility::codec::field{"notional_payment_lag", &bond_issue_leg::notional_payment_lag},
        utility::codec::field{"strict_notional_dates", &bond_issue_leg::strict_notional_dates},
        utility::codec::field{"indexings_from_asset_leg",
                              &bond_issue_leg::indexings_from_asset_leg},
        utility::codec::field{"settlement_fx_index", &bond_issue_leg::settlement_fx_index},
        utility::codec::field{"settlement_fixing_date", &bond_issue_leg::settlement_fixing_date},
        utility::codec::field{"modified_by", &bond_issue_leg::modified_by},
        utility::codec::field{"performed_by", &bond_issue_leg::performed_by},
        utility::codec::field{"change_reason_code", &bond_issue_leg::change_reason_code},
        utility::codec::field{"change_commentary", &bond_issue_leg::change_commentary},
        utility::codec::field{"recorded_at", &bond_issue_leg::recorded_at},
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
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::domain::bond_issue_leg> {
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

    static std::string write_json(const ores::trading::domain::bond_issue_leg& v);

    static ores::trading::domain::bond_issue_leg read_json(std::string_view text);

    static std::string write_msgpack(const ores::trading::domain::bond_issue_leg& v);

    static ores::trading::domain::bond_issue_leg read_msgpack(std::span<const std::byte> data);
};

}

#endif
