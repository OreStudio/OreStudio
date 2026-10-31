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
#ifndef ORES_TRADING_API_DOMAIN_BOND_ISSUE_LEG_SCHEDULE_DATE_CODEC_HPP
#define ORES_TRADING_API_DOMAIN_BOND_ISSUE_LEG_SCHEDULE_DATE_CODEC_HPP

#include "ores.trading.api/domain/bond_issue_leg_schedule_date.hpp"
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
 * @brief The members of bond_issue_leg_schedule_date, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_issue_leg_schedule_date&) {
    return std::tuple{
        utility::codec::field{"version", &bond_issue_leg_schedule_date::version},
        utility::codec::field{"tenant_id", &bond_issue_leg_schedule_date::tenant_id},
        utility::codec::field{"issue_id", &bond_issue_leg_schedule_date::issue_id},
        utility::codec::field{"leg_number", &bond_issue_leg_schedule_date::leg_number},
        utility::codec::field{"schedule_role", &bond_issue_leg_schedule_date::schedule_role},
        utility::codec::field{"schedule_sequence_number",
                              &bond_issue_leg_schedule_date::schedule_sequence_number},
        utility::codec::field{"sequence_number", &bond_issue_leg_schedule_date::sequence_number},
        utility::codec::field{"schedule_date", &bond_issue_leg_schedule_date::schedule_date},
        utility::codec::field{"modified_by", &bond_issue_leg_schedule_date::modified_by},
        utility::codec::field{"performed_by", &bond_issue_leg_schedule_date::performed_by},
        utility::codec::field{"change_reason_code",
                              &bond_issue_leg_schedule_date::change_reason_code},
        utility::codec::field{"change_commentary",
                              &bond_issue_leg_schedule_date::change_commentary},
        utility::codec::field{"recorded_at", &bond_issue_leg_schedule_date::recorded_at},
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
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::domain::bond_issue_leg_schedule_date> {
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

    static std::string write_json(const ores::trading::domain::bond_issue_leg_schedule_date& v);

    static ores::trading::domain::bond_issue_leg_schedule_date read_json(std::string_view text);

    static std::string write_msgpack(const ores::trading::domain::bond_issue_leg_schedule_date& v);

    static ores::trading::domain::bond_issue_leg_schedule_date
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
