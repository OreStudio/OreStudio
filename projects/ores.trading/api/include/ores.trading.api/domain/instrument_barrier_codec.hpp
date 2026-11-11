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
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_BARRIER_CODEC_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_BARRIER_CODEC_HPP

#include "ores.trading.api/domain/instrument_barrier.hpp"
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
 * @brief The members of instrument_barrier, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_barrier>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &instrument_barrier::version},
        utility::codec::field{"tenant_id", &instrument_barrier::tenant_id},
        utility::codec::field{"trade_id", &instrument_barrier::trade_id},
        utility::codec::field{"barrier_role", &instrument_barrier::barrier_role},
        utility::codec::field{"sequence_number", &instrument_barrier::sequence_number},
        utility::codec::field{"trade_activity_id", &instrument_barrier::trade_activity_id},
        utility::codec::field{"barrier_type", &instrument_barrier::barrier_type},
        utility::codec::field{"style", &instrument_barrier::style},
        utility::codec::field{"strict_comparison", &instrument_barrier::strict_comparison},
        utility::codec::field{"rebate", &instrument_barrier::rebate},
        utility::codec::field{"rebate_currency", &instrument_barrier::rebate_currency},
        utility::codec::field{"rebate_pay_time", &instrument_barrier::rebate_pay_time},
        utility::codec::field{"override_triggered", &instrument_barrier::override_triggered},
        utility::codec::field{"modified_by", &instrument_barrier::modified_by},
        utility::codec::field{"performed_by", &instrument_barrier::performed_by},
        utility::codec::field{"change_reason_code", &instrument_barrier::change_reason_code},
        utility::codec::field{"change_commentary", &instrument_barrier::change_commentary},
        utility::codec::field{"recorded_at", &instrument_barrier::recorded_at},
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
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::domain::instrument_barrier> {
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

    static std::string write_json(const ores::trading::domain::instrument_barrier& v);

    static ores::trading::domain::instrument_barrier read_json(std::string_view text);

    static std::string write_msgpack(const ores::trading::domain::instrument_barrier& v);

    static ores::trading::domain::instrument_barrier read_msgpack(std::span<const std::byte> data);
};

}

#endif
