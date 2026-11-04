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
#ifndef ORES_SYNTHETIC_API_DOMAIN_TRADE_GENERATION_CONFIG_CODEC_HPP
#define ORES_SYNTHETIC_API_DOMAIN_TRADE_GENERATION_CONFIG_CODEC_HPP

#include "ores.synthetic.api/domain/trade_generation_config.hpp"
#include "ores.synthetic.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::synthetic::domain {

/**
 * @brief The members of trade_generation_config, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_generation_config>) {
    return std::tuple{
        utility::codec::field{"version", &trade_generation_config::version},
        utility::codec::field{"tenant_id", &trade_generation_config::tenant_id},
        utility::codec::field{"id", &trade_generation_config::id},
        utility::codec::field{"party_id", &trade_generation_config::party_id},
        utility::codec::field{"name", &trade_generation_config::name},
        utility::codec::field{"description", &trade_generation_config::description},
        utility::codec::field{"asset_class_code", &trade_generation_config::asset_class_code},
        utility::codec::field{"min_tenor_code", &trade_generation_config::min_tenor_code},
        utility::codec::field{"max_tenor_code", &trade_generation_config::max_tenor_code},
        utility::codec::field{"min_notional", &trade_generation_config::min_notional},
        utility::codec::field{"max_notional", &trade_generation_config::max_notional},
        utility::codec::field{"as_of_date", &trade_generation_config::as_of_date},
        utility::codec::field{"seed", &trade_generation_config::seed},
        utility::codec::field{"trades_per_book", &trade_generation_config::trades_per_book},
        utility::codec::field{"structures_per_book", &trade_generation_config::structures_per_book},
        utility::codec::field{"enabled", &trade_generation_config::enabled},
        utility::codec::field{"reference_curve_id", &trade_generation_config::reference_curve_id},
        utility::codec::field{"modified_by", &trade_generation_config::modified_by},
        utility::codec::field{"performed_by", &trade_generation_config::performed_by},
        utility::codec::field{"change_reason_code", &trade_generation_config::change_reason_code},
        utility::codec::field{"change_commentary", &trade_generation_config::change_commentary},
        utility::codec::field{"recorded_at", &trade_generation_config::recorded_at},
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
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::domain::trade_generation_config> {
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

    static std::string write_json(const ores::synthetic::domain::trade_generation_config& v);

    static ores::synthetic::domain::trade_generation_config read_json(std::string_view text);

    static std::string write_msgpack(const ores::synthetic::domain::trade_generation_config& v);

    static ores::synthetic::domain::trade_generation_config
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
