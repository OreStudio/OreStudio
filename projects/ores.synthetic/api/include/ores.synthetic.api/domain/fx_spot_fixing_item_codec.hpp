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
#ifndef ORES_SYNTHETIC_API_DOMAIN_FX_SPOT_FIXING_ITEM_CODEC_HPP
#define ORES_SYNTHETIC_API_DOMAIN_FX_SPOT_FIXING_ITEM_CODEC_HPP

#include "ores.synthetic.api/domain/fx_spot_fixing_item.hpp"
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
 * @brief The members of fx_spot_fixing_item, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_fixing_item>) {
    return std::tuple{
        utility::codec::field{"version", &fx_spot_fixing_item::version},
        utility::codec::field{"tenant_id", &fx_spot_fixing_item::tenant_id},
        utility::codec::field{"id", &fx_spot_fixing_item::id},
        utility::codec::field{"party_id", &fx_spot_fixing_item::party_id},
        utility::codec::field{"fx_spot_config_id", &fx_spot_fixing_item::fx_spot_config_id},
        utility::codec::field{"rank", &fx_spot_fixing_item::rank},
        utility::codec::field{"item", &fx_spot_fixing_item::item},
        utility::codec::field{"provider_code", &fx_spot_fixing_item::provider_code},
        utility::codec::field{"policy", &fx_spot_fixing_item::policy},
        utility::codec::field{"window_days", &fx_spot_fixing_item::window_days},
        utility::codec::field{"modified_by", &fx_spot_fixing_item::modified_by},
        utility::codec::field{"performed_by", &fx_spot_fixing_item::performed_by},
        utility::codec::field{"change_reason_code", &fx_spot_fixing_item::change_reason_code},
        utility::codec::field{"change_commentary", &fx_spot_fixing_item::change_commentary},
        utility::codec::field{"recorded_at", &fx_spot_fixing_item::recorded_at},
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
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::domain::fx_spot_fixing_item> {
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

    static std::string write_json(const ores::synthetic::domain::fx_spot_fixing_item& v);

    static ores::synthetic::domain::fx_spot_fixing_item read_json(std::string_view text);

    static std::string write_msgpack(const ores::synthetic::domain::fx_spot_fixing_item& v);

    static ores::synthetic::domain::fx_spot_fixing_item
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
