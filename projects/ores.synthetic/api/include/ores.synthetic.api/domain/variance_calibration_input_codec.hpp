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
#ifndef ORES_SYNTHETIC_API_DOMAIN_VARIANCE_CALIBRATION_INPUT_CODEC_HPP
#define ORES_SYNTHETIC_API_DOMAIN_VARIANCE_CALIBRATION_INPUT_CODEC_HPP

#include "ores.synthetic.api/domain/variance_calibration_input.hpp"
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
 * @brief The members of variance_calibration_input, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const variance_calibration_input&) {
    return std::tuple{
        utility::codec::field{"version", &variance_calibration_input::version},
        utility::codec::field{"tenant_id", &variance_calibration_input::tenant_id},
        utility::codec::field{"id", &variance_calibration_input::id},
        utility::codec::field{"party_id", &variance_calibration_input::party_id},
        utility::codec::field{"base_currency_code",
                              &variance_calibration_input::base_currency_code},
        utility::codec::field{"quote_currency_code",
                              &variance_calibration_input::quote_currency_code},
        utility::codec::field{"fixing_uri", &variance_calibration_input::fixing_uri},
        utility::codec::field{"window_end", &variance_calibration_input::window_end},
        utility::codec::field{"window_days", &variance_calibration_input::window_days},
        utility::codec::field{"provider_code", &variance_calibration_input::provider_code},
        utility::codec::field{"inverted", &variance_calibration_input::inverted},
        utility::codec::field{"policy_id", &variance_calibration_input::policy_id},
        utility::codec::field{"policy_version", &variance_calibration_input::policy_version},
        utility::codec::field{"point_count", &variance_calibration_input::point_count},
        utility::codec::field{"used_count", &variance_calibration_input::used_count},
        utility::codec::field{"modified_by", &variance_calibration_input::modified_by},
        utility::codec::field{"performed_by", &variance_calibration_input::performed_by},
        utility::codec::field{"change_reason_code",
                              &variance_calibration_input::change_reason_code},
        utility::codec::field{"change_commentary", &variance_calibration_input::change_commentary},
        utility::codec::field{"recorded_at", &variance_calibration_input::recorded_at},
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
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::domain::variance_calibration_input> {
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

    static std::string write_json(const ores::synthetic::domain::variance_calibration_input& v);

    static ores::synthetic::domain::variance_calibration_input read_json(std::string_view text);

    static std::string write_msgpack(const ores::synthetic::domain::variance_calibration_input& v);

    static ores::synthetic::domain::variance_calibration_input
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
