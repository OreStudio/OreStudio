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
#ifndef ORES_SYNTHETIC_API_DOMAIN_VARIANCE_CALIBRATION_CODEC_HPP
#define ORES_SYNTHETIC_API_DOMAIN_VARIANCE_CALIBRATION_CODEC_HPP

#include "ores.synthetic.api/domain/variance_calibration.hpp"
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
 * @brief The members of variance_calibration, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<variance_calibration>) {
    return std::tuple{
        utility::codec::field{"version", &variance_calibration::version},
        utility::codec::field{"tenant_id", &variance_calibration::tenant_id},
        utility::codec::field{"id", &variance_calibration::id},
        utility::codec::field{"party_id", &variance_calibration::party_id},
        utility::codec::field{"base_currency_code", &variance_calibration::base_currency_code},
        utility::codec::field{"quote_currency_code", &variance_calibration::quote_currency_code},
        utility::codec::field{"input_id", &variance_calibration::input_id},
        utility::codec::field{"method", &variance_calibration::method},
        utility::codec::field{"method_version", &variance_calibration::method_version},
        utility::codec::field{"alpha", &variance_calibration::alpha},
        utility::codec::field{"gamma", &variance_calibration::gamma},
        utility::codec::field{"beta", &variance_calibration::beta},
        utility::codec::field{"persistence", &variance_calibration::persistence},
        utility::codec::field{"long_run_variance", &variance_calibration::long_run_variance},
        utility::codec::field{"starting_variance_ratio",
                              &variance_calibration::starting_variance_ratio},
        utility::codec::field{"log_likelihood", &variance_calibration::log_likelihood},
        utility::codec::field{"returns_used", &variance_calibration::returns_used},
        utility::codec::field{"starts", &variance_calibration::starts},
        utility::codec::field{"starts_agreed", &variance_calibration::starts_agreed},
        utility::codec::field{"state", &variance_calibration::state},
        utility::codec::field{"state_reason", &variance_calibration::state_reason},
        utility::codec::field{"modified_by", &variance_calibration::modified_by},
        utility::codec::field{"performed_by", &variance_calibration::performed_by},
        utility::codec::field{"change_reason_code", &variance_calibration::change_reason_code},
        utility::codec::field{"change_commentary", &variance_calibration::change_commentary},
        utility::codec::field{"recorded_at", &variance_calibration::recorded_at},
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
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::domain::variance_calibration> {
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

    static std::string write_json(const ores::synthetic::domain::variance_calibration& v);

    static ores::synthetic::domain::variance_calibration read_json(std::string_view text);

    static std::string write_msgpack(const ores::synthetic::domain::variance_calibration& v);

    static ores::synthetic::domain::variance_calibration
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
