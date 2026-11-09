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
#ifndef ORES_REFDATA_API_DOMAIN_COMMODITY_FUTURE_AVERAGING_TERMS_CODEC_HPP
#define ORES_REFDATA_API_DOMAIN_COMMODITY_FUTURE_AVERAGING_TERMS_CODEC_HPP

#include "ores.refdata.api/domain/commodity_future_averaging_terms.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::refdata::domain {

/**
 * @brief The members of commodity_future_averaging_terms, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<commodity_future_averaging_terms>) {
    return std::tuple{
        utility::codec::field{"version", &commodity_future_averaging_terms::version},
        utility::codec::field{"tenant_id", &commodity_future_averaging_terms::tenant_id},
        utility::codec::field{"id", &commodity_future_averaging_terms::id},
        utility::codec::field{"convention_id", &commodity_future_averaging_terms::convention_id},
        utility::codec::field{"party_id", &commodity_future_averaging_terms::party_id},
        utility::codec::field{"averaging_commodity_name",
                              &commodity_future_averaging_terms::averaging_commodity_name},
        utility::codec::field{"averaging_period",
                              &commodity_future_averaging_terms::averaging_period},
        utility::codec::field{"averaging_pricing_calendar",
                              &commodity_future_averaging_terms::averaging_pricing_calendar},
        utility::codec::field{"averaging_conventions",
                              &commodity_future_averaging_terms::averaging_conventions},
        utility::codec::field{"averaging_use_business_days",
                              &commodity_future_averaging_terms::averaging_use_business_days},
        utility::codec::field{"averaging_delivery_roll_days",
                              &commodity_future_averaging_terms::averaging_delivery_roll_days},
        utility::codec::field{"averaging_future_month_offset",
                              &commodity_future_averaging_terms::averaging_future_month_offset},
        utility::codec::field{"averaging_daily_expiry_offset",
                              &commodity_future_averaging_terms::averaging_daily_expiry_offset},
        utility::codec::field{"modified_by", &commodity_future_averaging_terms::modified_by},
        utility::codec::field{"performed_by", &commodity_future_averaging_terms::performed_by},
        utility::codec::field{"change_reason_code",
                              &commodity_future_averaging_terms::change_reason_code},
        utility::codec::field{"change_commentary",
                              &commodity_future_averaging_terms::change_commentary},
        utility::codec::field{"recorded_at", &commodity_future_averaging_terms::recorded_at},
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
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::domain::commodity_future_averaging_terms> {
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

    static std::string write_json(const ores::refdata::domain::commodity_future_averaging_terms& v);

    static ores::refdata::domain::commodity_future_averaging_terms read_json(std::string_view text);

    static std::string
    write_msgpack(const ores::refdata::domain::commodity_future_averaging_terms& v);

    static ores::refdata::domain::commodity_future_averaging_terms
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
