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
#ifndef ORES_REFDATA_API_DOMAIN_CROSS_CURRENCY_BASIS_CONVENTION_CODEC_HPP
#define ORES_REFDATA_API_DOMAIN_CROSS_CURRENCY_BASIS_CONVENTION_CODEC_HPP

#include "ores.refdata.api/domain/cross_currency_basis_convention.hpp"
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
 * @brief The members of cross_currency_basis_convention, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const cross_currency_basis_convention&) {
    return std::tuple{
        utility::codec::field{"version", &cross_currency_basis_convention::version},
        utility::codec::field{"tenant_id", &cross_currency_basis_convention::tenant_id},
        utility::codec::field{"id", &cross_currency_basis_convention::id},
        utility::codec::field{"party_id", &cross_currency_basis_convention::party_id},
        utility::codec::field{"settlement_days", &cross_currency_basis_convention::settlement_days},
        utility::codec::field{"settlement_calendar",
                              &cross_currency_basis_convention::settlement_calendar},
        utility::codec::field{"roll_convention", &cross_currency_basis_convention::roll_convention},
        utility::codec::field{"flat_index", &cross_currency_basis_convention::flat_index},
        utility::codec::field{"spread_index", &cross_currency_basis_convention::spread_index},
        utility::codec::field{"eom", &cross_currency_basis_convention::eom},
        utility::codec::field{"is_resettable", &cross_currency_basis_convention::is_resettable},
        utility::codec::field{"flat_index_is_resettable",
                              &cross_currency_basis_convention::flat_index_is_resettable},
        utility::codec::field{"flat_tenor", &cross_currency_basis_convention::flat_tenor},
        utility::codec::field{"spread_tenor", &cross_currency_basis_convention::spread_tenor},
        utility::codec::field{"spread_payment_lag",
                              &cross_currency_basis_convention::spread_payment_lag},
        utility::codec::field{"flat_payment_lag",
                              &cross_currency_basis_convention::flat_payment_lag},
        utility::codec::field{"spread_include_spread",
                              &cross_currency_basis_convention::spread_include_spread},
        utility::codec::field{"spread_lookback", &cross_currency_basis_convention::spread_lookback},
        utility::codec::field{"spread_fixing_days",
                              &cross_currency_basis_convention::spread_fixing_days},
        utility::codec::field{"spread_rate_cutoff",
                              &cross_currency_basis_convention::spread_rate_cutoff},
        utility::codec::field{"spread_is_averaged",
                              &cross_currency_basis_convention::spread_is_averaged},
        utility::codec::field{"spread_observation_shift",
                              &cross_currency_basis_convention::spread_observation_shift},
        utility::codec::field{"flat_include_spread",
                              &cross_currency_basis_convention::flat_include_spread},
        utility::codec::field{"flat_lookback", &cross_currency_basis_convention::flat_lookback},
        utility::codec::field{"flat_fixing_days",
                              &cross_currency_basis_convention::flat_fixing_days},
        utility::codec::field{"flat_rate_cutoff",
                              &cross_currency_basis_convention::flat_rate_cutoff},
        utility::codec::field{"flat_is_averaged",
                              &cross_currency_basis_convention::flat_is_averaged},
        utility::codec::field{"flat_observation_shift",
                              &cross_currency_basis_convention::flat_observation_shift},
        utility::codec::field{"oresmd_uri", &cross_currency_basis_convention::oresmd_uri},
        utility::codec::field{"modified_by", &cross_currency_basis_convention::modified_by},
        utility::codec::field{"performed_by", &cross_currency_basis_convention::performed_by},
        utility::codec::field{"change_reason_code",
                              &cross_currency_basis_convention::change_reason_code},
        utility::codec::field{"change_commentary",
                              &cross_currency_basis_convention::change_commentary},
        utility::codec::field{"recorded_at", &cross_currency_basis_convention::recorded_at},
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
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::domain::cross_currency_basis_convention> {
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

    static std::string write_json(const ores::refdata::domain::cross_currency_basis_convention& v);

    static ores::refdata::domain::cross_currency_basis_convention read_json(std::string_view text);

    static std::string
    write_msgpack(const ores::refdata::domain::cross_currency_basis_convention& v);

    static ores::refdata::domain::cross_currency_basis_convention
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
