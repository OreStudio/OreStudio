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
#ifndef ORES_REFDATA_API_DOMAIN_COMMODITY_FUTURE_CONTRACT_TERMS_CODEC_HPP
#define ORES_REFDATA_API_DOMAIN_COMMODITY_FUTURE_CONTRACT_TERMS_CODEC_HPP

#include "ores.refdata.api/domain/commodity_future_contract_terms.hpp"
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
 * @brief The members of commodity_future_contract_terms, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<commodity_future_contract_terms>) {
    return std::tuple{
        utility::codec::field{"version", &commodity_future_contract_terms::version},
        utility::codec::field{"tenant_id", &commodity_future_contract_terms::tenant_id},
        utility::codec::field{"id", &commodity_future_contract_terms::id},
        utility::codec::field{"convention_id", &commodity_future_contract_terms::convention_id},
        utility::codec::field{"party_id", &commodity_future_contract_terms::party_id},
        utility::codec::field{"contract_frequency",
                              &commodity_future_contract_terms::contract_frequency},
        utility::codec::field{"calendar", &commodity_future_contract_terms::calendar},
        utility::codec::field{"expiry_calendar", &commodity_future_contract_terms::expiry_calendar},
        utility::codec::field{"expiry_month_lag",
                              &commodity_future_contract_terms::expiry_month_lag},
        utility::codec::field{"one_contract_month",
                              &commodity_future_contract_terms::one_contract_month},
        utility::codec::field{"offset_days", &commodity_future_contract_terms::offset_days},
        utility::codec::field{"business_day_convention",
                              &commodity_future_contract_terms::business_day_convention},
        utility::codec::field{"adjust_before_offset",
                              &commodity_future_contract_terms::adjust_before_offset},
        utility::codec::field{"is_averaging", &commodity_future_contract_terms::is_averaging},
        utility::codec::field{"valid_contract_months",
                              &commodity_future_contract_terms::valid_contract_months},
        utility::codec::field{"hours_per_day", &commodity_future_contract_terms::hours_per_day},
        utility::codec::field{"index_name", &commodity_future_contract_terms::index_name},
        utility::codec::field{"savings_time", &commodity_future_contract_terms::savings_time},
        utility::codec::field{"delivery_location",
                              &commodity_future_contract_terms::delivery_location},
        utility::codec::field{"balance_of_the_month",
                              &commodity_future_contract_terms::balance_of_the_month},
        utility::codec::field{
            "balance_of_the_month_pricing_calendar",
            &commodity_future_contract_terms::balance_of_the_month_pricing_calendar},
        utility::codec::field{"prohibited_expiries",
                              &commodity_future_contract_terms::prohibited_expiries},
        utility::codec::field{"future_continuation_mappings",
                              &commodity_future_contract_terms::future_continuation_mappings},
        utility::codec::field{"modified_by", &commodity_future_contract_terms::modified_by},
        utility::codec::field{"performed_by", &commodity_future_contract_terms::performed_by},
        utility::codec::field{"change_reason_code",
                              &commodity_future_contract_terms::change_reason_code},
        utility::codec::field{"change_commentary",
                              &commodity_future_contract_terms::change_commentary},
        utility::codec::field{"recorded_at", &commodity_future_contract_terms::recorded_at},
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
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::domain::commodity_future_contract_terms> {
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

    static std::string write_json(const ores::refdata::domain::commodity_future_contract_terms& v);

    static ores::refdata::domain::commodity_future_contract_terms read_json(std::string_view text);

    static std::string
    write_msgpack(const ores::refdata::domain::commodity_future_contract_terms& v);

    static ores::refdata::domain::commodity_future_contract_terms
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
