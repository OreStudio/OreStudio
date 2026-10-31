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
#ifndef ORES_REFDATA_API_DOMAIN_COMMODITY_FUTURE_CONVENTION_CODEC_HPP
#define ORES_REFDATA_API_DOMAIN_COMMODITY_FUTURE_CONVENTION_CODEC_HPP

#include "ores.refdata.api/domain/commodity_future_convention.hpp"
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
 * @brief The members of commodity_future_convention, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_convention&) {
    return std::tuple{
        utility::codec::field{"version", &commodity_future_convention::version},
        utility::codec::field{"tenant_id", &commodity_future_convention::tenant_id},
        utility::codec::field{"id", &commodity_future_convention::id},
        utility::codec::field{"party_id", &commodity_future_convention::party_id},
        utility::codec::field{"contract_frequency",
                              &commodity_future_convention::contract_frequency},
        utility::codec::field{"calendar", &commodity_future_convention::calendar},
        utility::codec::field{"expiry_calendar", &commodity_future_convention::expiry_calendar},
        utility::codec::field{"expiry_month_lag", &commodity_future_convention::expiry_month_lag},
        utility::codec::field{"one_contract_month",
                              &commodity_future_convention::one_contract_month},
        utility::codec::field{"offset_days", &commodity_future_convention::offset_days},
        utility::codec::field{"business_day_convention",
                              &commodity_future_convention::business_day_convention},
        utility::codec::field{"adjust_before_offset",
                              &commodity_future_convention::adjust_before_offset},
        utility::codec::field{"is_averaging", &commodity_future_convention::is_averaging},
        utility::codec::field{"valid_contract_months",
                              &commodity_future_convention::valid_contract_months},
        utility::codec::field{"anchor_day_of_month",
                              &commodity_future_convention::anchor_day_of_month},
        utility::codec::field{"anchor_calendar_days_before",
                              &commodity_future_convention::anchor_calendar_days_before},
        utility::codec::field{"anchor_business_days_after",
                              &commodity_future_convention::anchor_business_days_after},
        utility::codec::field{"anchor_nth_nth", &commodity_future_convention::anchor_nth_nth},
        utility::codec::field{"anchor_nth_weekday",
                              &commodity_future_convention::anchor_nth_weekday},
        utility::codec::field{"anchor_last_weekday",
                              &commodity_future_convention::anchor_last_weekday},
        utility::codec::field{"anchor_weekly_day_of_the_week",
                              &commodity_future_convention::anchor_weekly_day_of_the_week},
        utility::codec::field{"option_expiry_month_lag",
                              &commodity_future_convention::option_expiry_month_lag},
        utility::codec::field{"option_contract_frequency",
                              &commodity_future_convention::option_contract_frequency},
        utility::codec::field{"option_expiry_offset",
                              &commodity_future_convention::option_expiry_offset},
        utility::codec::field{"option_calendar_days_before",
                              &commodity_future_convention::option_calendar_days_before},
        utility::codec::field{"option_min_business_days_before",
                              &commodity_future_convention::option_min_business_days_before},
        utility::codec::field{"option_expiry_day", &commodity_future_convention::option_expiry_day},
        utility::codec::field{"option_nth_nth", &commodity_future_convention::option_nth_nth},
        utility::codec::field{"option_nth_weekday",
                              &commodity_future_convention::option_nth_weekday},
        utility::codec::field{"option_expiry_last_weekday_of_month",
                              &commodity_future_convention::option_expiry_last_weekday_of_month},
        utility::codec::field{"option_expiry_weekly_day_of_the_week",
                              &commodity_future_convention::option_expiry_weekly_day_of_the_week},
        utility::codec::field{"option_business_day_convention",
                              &commodity_future_convention::option_business_day_convention},
        utility::codec::field{"hours_per_day", &commodity_future_convention::hours_per_day},
        utility::codec::field{"off_peak_index", &commodity_future_convention::off_peak_index},
        utility::codec::field{"peak_index", &commodity_future_convention::peak_index},
        utility::codec::field{"off_peak_hours", &commodity_future_convention::off_peak_hours},
        utility::codec::field{"peak_calendar", &commodity_future_convention::peak_calendar},
        utility::codec::field{"index_name", &commodity_future_convention::index_name},
        utility::codec::field{"savings_time", &commodity_future_convention::savings_time},
        utility::codec::field{"delivery_location", &commodity_future_convention::delivery_location},
        utility::codec::field{"balance_of_the_month",
                              &commodity_future_convention::balance_of_the_month},
        utility::codec::field{"balance_of_the_month_pricing_calendar",
                              &commodity_future_convention::balance_of_the_month_pricing_calendar},
        utility::codec::field{"option_underlying_future_convention",
                              &commodity_future_convention::option_underlying_future_convention},
        utility::codec::field{"averaging_commodity_name",
                              &commodity_future_convention::averaging_commodity_name},
        utility::codec::field{"averaging_period", &commodity_future_convention::averaging_period},
        utility::codec::field{"averaging_pricing_calendar",
                              &commodity_future_convention::averaging_pricing_calendar},
        utility::codec::field{"averaging_conventions",
                              &commodity_future_convention::averaging_conventions},
        utility::codec::field{"averaging_use_business_days",
                              &commodity_future_convention::averaging_use_business_days},
        utility::codec::field{"averaging_delivery_roll_days",
                              &commodity_future_convention::averaging_delivery_roll_days},
        utility::codec::field{"averaging_future_month_offset",
                              &commodity_future_convention::averaging_future_month_offset},
        utility::codec::field{"averaging_daily_expiry_offset",
                              &commodity_future_convention::averaging_daily_expiry_offset},
        utility::codec::field{"prohibited_expiries",
                              &commodity_future_convention::prohibited_expiries},
        utility::codec::field{"future_continuation_mappings",
                              &commodity_future_convention::future_continuation_mappings},
        utility::codec::field{"option_continuation_mappings",
                              &commodity_future_convention::option_continuation_mappings},
        utility::codec::field{"oresmd_uri", &commodity_future_convention::oresmd_uri},
        utility::codec::field{"modified_by", &commodity_future_convention::modified_by},
        utility::codec::field{"performed_by", &commodity_future_convention::performed_by},
        utility::codec::field{"change_reason_code",
                              &commodity_future_convention::change_reason_code},
        utility::codec::field{"change_commentary", &commodity_future_convention::change_commentary},
        utility::codec::field{"recorded_at", &commodity_future_convention::recorded_at},
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
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::domain::commodity_future_convention> {
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

    static std::string write_json(const ores::refdata::domain::commodity_future_convention& v);

    static ores::refdata::domain::commodity_future_convention read_json(std::string_view text);

    static std::string write_msgpack(const ores::refdata::domain::commodity_future_convention& v);

    static ores::refdata::domain::commodity_future_convention
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
