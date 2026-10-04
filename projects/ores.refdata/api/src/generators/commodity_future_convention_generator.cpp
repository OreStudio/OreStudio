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
 * Template: cpp_domain_type_generator.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.refdata.api/generators/commodity_future_convention_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

domain::commodity_future_convention
generate_synthetic_commodity_future_convention(utility::generation::generation_context& ctx) {
    [[maybe_unused]] static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::commodity_future_convention r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.workspace_id = utility::uuid::live_workspace_id();
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.id = std::string("COMDTY_WTI_USD") + "-" + std::to_string(idx);
    r.party_id = ctx.generate_uuid();
    r.contract_frequency = std::string("Monthly");
    r.calendar = std::string("ICE_FuturesUS");
    r.expiry_calendar = std::nullopt;
    r.expiry_month_lag = std::nullopt;
    r.one_contract_month = std::nullopt;
    r.offset_days = std::nullopt;
    r.business_day_convention = std::nullopt;
    r.adjust_before_offset = std::nullopt;
    r.is_averaging = std::nullopt;
    r.valid_contract_months = std::nullopt;
    r.anchor_day_of_month = std::nullopt;
    r.anchor_calendar_days_before = std::nullopt;
    r.anchor_business_days_after = std::nullopt;
    r.anchor_nth_nth = std::nullopt;
    r.anchor_nth_weekday = std::nullopt;
    r.anchor_last_weekday = std::nullopt;
    r.anchor_weekly_day_of_the_week = std::nullopt;
    r.option_expiry_month_lag = std::nullopt;
    r.option_contract_frequency = std::nullopt;
    r.option_expiry_offset = std::nullopt;
    r.option_calendar_days_before = std::nullopt;
    r.option_min_business_days_before = std::nullopt;
    r.option_expiry_day = std::nullopt;
    r.option_nth_nth = std::nullopt;
    r.option_nth_weekday = std::nullopt;
    r.option_expiry_last_weekday_of_month = std::nullopt;
    r.option_expiry_weekly_day_of_the_week = std::nullopt;
    r.option_business_day_convention = std::nullopt;
    r.hours_per_day = std::nullopt;
    r.off_peak_index = std::nullopt;
    r.peak_index = std::nullopt;
    r.off_peak_hours = std::nullopt;
    r.peak_calendar = std::nullopt;
    r.index_name = std::nullopt;
    r.savings_time = std::nullopt;
    r.delivery_location = std::nullopt;
    r.balance_of_the_month = std::nullopt;
    r.balance_of_the_month_pricing_calendar = std::nullopt;
    r.option_underlying_future_convention = std::nullopt;
    r.averaging_commodity_name = std::nullopt;
    r.averaging_period = std::nullopt;
    r.averaging_pricing_calendar = std::nullopt;
    r.averaging_conventions = std::nullopt;
    r.averaging_use_business_days = std::nullopt;
    r.averaging_delivery_roll_days = std::nullopt;
    r.averaging_future_month_offset = std::nullopt;
    r.averaging_daily_expiry_offset = std::nullopt;
    r.prohibited_expiries = std::nullopt;
    r.future_continuation_mappings = std::nullopt;
    r.option_continuation_mappings = std::nullopt;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::commodity_future_convention>
generate_synthetic_commodity_future_conventions(std::size_t n,
                                                utility::generation::generation_context& ctx) {
    std::vector<domain::commodity_future_convention> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_commodity_future_convention(ctx));
    return r;
}

}
