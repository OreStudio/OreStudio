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
#include "ores.analytics.api/generators/pricing_engine_type_generator.hpp"

#include "ores.utility/generation/generation_keys.hpp"

namespace ores::analytics::generators {

using ores::utility::generation::generation_keys;

std::vector<domain::pricing_engine_type> generate_fictional_pricing_engine_types(
    std::size_t n, utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        generation_keys::modified_by, "system");
    const auto now = ctx.past_timepoint();

    std::vector<domain::pricing_engine_type> all;
    all.reserve(20);

    all.push_back({
        .code = "TestEuropeanVanilla",
        .description = "Fictional European vanilla option pricer",
        .instrument_type_code = "",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .code = "TestBermudanSlab",
        .description = "Fictional Bermudan slab option pricer",
        .instrument_type_code = "",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .code = "TestAmericanCage",
        .description = "Fictional American cage option pricer",
        .instrument_type_code = "",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .code = "TestVanillaSwaplet",
        .description = "Fictional vanilla swaplet pricer",
        .instrument_type_code = "",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .code = "TestCrossCurrencyBasis",
        .description = "Fictional cross-currency basis pricer",
        .instrument_type_code = "",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .code = "TestInflationFloor",
        .description = "Fictional inflation floor pricer",
        .instrument_type_code = "",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .code = "TestCreditDefault",
        .description = "Fictional credit default pricer",
        .instrument_type_code = "",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .code = "TestEquityBarrier",
        .description = "Fictional equity barrier option pricer",
        .instrument_type_code = "",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .code = "TestFxDigital",
        .description = "Fictional FX digital option pricer",
        .instrument_type_code = "",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .code = "TestCommodityForward",
        .description = "Fictional commodity forward pricer",
        .instrument_type_code = "",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });

    if (n == 0 || n >= all.size())
        return all;

    return std::vector<domain::pricing_engine_type>(all.begin(), all.begin() + n);
}

}
