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
#include "ores.analytics.api/generators/pricing_model_config_generator.hpp"

#include <boost/uuid/random_generator.hpp>
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::analytics::generators {

using ores::utility::generation::generation_keys;

std::vector<domain::pricing_model_config> generate_fictional_pricing_model_configs(
    std::size_t n, utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        generation_keys::modified_by, "system");
    const auto now = ctx.past_timepoint();
    boost::uuids::random_generator gen;

    std::vector<domain::pricing_model_config> all;
    all.reserve(10);

    all.push_back({
        .id = gen(),
        .name = "test.base.vanilla",
        .description = "Fictional base vanilla pricing configuration",
        .config_variant = std::nullopt,
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .id = gen(),
        .name = "test.amc.monte.carlo",
        .description = "Fictional AMC Monte Carlo pricing configuration",
        .config_variant = std::optional<std::string>("amc"),
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .id = gen(),
        .name = "test.dynamic.grid",
        .description = "Fictional dynamic grid pricing configuration",
        .config_variant = std::optional<std::string>("dg"),
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .id = gen(),
        .name = "test.standard.rates",
        .description = "Fictional standard rates pricing configuration",
        .config_variant = std::optional<std::string>("standard"),
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    all.push_back({
        .id = gen(),
        .name = "test.equity.vol",
        .description = "Fictional equity volatility pricing configuration",
        .config_variant = std::nullopt,
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });

    if (n == 0 || n >= all.size())
        return all;

    return std::vector<domain::pricing_model_config>(all.begin(), all.begin() + n);
}

}
