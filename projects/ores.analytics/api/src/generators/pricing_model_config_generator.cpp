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
#include <boost/uuid/uuid_io.hpp>
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

    // Names embed part of the UUID to ensure uniqueness across parallel test runs,
    // since scoped_database_helper does not roll back data between test cases.
    auto make_config = [&](const std::string& base_name,
                           const std::string& description,
                           std::optional<std::string> variant) {
        const auto id = gen();
        const auto suffix = boost::uuids::to_string(id).substr(0, 8);
        return domain::pricing_model_config{
            .id = id,
            .name = base_name + "." + suffix,
            .description = description,
            .config_variant = variant,
            .modified_by = modified_by,
            .change_reason_code = "system.test",
            .change_commentary = "Synthetic test data",
            .recorded_at = now
        };
    };

    all.push_back(make_config("test.base.vanilla",
        "Fictional base vanilla pricing configuration", std::nullopt));
    all.push_back(make_config("test.amc.monte.carlo",
        "Fictional AMC Monte Carlo pricing configuration",
        std::optional<std::string>("amc")));
    all.push_back(make_config("test.dynamic.grid",
        "Fictional dynamic grid pricing configuration",
        std::optional<std::string>("dg")));
    all.push_back(make_config("test.standard.rates",
        "Fictional standard rates pricing configuration",
        std::optional<std::string>("standard")));
    all.push_back(make_config("test.equity.vol",
        "Fictional equity volatility pricing configuration", std::nullopt));

    if (n == 0 || n >= all.size())
        return all;

    return std::vector<domain::pricing_model_config>(all.begin(), all.begin() + n);
}

}
