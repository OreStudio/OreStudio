/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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

#include "ores.variability/service/system_flags_seeder.hpp"
#include "ores.variability/domain/system_flags.hpp"

namespace ores::variability::service {

using namespace ores::utility::log;

system_flags_seeder::system_flags_seeder(database::context ctx)
    : feature_flags_service_(std::move(ctx)) {}

std::size_t system_flags_seeder::seed() {
    BOOST_LOG_SEV(lg(), info) << "Seeding system flags...";

    std::size_t created_count = 0;
    std::size_t skipped_count = 0;

    for (const auto& def : domain::system_flag_definitions) {
        const auto flag_name = domain::to_flag_name(def.flag);

        auto existing = feature_flags_service_.get_feature_flag(flag_name);
        if (existing.has_value()) {
            BOOST_LOG_SEV(lg(), debug) << "System flag already exists: "
                                       << flag_name;
            ++skipped_count;
            continue;
        }

        BOOST_LOG_SEV(lg(), debug) << "Creating system flag: " << flag_name
                                   << " (default: "
                                   << (def.default_enabled ? "enabled" : "disabled")
                                   << ")";

        domain::feature_flags ff {
            .enabled = def.default_enabled,
            .name = flag_name,
            .description = std::string(def.description),
            .recorded_by = "system"
        };

        feature_flags_service_.save_feature_flag(ff);
        ++created_count;
    }

    BOOST_LOG_SEV(lg(), info) << "System flags seeded: " << created_count
                              << " created, " << skipped_count << " skipped.";

    return created_count;
}

}
