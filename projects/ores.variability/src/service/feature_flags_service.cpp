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

#include "ores.variability/service/feature_flags_service.hpp"

#include <algorithm>

namespace ores::variability::service {

using namespace ores::logging;

feature_flags_service::feature_flags_service(database::context ctx)
    : repo_(std::move(ctx)) {}

std::optional<domain::feature_flags>
feature_flags_service::get_feature_flag(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting feature flag: " << name;

    auto flags = repo_.read_latest(name);
    if (flags.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "Feature flag not found: " << name;
        return std::nullopt;
    }

    if (flags.size() > 1) {
        BOOST_LOG_SEV(lg(), warn) << "Found " << flags.size() << " latest feature flags for name '" << name << "', expected 1. Using the first one.";
    }
    return flags.front();
}

std::vector<domain::feature_flags> feature_flags_service::get_all_feature_flags() {
    BOOST_LOG_SEV(lg(), debug) << "Getting all feature flags";
    return repo_.read_latest();
}

void feature_flags_service::save_feature_flag(const domain::feature_flags& flag) {
    BOOST_LOG_SEV(lg(), info) << "Saving feature flag: " << flag.name
                              << ", enabled: " << (flag.enabled ? "true" : "false");

    // Bitemporal update pattern:
    // 1. Remove existing entry (close validity)
    // 2. Write new entry
    // 'remove' is idempotent in the repository (if it doesn't exist, nothing happens).
    repo_.remove(flag.name);
    repo_.write(flag);
}

void feature_flags_service::delete_feature_flag(const std::string& name) {
    BOOST_LOG_SEV(lg(), info) << "Deleting feature flag: " << name;
    repo_.remove(name);
}

std::vector<domain::feature_flags>
feature_flags_service::get_feature_flag_history(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting feature flag history for: " << name;
    auto history = repo_.read_all(name);

    // Sort by version descending (newest first)
    std::ranges::sort(history, [](const auto& a, const auto& b) {
        return a.version > b.version;
    });

    return history;
}

} // namespace ores::variability::service
