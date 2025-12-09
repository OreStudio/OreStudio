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

#include "ores.variability/service/system_flags_service.hpp"

namespace ores::variability::service {

using namespace ores::utility::log;

system_flags_service::system_flags_service(utility::database::context ctx)
    : feature_flags_service_(std::move(ctx)) {}

bool system_flags_service::is_enabled(domain::system_flag flag) {
    const auto flag_name = domain::to_flag_name(flag);
    BOOST_LOG_SEV(lg(), debug) << "Checking system flag: " << flag_name;

    auto flag_opt = feature_flags_service_.get_feature_flag(flag_name);
    if (!flag_opt.has_value()) {
        const auto& def = domain::get_definition(flag);
        BOOST_LOG_SEV(lg(), debug) << "System flag " << flag_name
            << " not found, using default: "
            << (def.default_enabled ? "true" : "false");
        return def.default_enabled;
    }

    BOOST_LOG_SEV(lg(), debug) << "System flag " << flag_name << " is "
        << (flag_opt->enabled ? "enabled" : "disabled");
    return flag_opt->enabled;
}

void system_flags_service::set_enabled(domain::system_flag flag, bool enabled,
    std::string_view modified_by) {
    const auto flag_name = domain::to_flag_name(flag);
    const auto& def = domain::get_definition(flag);

    BOOST_LOG_SEV(lg(), info) << "Setting system flag " << flag_name
        << " to " << (enabled ? "enabled" : "disabled")
        << " by " << modified_by;

    domain::feature_flags ff{
        .enabled = enabled,
        .name = flag_name,
        .description = std::string(def.description),
        .modified_by = std::string(modified_by)
    };

    feature_flags_service_.save_feature_flag(ff);
}

bool system_flags_service::is_bootstrap_mode_enabled() {
    return is_enabled(domain::system_flag::bootstrap_mode);
}

void system_flags_service::set_bootstrap_mode(bool enabled,
    std::string_view modified_by) {
    set_enabled(domain::system_flag::bootstrap_mode, enabled, modified_by);
}

bool system_flags_service::is_user_signups_enabled() {
    return is_enabled(domain::system_flag::user_signups);
}

void system_flags_service::set_user_signups(bool enabled,
    std::string_view modified_by) {
    set_enabled(domain::system_flag::user_signups, enabled, modified_by);
}

}
