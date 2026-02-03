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

using namespace ores::logging;

system_flags_service::system_flags_service(database::context ctx,
    std::string tenant_id)
    : feature_flags_service_(std::move(ctx)),
      tenant_id_(std::move(tenant_id)) {}

void system_flags_service::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Refreshing system flags cache from database";

    for (const auto& def : domain::system_flag_definitions) {
        const auto flag_name = domain::to_flag_name(def.flag);
        auto flag_opt = feature_flags_service_.get_feature_flag(flag_name);

        bool enabled = flag_opt.has_value() ? flag_opt->enabled : def.default_enabled;
        update_cache(def.flag, enabled);

        BOOST_LOG_SEV(lg(), debug) << "Cached " << flag_name << " = "
            << (enabled ? "enabled" : "disabled");
    }

    BOOST_LOG_SEV(lg(), info) << "System flags cache refreshed";
}

const domain::system_flags_cache& system_flags_service::cache() const {
    return cache_;
}

bool system_flags_service::is_enabled(domain::system_flag flag) const {
    switch (flag) {
    case domain::system_flag::bootstrap_mode:
        return cache_.bootstrap_mode;
    case domain::system_flag::user_signups:
        return cache_.user_signups;
    case domain::system_flag::signup_requires_authorization:
        return cache_.signup_requires_authorization;
    case domain::system_flag::disable_password_validation:
        return cache_.disable_password_validation;
    }
    // Should never reach here for valid enum values
    return false;
}

void system_flags_service::set_enabled(domain::system_flag flag, bool enabled,
    std::string_view recorded_by,
    std::string_view change_reason_code,
    std::string_view change_commentary) {
    const auto flag_name = domain::to_flag_name(flag);
    const auto& def = domain::get_definition(flag);

    BOOST_LOG_SEV(lg(), info) << "Setting system flag " << flag_name
        << " to " << (enabled ? "enabled" : "disabled")
        << " by " << recorded_by;

    domain::feature_flags ff{
        .tenant_id = tenant_id_,
        .enabled = enabled,
        .name = flag_name,
        .description = std::string(def.description),
        .recorded_by = std::string(recorded_by),
        .change_reason_code = std::string(change_reason_code),
        .change_commentary = std::string(change_commentary)
    };

    feature_flags_service_.save_feature_flag(ff);
    update_cache(flag, enabled);
}

bool system_flags_service::is_bootstrap_mode_enabled() const {
    return cache_.bootstrap_mode;
}

void system_flags_service::set_bootstrap_mode(bool enabled,
    std::string_view recorded_by,
    std::string_view change_reason_code,
    std::string_view change_commentary) {
    set_enabled(domain::system_flag::bootstrap_mode, enabled, recorded_by,
        change_reason_code, change_commentary);
}

bool system_flags_service::is_user_signups_enabled() const {
    return cache_.user_signups;
}

void system_flags_service::set_user_signups(bool enabled,
    std::string_view recorded_by,
    std::string_view change_reason_code,
    std::string_view change_commentary) {
    set_enabled(domain::system_flag::user_signups, enabled, recorded_by,
        change_reason_code, change_commentary);
}

bool system_flags_service::is_signup_requires_authorization_enabled() const {
    return cache_.signup_requires_authorization;
}

void system_flags_service::set_signup_requires_authorization(bool enabled,
    std::string_view recorded_by,
    std::string_view change_reason_code,
    std::string_view change_commentary) {
    set_enabled(domain::system_flag::signup_requires_authorization, enabled,
        recorded_by, change_reason_code, change_commentary);
}

void system_flags_service::update_cache(domain::system_flag flag, bool enabled) {
    switch (flag) {
    case domain::system_flag::bootstrap_mode:
        cache_.bootstrap_mode = enabled;
        break;
    case domain::system_flag::user_signups:
        cache_.user_signups = enabled;
        break;
    case domain::system_flag::signup_requires_authorization:
        cache_.signup_requires_authorization = enabled;
        break;
    case domain::system_flag::disable_password_validation:
        cache_.disable_password_validation = enabled;
        break;
    }
}

}
