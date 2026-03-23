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
#include "ores.variability.core/service/system_settings_service.hpp"

#include <algorithm>
#include <stdexcept>
#include "ores.variability.api/domain/system_settings.hpp"

namespace ores::variability::service {

using namespace ores::logging;

system_settings_service::system_settings_service(database::context ctx,
    std::string tenant_id)
    : ctx_(std::move(ctx)), tenant_id_(std::move(tenant_id)) {}

std::optional<domain::system_setting>
system_settings_service::get(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting system setting: " << name;

    auto settings = repo_.read_latest(ctx_, name);
    if (settings.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "System setting not found: " << name;
        return std::nullopt;
    }

    if (settings.size() > 1) {
        BOOST_LOG_SEV(lg(), warn) << "Found " << settings.size()
            << " active settings for name '" << name << "', using first.";
    }
    return settings.front();
}

std::vector<domain::system_setting> system_settings_service::get_all() {
    BOOST_LOG_SEV(lg(), debug) << "Getting all system settings";
    return repo_.read_latest(ctx_);
}

void system_settings_service::save(const domain::system_setting& setting) {
    if (setting.name.empty())
        throw std::invalid_argument("System setting name cannot be empty.");

    BOOST_LOG_SEV(lg(), info) << "Saving system setting: " << setting.name
                              << " = " << setting.value
                              << " (" << setting.data_type << ")";

    // Bitemporal update: close existing then write new version.
    repo_.remove(ctx_, setting.name);
    repo_.write(ctx_, setting);

    // Update cache
    cache_[setting.name] = setting.value;
}

void system_settings_service::remove(const std::string& name) {
    BOOST_LOG_SEV(lg(), info) << "Removing system setting: " << name;
    repo_.remove(ctx_, name);
    cache_.erase(name);
}

std::vector<domain::system_setting>
system_settings_service::get_history(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for system setting: " << name;
    auto history = repo_.read_all(ctx_, name);

    std::ranges::sort(history, [](const auto& a, const auto& b) {
        return a.version > b.version;
    });
    return history;
}

void system_settings_service::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Refreshing system settings cache from database";
    cache_.clear();

    for (const auto& s : repo_.read_latest(ctx_))
        cache_[s.name] = s.value;

    BOOST_LOG_SEV(lg(), info) << "System settings cache refreshed. Count: "
                              << cache_.size();
}

bool system_settings_service::get_bool(std::string_view name) const {
    auto it = cache_.find(std::string(name));
    const auto raw = (it != cache_.end())
        ? it->second
        : std::string(domain::get_setting_default(name));

    // Accept "true"/"false" case-insensitively; also "1"/"0".
    if (raw == "true" || raw == "1")  return true;
    if (raw == "false" || raw == "0") return false;

    BOOST_LOG_SEV(lg(), warn) << "Unexpected boolean value '" << raw
        << "' for setting '" << name << "'. Returning false.";
    return false;
}

int system_settings_service::get_int(std::string_view name) const {
    auto it = cache_.find(std::string(name));
    const auto raw = (it != cache_.end())
        ? it->second
        : std::string(domain::get_setting_default(name));

    try {
        return std::stoi(raw);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to parse integer value '" << raw
            << "' for setting '" << name << "': " << e.what()
            << ". Returning 0.";
        return 0;
    }
}

std::string system_settings_service::get_string(std::string_view name) const {
    auto it = cache_.find(std::string(name));
    if (it != cache_.end())
        return it->second;
    return std::string(domain::get_setting_default(name));
}

std::string system_settings_service::get_json(std::string_view name) const {
    return get_string(name);
}

// -------------------------------------------------------------------------
// Convenience methods
// -------------------------------------------------------------------------

bool system_settings_service::is_bootstrap_mode_enabled() const {
    return get_bool("system.bootstrap_mode");
}

void system_settings_service::set_bootstrap_mode(bool enabled,
    std::string_view modified_by,
    std::string_view change_reason_code,
    std::string_view change_commentary) {
    set_bool_setting("system.bootstrap_mode", enabled,
        modified_by, change_reason_code, change_commentary);
}

bool system_settings_service::is_user_signups_enabled() const {
    return get_bool("system.user_signups");
}

void system_settings_service::set_user_signups(bool enabled,
    std::string_view modified_by,
    std::string_view change_reason_code,
    std::string_view change_commentary) {
    set_bool_setting("system.user_signups", enabled,
        modified_by, change_reason_code, change_commentary);
}

bool system_settings_service::is_signup_requires_authorization_enabled() const {
    return get_bool("system.signup_requires_authorization");
}

void system_settings_service::set_signup_requires_authorization(bool enabled,
    std::string_view modified_by,
    std::string_view change_reason_code,
    std::string_view change_commentary) {
    set_bool_setting("system.signup_requires_authorization", enabled,
        modified_by, change_reason_code, change_commentary);
}

bool system_settings_service::is_password_validation_disabled() const {
    return get_bool("system.disable_password_validation");
}

void system_settings_service::set_bool_setting(std::string_view name,
    bool value,
    std::string_view modified_by,
    std::string_view change_reason_code,
    std::string_view change_commentary) {
    const auto& def = domain::get_setting_definition(name);

    domain::system_setting s{
        .tenant_id = tenant_id_,
        .name = std::string(name),
        .value = value ? "true" : "false",
        .data_type = std::string(def.data_type),
        .description = std::string(def.description),
        .modified_by = std::string(modified_by),
        .change_reason_code = std::string(change_reason_code),
        .change_commentary = std::string(change_commentary)
    };

    save(s);
}

}
