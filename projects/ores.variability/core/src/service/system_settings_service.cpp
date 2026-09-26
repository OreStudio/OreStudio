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

#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.variability.api/domain/system_settings.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <stdexcept>
#include <utility>

namespace ores::variability::service {

using namespace ores::logging;
using ores::database::repository::execute_parameterized_multi_column_query;

namespace {

// The scoped read returns the row's id and party beside its value, because a
// caller that writes a setting back must name the row it replaces, and the
// party the database resolved for it is not a value the caller ever stated.
constexpr auto tenant_scope_sql = "select setting_id, setting_name, setting_party_id, "
                                  "setting_value, setting_data_type, setting_description "
                                  "from ores_variability_get_system_settings_fn($1)";

constexpr auto party_scope_sql = "select setting_id, setting_name, setting_party_id, "
                                 "setting_value, setting_data_type, setting_description "
                                 "from ores_variability_get_system_settings_fn($1, $2)";

domain::system_setting row_to_setting(const std::vector<std::optional<std::string>>& row) {
    domain::system_setting s;
    if (row.size() < 6)
        return s;

    const boost::uuids::string_generator parse;
    if (row[0])
        s.id = parse(*row[0]);
    if (row[1])
        s.name = *row[1];
    if (row[2])
        s.party_id = parse(*row[2]);
    if (row[3])
        s.value = *row[3];
    if (row[4])
        s.data_type = *row[4];
    if (row[5])
        s.description = *row[5];
    return s;
}

} // namespace

system_settings_service::system_settings_service(database::context ctx,
                                                 std::string tenant_id,
                                                 std::string party_id)
    : ctx_(std::move(ctx))
    , tenant_id_(std::move(tenant_id))
    , party_id_(std::move(party_id)) {}

std::vector<domain::system_setting> system_settings_service::get_all() {
    if (tenant_id_.empty()) {
        // The variability service's own context: it holds a direct SELECT
        // grant on the table, so no definer function is needed.
        return repo_.read_latest(ctx_);
    }

    BOOST_LOG_SEV(lg(), debug) << "Reading system settings for tenant " << tenant_id_
                               << " party " << (party_id_.empty() ? "(system)" : party_id_);

    const auto rows = party_id_.empty()
        ? execute_parameterized_multi_column_query(ctx_,
                                                   tenant_scope_sql,
                                                   {tenant_id_},
                                                   lg(),
                                                   "Reading system settings for one tenant")
        : execute_parameterized_multi_column_query(ctx_,
                                                   party_scope_sql,
                                                   {tenant_id_, party_id_},
                                                   lg(),
                                                   "Reading system settings for one party");

    std::vector<domain::system_setting> settings;
    settings.reserve(rows.size());
    for (const auto& row : rows)
        settings.push_back(row_to_setting(row));
    return settings;
}

void system_settings_service::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Refreshing the system settings cache";
    cache_.clear();
    for (const auto& setting : get_all())
        cache_[setting.name] = setting.value;
    BOOST_LOG_SEV(lg(), info) << "System settings cache refreshed. Count: " << cache_.size();
}

void system_settings_service::save(const domain::system_setting& setting) {
    if (setting.name.empty())
        throw std::invalid_argument("System setting name cannot be empty.");

    auto value = setting;

    // A caller addresses a setting by its name within a scope, and the store
    // keeps a surrogate id, so the row already carrying this name is read first
    // and its id taken. A write with a fresh id would collide with the live row
    // on the composite unique index instead of superseding it.
    if (value.id.is_nil()) {
        for (const auto& row : get_all()) {
            if (row.name != value.name)
                continue;
            if (!value.party_id.is_nil() && row.party_id != value.party_id)
                continue;
            value.id = row.id;
            value.version = row.version;
            value.party_id = row.party_id;
            break;
        }
    }
    if (value.id.is_nil())
        value.id = boost::uuids::random_generator()();

    BOOST_LOG_SEV(lg(), info) << "Saving system setting: " << value.name << " = " << value.value
                              << " (" << value.data_type << ") with version " << value.version;

    repo_.write(ctx_, value);
    cache_[value.name] = value.value;
}

bool system_settings_service::get_bool(std::string_view name) const {
    auto it = cache_.find(std::string(name));
    const auto raw =
        (it != cache_.end()) ? it->second : std::string(domain::get_setting_default(name));

    // Accept "true"/"false" case-insensitively; also "1"/"0".
    if (raw == "true" || raw == "1")
        return true;
    if (raw == "false" || raw == "0")
        return false;

    BOOST_LOG_SEV(lg(), warn) << "Unexpected boolean value '" << raw << "' for setting '" << name
                              << "'. Returning false.";
    return false;
}

int system_settings_service::get_int(std::string_view name) const {
    auto it = cache_.find(std::string(name));
    const auto raw =
        (it != cache_.end()) ? it->second : std::string(domain::get_setting_default(name));

    try {
        return std::stoi(raw);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to parse integer value '" << raw << "' for setting '"
                                  << name << "': " << e.what() << ". Returning 0.";
        return 0;
    }
}

std::string system_settings_service::get_string(std::string_view name) const {
    auto it = cache_.find(std::string(name));
    if (it != cache_.end())
        return it->second;
    return std::string(domain::get_setting_default(name));
}

std::optional<std::string> system_settings_service::get(std::string_view name) const {
    auto it = cache_.find(std::string(name));
    if (it == cache_.end())
        return std::nullopt;
    return it->second;
}

void system_settings_service::set_bool_setting(std::string_view name,
                                               bool value,
                                               std::string_view modified_by,
                                               std::string_view change_reason_code,
                                               std::string_view change_commentary) {
    const auto& def = domain::get_setting_definition(name);

    domain::system_setting s;
    if (!tenant_id_.empty()) {
        const auto tenant = utility::uuid::tenant_id::from_string(tenant_id_);
        if (tenant)
            s.tenant_id = *tenant;
    }
    if (!party_id_.empty())
        s.party_id = boost::uuids::string_generator()(party_id_);
    s.name = std::string(name);
    s.value = value ? "true" : "false";
    s.data_type = std::string(def.data_type);
    s.description = std::string(def.description);
    s.modified_by = std::string(modified_by);
    s.change_reason_code = std::string(change_reason_code);
    s.change_commentary = std::string(change_commentary);

    save(s);
}

bool system_settings_service::is_bootstrap_mode_enabled() const {
    return get_bool("system.bootstrap_mode");
}

void system_settings_service::set_bootstrap_mode(bool enabled,
                                                 std::string_view modified_by,
                                                 std::string_view change_reason_code,
                                                 std::string_view change_commentary) {
    set_bool_setting("system.bootstrap_mode",
                     enabled,
                     modified_by,
                     change_reason_code,
                     change_commentary);
}

bool system_settings_service::is_user_signups_enabled() const {
    return get_bool("system.user_signups");
}

bool system_settings_service::is_signup_requires_authorization_enabled() const {
    return get_bool("system.signup_requires_authorization");
}

bool system_settings_service::is_onboarding_party_complete() const {
    return get_bool("onboarding.party");
}

void system_settings_service::set_onboarding_tenant_complete(bool complete,
                                                             std::string_view modified_by,
                                                             std::string_view change_reason_code,
                                                             std::string_view change_commentary) {
    set_bool_setting("onboarding.tenant",
                     complete,
                     modified_by,
                     change_reason_code,
                     change_commentary);
}

void system_settings_service::set_onboarding_party_complete(bool complete,
                                                            std::string_view modified_by,
                                                            std::string_view change_reason_code,
                                                            std::string_view change_commentary) {
    set_bool_setting("onboarding.party",
                     complete,
                     modified_by,
                     change_reason_code,
                     change_commentary);
}

}
