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
#ifndef ORES_VARIABILITY_SERVICE_SYSTEM_SETTINGS_SERVICE_HPP
#define ORES_VARIABILITY_SERVICE_SYSTEM_SETTINGS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.variability.api/domain/system_setting.hpp"
#include "ores.variability.core/export.hpp"
#include "ores.variability.core/repository/system_settings_repository.hpp"
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::variability::service {

/**
 * @brief Service for managing typed system settings.
 *
 * Service for managing typed system settings. Provides typed accessors
 * (get_bool, get_int, get_string, get_json) that return compile-time defaults
 * from the registry when a setting is absent from the DB.
 */
class ORES_VARIABILITY_CORE_EXPORT system_settings_service {
private:
    inline static std::string_view logger_name = "ores.variability.service.system_settings_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs a system_settings_service.
     *
     * @param ctx The database context to use for operations.
     * @param tenant_id The tenant identifier used when creating new settings.
     *        Defaults to empty; must be set for convenience set_* methods to work.
     * @param party_id The party identifier scoping settings read/written by
     *        this instance. Defaults to empty, meaning the tenant's system
     *        party (system-/tenant-wide settings). Set to a specific
     *        party's id to read/write that party's own settings (e.g.
     *        onboarding.party). Ignored when tenant_id is empty.
     */
    explicit system_settings_service(database::context ctx,
                                     std::string tenant_id = {},
                                     std::string party_id = {});

    // -------------------------------------------------------------------------
    // Raw access
    // -------------------------------------------------------------------------

    /**
     * @brief Retrieves a single setting by name (latest active version).
     */
    [[nodiscard]] std::optional<domain::system_setting> get(const std::string& name);

    /**
     * @brief Retrieves all currently active settings.
     */
    [[nodiscard]] std::vector<domain::system_setting> get_all();

    /**
     * @brief Saves a setting using the bitemporal update pattern.
     */
    void save(const domain::system_setting& setting);

    /**
     * @brief Logically removes a setting.
     */
    void remove(const std::string& name);

    /**
     * @brief Retrieves all historical versions of a setting.
     */
    [[nodiscard]] std::vector<domain::system_setting> get_history(const std::string& name);

    // -------------------------------------------------------------------------
    // Typed accessors (return registry default if absent from DB)
    // -------------------------------------------------------------------------

    /**
     * @brief Returns the boolean value of a setting.
     *
     * Expects value to be "true" or "false" (case-insensitive).
     * Returns the registry default if the setting is absent.
     */
    [[nodiscard]] bool get_bool(std::string_view name) const;

    /**
     * @brief Returns the integer value of a setting.
     *
     * Returns the registry default if the setting is absent.
     */
    [[nodiscard]] int get_int(std::string_view name) const;

    /**
     * @brief Returns the string value of a setting.
     *
     * Returns the registry default if the setting is absent.
     */
    [[nodiscard]] std::string get_string(std::string_view name) const;

    /**
     * @brief Returns the JSON value of a setting as a raw string.
     *
     * Returns the registry default if the setting is absent.
     * The caller is responsible for parsing the JSON.
     */
    [[nodiscard]] std::string get_json(std::string_view name) const;

    /**
     * @brief Refreshes the in-memory cache by re-reading all settings from the DB.
     */
    void refresh();

    // -------------------------------------------------------------------------
    // Convenience methods
    // -------------------------------------------------------------------------

    [[nodiscard]] bool is_bootstrap_mode_enabled() const;
    void set_bootstrap_mode(bool enabled,
                            std::string_view modified_by,
                            std::string_view change_reason_code,
                            std::string_view change_commentary);

    [[nodiscard]] bool is_user_signups_enabled() const;
    void set_user_signups(bool enabled,
                          std::string_view modified_by,
                          std::string_view change_reason_code,
                          std::string_view change_commentary);

    [[nodiscard]] bool is_signup_requires_authorization_enabled() const;
    void set_signup_requires_authorization(bool enabled,
                                           std::string_view modified_by,
                                           std::string_view change_reason_code,
                                           std::string_view change_commentary);

    [[nodiscard]] bool is_password_validation_disabled() const;

    /**
     * @brief Whether the system provisioner wizard has completed
     * (onboarding.system, system-tenant-scoped).
     */
    [[nodiscard]] bool is_onboarding_system_complete() const;
    void set_onboarding_system_complete(bool complete,
                                        std::string_view modified_by,
                                        std::string_view change_reason_code,
                                        std::string_view change_commentary);

    /**
     * @brief Whether the tenant provisioner wizard has completed for this
     * tenant (onboarding.tenant, tenant-scoped via the system party).
     */
    [[nodiscard]] bool is_onboarding_tenant_complete() const;
    void set_onboarding_tenant_complete(bool complete,
                                       std::string_view modified_by,
                                       std::string_view change_reason_code,
                                       std::string_view change_commentary);

    /**
     * @brief Whether the party provisioner wizard has completed for this
     * party (onboarding.party, party-scoped — construct this instance with
     * the specific party's id to read/write its flag).
     */
    [[nodiscard]] bool is_onboarding_party_complete() const;
    void set_onboarding_party_complete(bool complete,
                                       std::string_view modified_by,
                                       std::string_view change_reason_code,
                                       std::string_view change_commentary);

private:
    void set_bool_setting(std::string_view name,
                          bool value,
                          std::string_view modified_by,
                          std::string_view change_reason_code,
                          std::string_view change_commentary);

    // Cache: name → value text, populated by refresh()
    std::unordered_map<std::string, std::string> cache_;

    database::context ctx_;
    std::string tenant_id_;
    std::string party_id_;
    repository::system_settings_repository repo_;
};

}

#endif
