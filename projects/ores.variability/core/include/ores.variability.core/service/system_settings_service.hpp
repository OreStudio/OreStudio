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
#include "ores.variability.core/repository/system_setting_repository.hpp"
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ores::variability::service {

/**
 * @brief The typed settings interface components read in process.
 *
 * The generated =system_setting_service= speaks the wire protocol: it takes a
 * canonical request and answers a canonical response. That is the wrong shape
 * for a component that reads a setting while it starts, or inside a request
 * handler, and wrong for one whose need is a typed value rather than a row. So
 * this class is hand-written and deliberately small: it holds the typed
 * accessors the in-process consumers call, and the generated repository does
 * the storage work behind them.
 *
 * Two things it does that the generated repository cannot. It reads through
 * =ores_variability_get_system_settings_fn= when a tenant is named, because a
 * service context holds no direct SELECT grant on the settings table, and the
 * SECURITY DEFINER function is the boundary that lets it read at all. And it
 * reads one scope at a time -- a tenant's system party for a tenant-wide
 * setting, or a named party's own row -- because a setting name is unique
 * within a (tenant, party) pair and not within a tenant.
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
     * @param tenant_id The tenant whose settings this instance reads. Empty
     *        means the caller's own context is the scope, which is the
     *        variability service's own case.
     * @param party_id The party scoping this instance. Empty means the
     *        tenant's system party, which is where tenant-wide settings live.
     *        Set to a party's own id to read and write that party's settings,
     *        such as =onboarding.party=.
     */
    explicit system_settings_service(database::context ctx,
                                     std::string tenant_id = {},
                                     std::string party_id = {});

    /**
     * @brief Retrieves all currently active settings in this instance's scope.
     */
    [[nodiscard]] std::vector<domain::system_setting> get_all();

    /**
     * @brief Returns the boolean value of a setting.
     *
     * Expects "true" or "false", case-insensitively; also accepts "1" and "0".
     * Returns the registered default when the setting is absent.
     */
    [[nodiscard]] bool get_bool(std::string_view name) const;

    /**
     * @brief Returns the integer value of a setting.
     *
     * Returns the registered default when the setting is absent.
     */
    [[nodiscard]] int get_int(std::string_view name) const;

    /**
     * @brief Returns the string value of a setting.
     *
     * Returns the registered default when the setting is absent.
     */
    [[nodiscard]] std::string get_string(std::string_view name) const;

    /**
     * @brief Returns a setting's value as it was last read, or nothing.
     *
     * For a setting this component does not know about there is no registered
     * default to fall back on, so a consumer that has its own default reads the
     * value here and applies it. Nothing means the setting was absent when this
     * instance last refreshed.
     */
    [[nodiscard]] std::optional<std::string> get(std::string_view name) const;

    /**
     * @brief Re-reads every setting in this instance's scope.
     *
     * The typed accessors answer from this cache, so a caller that wants a
     * change to take effect without a restart calls this first.
     */
    void refresh();

    [[nodiscard]] bool is_bootstrap_mode_enabled() const;
    void set_bootstrap_mode(bool enabled,
                            std::string_view modified_by,
                            std::string_view change_reason_code,
                            std::string_view change_commentary);

    [[nodiscard]] bool is_user_signups_enabled() const;

    [[nodiscard]] bool is_signup_requires_authorization_enabled() const;

    /**
     * @brief Whether the party provisioner wizard has completed for this
     * party (=onboarding.party=). Construct this instance with the party's own
     * id to read its flag.
     */
    [[nodiscard]] bool is_onboarding_party_complete() const;

    /**
     * @brief Whether the system provisioner wizard has completed for this
     * tenant (=onboarding.system=, tenant-wide, under the tenant's system
     * party). Construct this instance with the tenant's own id and no party.
     */
    [[nodiscard]] bool is_onboarding_system_complete() const;

    /**
     * @brief Marks the tenant provisioner wizard complete
     * (=onboarding.tenant=, tenant-wide).
     */
    void set_onboarding_tenant_complete(bool complete,
                                        std::string_view modified_by,
                                        std::string_view change_reason_code,
                                        std::string_view change_commentary);

    /**
     * @brief Marks this party's provisioner wizard complete
     * (=onboarding.party=, scoped to the party this instance was built with).
     */
    void set_onboarding_party_complete(bool complete,
                                       std::string_view modified_by,
                                       std::string_view change_reason_code,
                                       std::string_view change_commentary);

    /**
     * @brief Marks the system provisioner wizard complete
     * (=onboarding.system=, tenant-wide, under the tenant's system party).
     */
    void set_onboarding_system_complete(bool complete,
                                        std::string_view modified_by,
                                        std::string_view change_reason_code,
                                        std::string_view change_commentary);

private:
    /**
     * @brief Writes one setting into this instance's scope.
     *
     * A setting is addressed by its name within a scope, and the store keeps a
     * surrogate id, so the row already carrying this name and party is read
     * first: the write replaces that row rather than colliding with it.
     */
    void save(const domain::system_setting& setting);

    void set_bool_setting(std::string_view name,
                          bool value,
                          std::string_view modified_by,
                          std::string_view change_reason_code,
                          std::string_view change_commentary);

    // The scope's settings, by name, as of the last refresh().
    std::unordered_map<std::string, std::string> cache_;

    database::context ctx_;
    std::string tenant_id_;
    std::string party_id_;
    repository::system_setting_repository repo_;
};

}

#endif
