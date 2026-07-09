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
#ifndef ORES_VARIABILITY_REPOSITORY_SYSTEM_SETTINGS_REPOSITORY_HPP
#define ORES_VARIABILITY_REPOSITORY_SYSTEM_SETTINGS_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.variability.api/domain/system_setting.hpp"
#include "ores.variability.core/export.hpp"
#include <cstdint>
#include <sqlgen/postgres.hpp>
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::variability::repository {

/**
 * @brief Reads and writes system settings from data storage.
 */
class ORES_VARIABILITY_CORE_EXPORT system_settings_repository {
private:
    inline static std::string_view logger_name =
        "ores.variability.repository.system_settings_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes a system setting to the database.
     */
    /**@{*/
    void write(context ctx, const domain::system_setting& setting);
    void write(context ctx, const std::vector<domain::system_setting>& settings);
    /**@}*/

    /**
     * @brief Reads latest settings, possibly filtered by name.
     */
    /**@{*/
    std::vector<domain::system_setting> read_latest(context ctx);
    std::vector<domain::system_setting> read_latest(context ctx, const std::string& name);
    /**@}*/

    /**
     * @brief Reads latest settings with pagination.
     */
    std::vector<domain::system_setting>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active settings.
     */
    std::uint32_t get_total_count(context ctx);

    /**
     * @brief Reads all versions of settings, possibly filtered by name.
     */
    /**@{*/
    std::vector<domain::system_setting> read_all(context ctx);
    std::vector<domain::system_setting> read_all(context ctx, const std::string& name);
    /**@}*/

    /**
     * @brief Reads active settings for a tenant via a SECURITY DEFINER function.
     *
     * Returns a name→value map by calling ores_variability_get_system_settings_fn.
     * Used by service contexts that hold no direct SELECT grant on the table.
     */
    std::unordered_map<std::string, std::string> read_for_tenant(context ctx,
                                                                 const std::string& tenant_id);

    /**
     * @brief Reads active settings scoped to one (tenant, party) pair via a
     * SECURITY DEFINER function.
     *
     * Unlike read_for_tenant (which defaults to the tenant's system party),
     * this always scopes to the given party_id — used for party-specific
     * flags such as onboarding.party.
     */
    std::unordered_map<std::string, std::string>
    read_for_party(context ctx, const std::string& tenant_id, const std::string& party_id);

    /**
     * @brief Logically deletes a setting by closing its temporal validity.
     */
    void remove(context ctx, const std::string& name);
};

}

#endif
