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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_CORE_SERVICE_CRM_TOPOLOGY_CONFIG_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CRM_TOPOLOGY_CONFIG_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/crm_topology_config.hpp"
#include "ores.refdata.api/messaging/crm_topology_config_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/crm_topology_config_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing CRM topology configs.
 *
 * Provides a higher-level interface for CRM topology config operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT crm_topology_config_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.crm_topology_config_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a crm_topology_config_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit crm_topology_config_service(context ctx);

    /**
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_crm_topology_configs_response
    list_crm_topology_configs(const messaging::list_crm_topology_configs_request& request);
    messaging::get_crm_topology_config_response
    get_crm_topology_config(const messaging::get_crm_topology_config_request& request);
    messaging::get_many_crm_topology_configs_response
    get_many_crm_topology_configs(const messaging::get_many_crm_topology_configs_request& request);
    messaging::put_crm_topology_config_response
    put_crm_topology_config(const messaging::put_crm_topology_config_request& request);
    messaging::put_many_crm_topology_configs_response
    put_many_crm_topology_configs(const messaging::put_many_crm_topology_configs_request& request);
    messaging::delete_crm_topology_config_response
    delete_crm_topology_config(const messaging::delete_crm_topology_config_request& request);
    messaging::delete_many_crm_topology_configs_response delete_many_crm_topology_configs(
        const messaging::delete_many_crm_topology_configs_request& request);
    messaging::list_crm_topology_config_versions_response list_crm_topology_config_versions(
        const messaging::list_crm_topology_config_versions_request& request);
    messaging::get_crm_topology_config_version_response get_crm_topology_config_version(
        const messaging::get_crm_topology_config_version_request& request);
    /**@}*/

    /**
     * @brief Lists CRM topology configs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of CRM topology configs for the requested page.
     */
    std::vector<domain::crm_topology_config> list_crm_topology_configs(std::uint32_t offset,
                                                                       std::uint32_t limit);

    /**
     * @brief Gets the total count of active CRM topology configs.
     *
     * @return Total number of active CRM topology configs.
     */
    std::uint32_t count_crm_topology_configs();


    /**
     * @brief Retrieves a single CRM topology config as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The CRM topology config at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::crm_topology_config>
    get_crm_topology_config_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single CRM topology config by its primary key.
     *
     * @return The CRM topology config if found, std::nullopt otherwise.
     */
    std::optional<domain::crm_topology_config> get_crm_topology_config(const std::string& id);

    /**
     * @brief Retrieves a batch of CRM topology configs by primary key.
     */
    std::vector<domain::crm_topology_config>
    get_crm_topology_configs(const std::vector<std::string>& ids);

    /**
     * @brief Saves a CRM topology config (creates or updates).
     *
     * @param crm_topology_config The CRM topology config to save.
     * @throws std::exception on failure.
     */
    void save_crm_topology_config(const domain::crm_topology_config& crm_topology_config);

    /**
     * @brief Saves a batch of CRM topology configs.
     *
     * @param crm_topology_configs The CRM topology configs to save.
     * @throws std::exception on failure.
     */
    void
    save_crm_topology_configs(const std::vector<domain::crm_topology_config>& crm_topology_configs);

    /**
     * @brief Deletes a CRM topology config by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_crm_topology_config(const std::string& id);

    /**
     * @brief Deletes CRM topology configs by their primary keys.
     */
    void delete_crm_topology_configs(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a CRM topology config.
     */
    std::vector<domain::crm_topology_config> get_crm_topology_config_history(const std::string& id);

private:
    context ctx_;
    repository::crm_topology_config_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const messaging::crm_topology_config_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::crm_topology_config& out);
};

}

#endif
