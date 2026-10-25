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
#ifndef ORES_REPORTING_CORE_SERVICE_ENGINE_SETTINGS_GROUP_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_ENGINE_SETTINGS_GROUP_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/engine_settings_group.hpp"
#include "ores.reporting.api/messaging/engine_settings_group_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/engine_settings_group_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing engine settings groups.
 *
 * Provides a higher-level interface for engine settings group operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT engine_settings_group_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.engine_settings_group_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a engine_settings_group_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit engine_settings_group_service(context ctx);

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
    messaging::list_engine_settings_groups_response
    list_engine_settings_groups(const messaging::list_engine_settings_groups_request& request);
    messaging::get_engine_settings_group_response
    get_engine_settings_group(const messaging::get_engine_settings_group_request& request);
    messaging::get_many_engine_settings_groups_response get_many_engine_settings_groups(
        const messaging::get_many_engine_settings_groups_request& request);
    messaging::put_engine_settings_group_response
    put_engine_settings_group(const messaging::put_engine_settings_group_request& request);
    messaging::put_many_engine_settings_groups_response put_many_engine_settings_groups(
        const messaging::put_many_engine_settings_groups_request& request);
    messaging::delete_engine_settings_group_response
    delete_engine_settings_group(const messaging::delete_engine_settings_group_request& request);
    messaging::delete_many_engine_settings_groups_response delete_many_engine_settings_groups(
        const messaging::delete_many_engine_settings_groups_request& request);
    messaging::list_engine_settings_group_versions_response list_engine_settings_group_versions(
        const messaging::list_engine_settings_group_versions_request& request);
    messaging::get_engine_settings_group_version_response get_engine_settings_group_version(
        const messaging::get_engine_settings_group_version_request& request);
    /**@}*/

    /**
     * @brief Lists engine settings groups with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of engine settings groups for the requested page.
     */
    std::vector<domain::engine_settings_group> list_groups(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active engine settings groups.
     *
     * @return Total number of active engine settings groups.
     */
    std::uint32_t count_groups();


    /**
     * @brief Retrieves a single engine settings group as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The engine settings group at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::engine_settings_group> get_group_at_version(const std::string& code,
                                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single engine settings group by its primary key.
     *
     * @return The engine settings group if found, std::nullopt otherwise.
     */
    std::optional<domain::engine_settings_group> find_group(const std::string& code);

    /**
     * @brief Retrieves a batch of engine settings groups by primary key.
     */
    std::vector<domain::engine_settings_group> get_groups(const std::vector<std::string>& codes);

    /**
     * @brief Saves a engine settings group (creates or updates).
     *
     * @param group The engine settings group to save.
     * @throws std::exception on failure.
     */
    void save_group(const domain::engine_settings_group& group);

    /**
     * @brief Saves a batch of engine settings groups.
     *
     * @param groups The engine settings groups to save.
     * @throws std::exception on failure.
     */
    void save_groups(const std::vector<domain::engine_settings_group>& groups);

    /**
     * @brief Deletes a engine settings group by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_group(const std::string& code);

    /**
     * @brief Deletes engine settings groups by their primary keys.
     */
    void delete_groups(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a engine settings group.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::engine_settings_group> get_group_history(const std::string& code);

private:
    context ctx_;
    repository::engine_settings_group_repository repo_;

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
    prepare_change(const messaging::engine_settings_group_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::engine_settings_group& out);
};

}

#endif
