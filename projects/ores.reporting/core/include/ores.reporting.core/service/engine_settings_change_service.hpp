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
#ifndef ORES_REPORTING_CORE_SERVICE_ENGINE_SETTINGS_CHANGE_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_ENGINE_SETTINGS_CHANGE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/engine_settings_change.hpp"
#include "ores.reporting.api/messaging/engine_settings_change_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/engine_settings_change_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing engine settings changes.
 *
 * Provides a higher-level interface for engine settings change operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT engine_settings_change_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.engine_settings_change_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a engine_settings_change_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit engine_settings_change_service(context ctx);

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
    ::ores::reporting::messaging::list_engine_settings_changes_response
    list_engine_settings_changes(
        const ::ores::reporting::messaging::list_engine_settings_changes_request& request);
    ::ores::reporting::messaging::get_engine_settings_change_response get_engine_settings_change(
        const ::ores::reporting::messaging::get_engine_settings_change_request& request);
    ::ores::reporting::messaging::get_many_engine_settings_changes_response
    get_many_engine_settings_changes(
        const ::ores::reporting::messaging::get_many_engine_settings_changes_request& request);
    ::ores::reporting::messaging::list_by_request_id_engine_settings_changes_response
    list_by_request_id_engine_settings_changes(
        const ::ores::reporting::messaging::list_by_request_id_engine_settings_changes_request&
            request);
    ::ores::reporting::messaging::list_by_engine_settings_id_engine_settings_changes_response
    list_by_engine_settings_id_engine_settings_changes(
        const ::ores::reporting::messaging::
            list_by_engine_settings_id_engine_settings_changes_request& request);
    ::ores::reporting::messaging::list_engine_settings_change_versions_response
    list_engine_settings_change_versions(
        const ::ores::reporting::messaging::list_engine_settings_change_versions_request& request);
    ::ores::reporting::messaging::get_engine_settings_change_version_response
    get_engine_settings_change_version(
        const ::ores::reporting::messaging::get_engine_settings_change_version_request& request);
    /**@}*/

    /**
     * @brief Lists engine settings changes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of engine settings changes for the requested page.
     */
    std::vector<domain::engine_settings_change> list_engine_settings_changes(std::uint32_t offset,
                                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active engine settings changes.
     *
     * @return Total number of active engine settings changes.
     */
    std::uint32_t count_engine_settings_changes();


    /**
     * @brief Lists engine settings changes filtered by request_id, with pagination.
     *
     * @param request_id The request_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching engine settings changes for the requested page.
     */
    std::vector<domain::engine_settings_change> list_engine_settings_changes_by_request_id(
        const std::string& request_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active engine settings changes filtered by request_id.
     *
     * @param request_id The request_id to filter by.
     * @return Total number of matching engine settings changes.
     */
    std::uint32_t count_engine_settings_changes_by_request_id(const std::string& request_id);


    /**
     * @brief Lists engine settings changes filtered by engine_settings_id, with pagination.
     *
     * @param engine_settings_id The engine_settings_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching engine settings changes for the requested page.
     */
    std::vector<domain::engine_settings_change> list_engine_settings_changes_by_engine_settings_id(
        const std::string& engine_settings_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active engine settings changes filtered by engine_settings_id.
     *
     * @param engine_settings_id The engine_settings_id to filter by.
     * @return Total number of matching engine settings changes.
     */
    std::uint32_t
    count_engine_settings_changes_by_engine_settings_id(const std::string& engine_settings_id);


    /**
     * @brief Retrieves a single engine settings change as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The engine settings change at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::engine_settings_change>
    get_engine_settings_change_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single engine settings change by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The engine settings change if found, std::nullopt otherwise.
     */
    std::optional<domain::engine_settings_change>
    get_engine_settings_change(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of engine settings changes by primary key.
     */
    std::vector<domain::engine_settings_change>
    get_engine_settings_changes(const std::vector<std::string>& ids);

    /**
     * @brief Saves a engine settings change (creates or updates).
     *
     * @param engine_settings_change The engine settings change to save.
     * @throws std::exception on failure.
     */
    void save_engine_settings_change(const domain::engine_settings_change& engine_settings_change);

    /**
     * @brief Saves a batch of engine settings changes.
     *
     * @param engine_settings_changes The engine settings changes to save.
     * @throws std::exception on failure.
     */
    void save_engine_settings_changes(
        const std::vector<domain::engine_settings_change>& engine_settings_changes);

    /**
     * @brief Deletes a engine settings change by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_engine_settings_change(const boost::uuids::uuid& id);

    /**
     * @brief Deletes engine settings changes by their primary keys.
     */
    void delete_engine_settings_changes(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a engine settings change.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::engine_settings_change>
    get_engine_settings_change_history(const std::string& id);

private:
    context ctx_;
    repository::engine_settings_change_repository repo_;
};

}

#endif
