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
#ifndef ORES_REFDATA_CORE_SERVICE_COUNTERPARTY_IDENTIFIER_CHANGE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_COUNTERPARTY_IDENTIFIER_CHANGE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/counterparty_identifier_change.hpp"
#include "ores.refdata.api/messaging/counterparty_identifier_change_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/counterparty_identifier_change_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing counterparty_identifier changes.
 *
 * Provides a higher-level interface for counterparty_identifier change operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT counterparty_identifier_change_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.counterparty_identifier_change_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a counterparty_identifier_change_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit counterparty_identifier_change_service(context ctx);

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
    messaging::list_counterparty_identifier_changes_response list_counterparty_identifier_changes(
        const messaging::list_counterparty_identifier_changes_request& request);
    messaging::get_counterparty_identifier_change_response get_counterparty_identifier_change(
        const messaging::get_counterparty_identifier_change_request& request);
    messaging::get_many_counterparty_identifier_changes_response
    get_many_counterparty_identifier_changes(
        const messaging::get_many_counterparty_identifier_changes_request& request);
    messaging::list_by_request_id_counterparty_identifier_changes_response
    list_by_request_id_counterparty_identifier_changes(
        const messaging::list_by_request_id_counterparty_identifier_changes_request& request);
    messaging::list_counterparty_identifier_change_versions_response
    list_counterparty_identifier_change_versions(
        const messaging::list_counterparty_identifier_change_versions_request& request);
    messaging::get_counterparty_identifier_change_version_response
    get_counterparty_identifier_change_version(
        const messaging::get_counterparty_identifier_change_version_request& request);
    /**@}*/

    /**
     * @brief Lists counterparty_identifier changes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of counterparty_identifier changes for the requested page.
     */
    std::vector<domain::counterparty_identifier_change>
    list_counterparty_identifier_changes(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparty_identifier changes.
     *
     * @return Total number of active counterparty_identifier changes.
     */
    std::uint32_t count_counterparty_identifier_changes();


    /**
     * @brief Lists counterparty_identifier changes filtered by request_id, with pagination.
     *
     * @param request_id The request_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching counterparty_identifier changes for the requested page.
     */
    std::vector<domain::counterparty_identifier_change>
    list_counterparty_identifier_changes_by_request_id(const std::string& request_id,
                                                       std::uint32_t offset,
                                                       std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparty_identifier changes filtered by request_id.
     *
     * @param request_id The request_id to filter by.
     * @return Total number of matching counterparty_identifier changes.
     */
    std::uint32_t
    count_counterparty_identifier_changes_by_request_id(const std::string& request_id);


    /**
     * @brief Retrieves a single counterparty_identifier change as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The counterparty_identifier change at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty_identifier_change>
    get_counterparty_identifier_change_at_version(const boost::uuids::uuid& id,
                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single counterparty_identifier change by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The counterparty_identifier change if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty_identifier_change>
    get_counterparty_identifier_change(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of counterparty_identifier changes by primary key.
     */
    std::vector<domain::counterparty_identifier_change>
    get_counterparty_identifier_changes(const std::vector<std::string>& ids);

    /**
     * @brief Saves a counterparty_identifier change (creates or updates).
     *
     * @param counterparty_identifier_change The counterparty_identifier change to save.
     * @throws std::exception on failure.
     */
    void save_counterparty_identifier_change(
        const domain::counterparty_identifier_change& counterparty_identifier_change);

    /**
     * @brief Saves a batch of counterparty_identifier changes.
     *
     * @param counterparty_identifier_changes The counterparty_identifier changes to save.
     * @throws std::exception on failure.
     */
    void save_counterparty_identifier_changes(
        const std::vector<domain::counterparty_identifier_change>& counterparty_identifier_changes);

    /**
     * @brief Deletes a counterparty_identifier change by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_counterparty_identifier_change(const boost::uuids::uuid& id);

    /**
     * @brief Deletes counterparty_identifier changes by their primary keys.
     */
    void delete_counterparty_identifier_changes(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a counterparty_identifier change.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::counterparty_identifier_change>
    get_counterparty_identifier_change_history(const std::string& id);

private:
    context ctx_;
    repository::counterparty_identifier_change_repository repo_;
};

}

#endif
