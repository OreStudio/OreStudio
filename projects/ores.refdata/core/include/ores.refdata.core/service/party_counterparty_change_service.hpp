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
#ifndef ORES_REFDATA_CORE_SERVICE_PARTY_COUNTERPARTY_CHANGE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_PARTY_COUNTERPARTY_CHANGE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_counterparty_change.hpp"
#include "ores.refdata.api/messaging/party_counterparty_change_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/party_counterparty_change_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing party counterparty changes.
 *
 * Provides a higher-level interface for party counterparty change operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT party_counterparty_change_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.party_counterparty_change_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_counterparty_change_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit party_counterparty_change_service(context ctx);

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
    messaging::list_party_counterparty_changes_response list_party_counterparty_changes(
        const messaging::list_party_counterparty_changes_request& request);
    messaging::get_party_counterparty_change_response
    get_party_counterparty_change(const messaging::get_party_counterparty_change_request& request);
    messaging::get_many_party_counterparty_changes_response get_many_party_counterparty_changes(
        const messaging::get_many_party_counterparty_changes_request& request);
    messaging::list_by_request_id_party_counterparty_changes_response
    list_by_request_id_party_counterparty_changes(
        const messaging::list_by_request_id_party_counterparty_changes_request& request);
    messaging::list_party_counterparty_change_versions_response
    list_party_counterparty_change_versions(
        const messaging::list_party_counterparty_change_versions_request& request);
    messaging::get_party_counterparty_change_version_response get_party_counterparty_change_version(
        const messaging::get_party_counterparty_change_version_request& request);
    /**@}*/

    /**
     * @brief Lists party counterparty changes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of party counterparty changes for the requested page.
     */
    std::vector<domain::party_counterparty_change>
    list_party_counterparty_changes(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party counterparty changes.
     *
     * @return Total number of active party counterparty changes.
     */
    std::uint32_t count_party_counterparty_changes();


    /**
     * @brief Lists party counterparty changes filtered by request_id, with pagination.
     *
     * @param request_id The request_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching party counterparty changes for the requested page.
     */
    std::vector<domain::party_counterparty_change> list_party_counterparty_changes_by_request_id(
        const std::string& request_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party counterparty changes filtered by request_id.
     *
     * @param request_id The request_id to filter by.
     * @return Total number of matching party counterparty changes.
     */
    std::uint32_t count_party_counterparty_changes_by_request_id(const std::string& request_id);


    /**
     * @brief Retrieves a single party counterparty change as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The party counterparty change at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::party_counterparty_change>
    get_party_counterparty_change_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single party counterparty change by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The party counterparty change if found, std::nullopt otherwise.
     */
    std::optional<domain::party_counterparty_change>
    get_party_counterparty_change(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of party counterparty changes by primary key.
     */
    std::vector<domain::party_counterparty_change>
    get_party_counterparty_changes(const std::vector<std::string>& ids);

    /**
     * @brief Saves a party counterparty change (creates or updates).
     *
     * @param party_counterparty_change The party counterparty change to save.
     * @throws std::exception on failure.
     */
    void save_party_counterparty_change(
        const domain::party_counterparty_change& party_counterparty_change);

    /**
     * @brief Saves a batch of party counterparty changes.
     *
     * @param party_counterparty_changes The party counterparty changes to save.
     * @throws std::exception on failure.
     */
    void save_party_counterparty_changes(
        const std::vector<domain::party_counterparty_change>& party_counterparty_changes);

    /**
     * @brief Deletes a party counterparty change by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_party_counterparty_change(const boost::uuids::uuid& id);

    /**
     * @brief Deletes party counterparty changes by their primary keys.
     */
    void delete_party_counterparty_changes(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a party counterparty change.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::party_counterparty_change>
    get_party_counterparty_change_history(const std::string& id);

private:
    context ctx_;
    repository::party_counterparty_change_repository repo_;
};

}

#endif
