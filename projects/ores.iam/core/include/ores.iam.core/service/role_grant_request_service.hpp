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
#ifndef ORES_IAM_CORE_SERVICE_ROLE_GRANT_REQUEST_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_ROLE_GRANT_REQUEST_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/role_grant_request.hpp"
#include "ores.iam.api/messaging/role_grant_request_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/role_grant_request_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing role grant requests.
 *
 * Provides a higher-level interface for role grant request operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT role_grant_request_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.role_grant_request_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a role_grant_request_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit role_grant_request_service(context ctx);

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
    messaging::list_role_grant_requests_response
    list_role_grant_requests(const messaging::list_role_grant_requests_request& request);
    messaging::get_role_grant_request_response
    get_role_grant_request(const messaging::get_role_grant_request_request& request);
    messaging::get_many_role_grant_requests_response
    get_many_role_grant_requests(const messaging::get_many_role_grant_requests_request& request);
    messaging::put_role_grant_request_response
    put_role_grant_request(const messaging::put_role_grant_request_request& request);
    messaging::put_many_role_grant_requests_response
    put_many_role_grant_requests(const messaging::put_many_role_grant_requests_request& request);
    messaging::delete_role_grant_request_response
    delete_role_grant_request(const messaging::delete_role_grant_request_request& request);
    messaging::delete_many_role_grant_requests_response delete_many_role_grant_requests(
        const messaging::delete_many_role_grant_requests_request& request);
    messaging::list_by_account_id_role_grant_requests_response
    list_by_account_id_role_grant_requests(
        const messaging::list_by_account_id_role_grant_requests_request& request);
    messaging::list_role_grant_request_versions_response list_role_grant_request_versions(
        const messaging::list_role_grant_request_versions_request& request);
    messaging::get_role_grant_request_version_response get_role_grant_request_version(
        const messaging::get_role_grant_request_version_request& request);
    /**@}*/

    /**
     * @brief Lists role grant requests with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of role grant requests for the requested page.
     */
    std::vector<domain::role_grant_request> list_role_grant_requests(std::uint32_t offset,
                                                                     std::uint32_t limit);

    /**
     * @brief Gets the total count of active role grant requests.
     *
     * @return Total number of active role grant requests.
     */
    std::uint32_t count_role_grant_requests();


    /**
     * @brief Lists role grant requests filtered by account_id, with pagination.
     *
     * @param account_id The account_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching role grant requests for the requested page.
     */
    std::vector<domain::role_grant_request> list_role_grant_requests_by_account_id(
        const std::string& account_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active role grant requests filtered by account_id.
     *
     * @param account_id The account_id to filter by.
     * @return Total number of matching role grant requests.
     */
    std::uint32_t count_role_grant_requests_by_account_id(const std::string& account_id);


    /**
     * @brief Retrieves a single role grant request as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The role grant request at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::role_grant_request>
    get_role_grant_request_at_version(const boost::uuids::uuid& request_id, std::uint32_t version);

    /**
     * @brief Retrieves a single role grant request by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The role grant request if found, std::nullopt otherwise.
     */
    std::optional<domain::role_grant_request>
    get_role_grant_request(const boost::uuids::uuid& request_id);

    /**
     * @brief Retrieves a batch of role grant requests by primary key.
     */
    std::vector<domain::role_grant_request>
    get_role_grant_requests(const std::vector<std::string>& request_ids);

    /**
     * @brief Saves a role grant request (creates or updates).
     *
     * @param role_grant_request The role grant request to save.
     * @throws std::exception on failure.
     */
    void save_role_grant_request(const domain::role_grant_request& role_grant_request);

    /**
     * @brief Saves a batch of role grant requests.
     *
     * @param role_grant_requests The role grant requests to save.
     * @throws std::exception on failure.
     */
    void
    save_role_grant_requests(const std::vector<domain::role_grant_request>& role_grant_requests);

    /**
     * @brief Deletes a role grant request by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_role_grant_request(const boost::uuids::uuid& request_id);

    /**
     * @brief Deletes role grant requests by their primary keys.
     */
    void delete_role_grant_requests(const std::vector<std::string>& request_ids);

    /**
     * @brief Retrieves all historical versions of a role grant request.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::role_grant_request>
    get_role_grant_request_history(const std::string& request_id);

private:
    context ctx_;
    repository::role_grant_request_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::role_grant_request_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::role_grant_request& out);
};

}

#endif
