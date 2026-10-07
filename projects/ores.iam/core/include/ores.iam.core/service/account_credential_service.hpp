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
#ifndef ORES_IAM_CORE_SERVICE_ACCOUNT_CREDENTIAL_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_ACCOUNT_CREDENTIAL_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/account_credential.hpp"
#include "ores.iam.api/messaging/account_credential_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/account_credential_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing account credentials.
 *
 * Provides a higher-level interface for account credential operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT account_credential_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.account_credential_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a account_credential_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit account_credential_service(context ctx);

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
    messaging::list_account_credentials_response
    list_account_credentials(const messaging::list_account_credentials_request& request);
    messaging::get_account_credential_response
    get_account_credential(const messaging::get_account_credential_request& request);
    messaging::get_many_account_credentials_response
    get_many_account_credentials(const messaging::get_many_account_credentials_request& request);
    messaging::list_by_account_id_account_credentials_response
    list_by_account_id_account_credentials(
        const messaging::list_by_account_id_account_credentials_request& request);
    messaging::list_account_credential_versions_response list_account_credential_versions(
        const messaging::list_account_credential_versions_request& request);
    messaging::get_account_credential_version_response get_account_credential_version(
        const messaging::get_account_credential_version_request& request);
    /**@}*/

    /**
     * @brief Lists account credentials with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of account credentials for the requested page.
     */
    std::vector<domain::account_credential> list_account_credentials(std::uint32_t offset,
                                                                     std::uint32_t limit);

    /**
     * @brief Gets the total count of active account credentials.
     *
     * @return Total number of active account credentials.
     */
    std::uint32_t count_account_credentials();


    /**
     * @brief Lists account credentials filtered by account_id, with pagination.
     *
     * @param account_id The account_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching account credentials for the requested page.
     */
    std::vector<domain::account_credential> list_account_credentials_by_account_id(
        const std::string& account_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active account credentials filtered by account_id.
     *
     * @param account_id The account_id to filter by.
     * @return Total number of matching account credentials.
     */
    std::uint32_t count_account_credentials_by_account_id(const std::string& account_id);


    /**
     * @brief Retrieves a single account credential as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The account credential at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::account_credential>
    get_account_credential_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single account credential by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The account credential if found, std::nullopt otherwise.
     */
    std::optional<domain::account_credential> get_account_credential(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of account credentials by primary key.
     */
    std::vector<domain::account_credential>
    get_account_credentials(const std::vector<std::string>& ids);

    /**
     * @brief Saves a account credential (creates or updates).
     *
     * @param account_credential The account credential to save.
     * @throws std::exception on failure.
     */
    void save_account_credential(const domain::account_credential& account_credential);

    /**
     * @brief Saves a batch of account credentials.
     *
     * @param account_credentials The account credentials to save.
     * @throws std::exception on failure.
     */
    void
    save_account_credentials(const std::vector<domain::account_credential>& account_credentials);

    /**
     * @brief Deletes a account credential by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_account_credential(const boost::uuids::uuid& id);

    /**
     * @brief Deletes account credentials by their primary keys.
     */
    void delete_account_credentials(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a account credential.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::account_credential> get_account_credential_history(const std::string& id);

private:
    context ctx_;
    repository::account_credential_repository repo_;
};

}

#endif
