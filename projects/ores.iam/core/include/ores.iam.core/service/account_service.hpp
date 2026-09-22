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
#ifndef ORES_IAM_CORE_SERVICE_ACCOUNT_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_ACCOUNT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/account.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing accounts.
 *
 * Provides a higher-level interface for account operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT account_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.account_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a account_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit account_service(context ctx);

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
    messaging::list_accounts_response
    list_accounts(const messaging::list_accounts_request& request);
    messaging::get_account_response get_account(const messaging::get_account_request& request);
    messaging::get_many_accounts_response
    get_many_accounts(const messaging::get_many_accounts_request& request);
    messaging::list_account_versions_response
    list_account_versions(const messaging::list_account_versions_request& request);
    messaging::get_account_version_response
    get_account_version(const messaging::get_account_version_request& request);
    /**@}*/

    /**
     * @brief Lists accounts with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of accounts for the requested page.
     */
    std::vector<domain::account> list_accounts(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active accounts.
     *
     * @return Total number of active accounts.
     */
    std::uint32_t count_accounts();


    /**
     * @brief Retrieves a single account as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The account at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::account> get_account_at_version(const boost::uuids::uuid& id,
                                                          std::uint32_t version);

    /**
     * @brief Retrieves a single account by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The account if found, std::nullopt otherwise.
     */
    std::optional<domain::account> get_account(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single account by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The account if found, std::nullopt otherwise.
     */
    std::optional<domain::account> get_account_by_username(const std::string& username);

    /**
     * @brief Retrieves a batch of accounts by primary key.
     */
    std::vector<domain::account> get_accounts(const std::vector<std::string>& ids);

    /**
     * @brief Saves a account (creates or updates).
     *
     * @param account The account to save.
     * @throws std::exception on failure.
     */
    void save_account(const domain::account& account);

    /**
     * @brief Saves a batch of accounts.
     *
     * @param accounts The accounts to save.
     * @throws std::exception on failure.
     */
    void save_accounts(const std::vector<domain::account>& accounts);

    /**
     * @brief Deletes a account by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_account(const boost::uuids::uuid& id);

    /**
     * @brief Deletes accounts by their primary keys.
     */
    void delete_accounts(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a account.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::account> get_account_history(const std::string& key);

private:
    context ctx_;
    repository::account_repository repo_;
};

}

#endif
