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
#ifndef ORES_IAM_CORE_SERVICE_ACCOUNT_CONTACT_INFORMATION_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_ACCOUNT_CONTACT_INFORMATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/account_contact_information.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/account_contact_information_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing account contact informations.
 *
 * Provides a higher-level interface for account contact information operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT account_contact_information_service {
private:
    inline static std::string_view logger_name =
        "ores.iam.service.account_contact_information_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a account_contact_information_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit account_contact_information_service(context ctx);

    /**
     * @brief Lists account contact informations with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of account contact informations for the requested page.
     */
    std::vector<domain::account_contact_information>
    list_account_contact_informations(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active account contact informations.
     *
     * @return Total number of active account contact informations.
     */
    std::uint32_t count_account_contact_informations();

    /**
     * @brief Lists account contact informations filtered by account_id, with pagination.
     *
     * @param account_id The account_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching account contact informations for the requested page.
     */
    std::vector<domain::account_contact_information>
    list_account_contact_informations_by_account_id(const std::string& account_id,
                                                    std::uint32_t offset,
                                                    std::uint32_t limit);

    /**
     * @brief Gets the total count of active account contact informations filtered by account_id.
     *
     * @param account_id The account_id to filter by.
     * @return Total number of matching account contact informations.
     */
    std::uint32_t count_account_contact_informations_by_account_id(const std::string& account_id);

    /**
     * @brief Lists account contact informations filtered by account_id that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param account_id The account_id to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching account contact informations.
     */
    std::vector<domain::account_contact_information>
    list_account_contact_informations_by_account_id_as_of(
        const std::string& account_id,
        std::chrono::system_clock::time_point valid_from_bound,
        std::chrono::system_clock::time_point valid_to_bound);
    /**
     * @brief Retrieves a single account contact information as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param id The id of the account contact information.
     * @param version The version to fetch.
     * @return The account contact information at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::account_contact_information>
    get_account_contact_information_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single account contact information by its id.
     *
     * @param id The id of the account contact information.
     * @return The account contact information if found, std::nullopt otherwise.
     */
    std::optional<domain::account_contact_information>
    get_account_contact_information(const std::string& id);

    /**
     * @brief Saves a account contact information (creates or updates).
     *
     * @param account_contact_information The account contact information to save.
     * @throws std::exception on failure.
     */
    void save_account_contact_information(
        const domain::account_contact_information& account_contact_information);

    /**
     * @brief Saves a batch of account contact informations.
     *
     * @param account_contact_informations The account contact informations to save.
     * @throws std::exception on failure.
     */
    void save_account_contact_informations(
        const std::vector<domain::account_contact_information>& account_contact_informations);

    /**
     * @brief Deletes a account contact information by its id.
     *
     * @param id The id of the account contact information to delete.
     * @throws std::exception on failure.
     */
    void delete_account_contact_information(const std::string& id);

    /**
     * @brief Deletes account contact informations by their ids.
     */
    void delete_account_contact_informations(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a account contact information.
     */
    std::vector<domain::account_contact_information>
    get_account_contact_information_history(const std::string& id);

private:
    context ctx_;
    repository::account_contact_information_repository repo_;
};

}

#endif
