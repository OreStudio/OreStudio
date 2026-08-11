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
#ifndef ORES_IAM_CORE_SERVICE_ACCOUNT_TYPE_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_ACCOUNT_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/account_type.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/account_type_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing account types.
 *
 * Provides a higher-level interface for account type operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT account_type_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.account_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a account_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit account_type_service(context ctx);

    /**
     * @brief Lists account types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of account types for the requested page.
     */
    std::vector<domain::account_type> list_types(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active account types.
     *
     * @return Total number of active account types.
     */
    std::uint32_t count_types();


    /**
     * @brief Retrieves a single account type as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The account type at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::account_type> get_type_at_version(const std::string& type,
                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single account type by its primary key.
     *
     * @return The account type if found, std::nullopt otherwise.
     */
    std::optional<domain::account_type> find_type(const std::string& type);

    /**
     * @brief Saves a account type (creates or updates).
     *
     * @param type The account type to save.
     * @throws std::exception on failure.
     */
    void save_type(const domain::account_type& type);

    /**
     * @brief Saves a batch of account types.
     *
     * @param types The account types to save.
     * @throws std::exception on failure.
     */
    void save_types(const std::vector<domain::account_type>& types);

    /**
     * @brief Deletes a account type by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_type(const std::string& type);

    /**
     * @brief Deletes account types by their primary keys.
     */
    void delete_types(const std::vector<std::string>& types);

    /**
     * @brief Retrieves all historical versions of a account type.
     */
    std::vector<domain::account_type> get_type_history(const std::string& type);

private:
    context ctx_;
    repository::account_type_repository repo_;
};

}

#endif
