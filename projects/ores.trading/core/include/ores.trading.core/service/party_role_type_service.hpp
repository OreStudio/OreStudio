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
#ifndef ORES_TRADING_CORE_SERVICE_PARTY_ROLE_TYPE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_PARTY_ROLE_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/party_role_type.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/party_role_type_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing party role types.
 *
 * Provides a higher-level interface for party role type operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT party_role_type_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.party_role_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_role_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit party_role_type_service(context ctx);

    /**
     * @brief Lists party role types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of party role types for the requested page.
     */
    std::vector<domain::party_role_type> list_role_types(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party role types.
     *
     * @return Total number of active party role types.
     */
    std::uint32_t count_role_types();


    /**
     * @brief Retrieves a single party role type as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The party role type at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::party_role_type> get_role_type_at_version(const std::string& code,
                                                                    std::uint32_t version);

    /**
     * @brief Retrieves a single party role type by its primary key.
     *
     * @return The party role type if found, std::nullopt otherwise.
     */
    std::optional<domain::party_role_type> get_role_type(const std::string& code);

    /**
     * @brief Saves a party role type (creates or updates).
     *
     * @param role_type The party role type to save.
     * @throws std::exception on failure.
     */
    void save_role_type(const domain::party_role_type& role_type);

    /**
     * @brief Saves a batch of party role types.
     *
     * @param role_types The party role types to save.
     * @throws std::exception on failure.
     */
    void save_role_types(const std::vector<domain::party_role_type>& role_types);

    /**
     * @brief Deletes a party role type by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_role_type(const std::string& code);

    /**
     * @brief Deletes party role types by their primary keys.
     */
    void delete_role_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a party role type.
     */
    std::vector<domain::party_role_type> get_role_type_history(const std::string& code);

private:
    context ctx_;
    repository::party_role_type_repository repo_;
};

}

#endif
