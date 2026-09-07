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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_PARTY_ROLE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_PARTY_ROLE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_party_role.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/trade_party_role_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing trade party roles.
 *
 * Provides a higher-level interface for trade party role operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT trade_party_role_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_party_role_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a trade_party_role_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit trade_party_role_service(context ctx);

    /**
     * @brief Lists trade party roles with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trade party roles for the requested page.
     */
    std::vector<domain::trade_party_role> list_roles(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade party roles.
     *
     * @return Total number of active trade party roles.
     */
    std::uint32_t count_roles();


    /**
     * @brief Retrieves a single trade party role as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The trade party role at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_party_role> get_role_at_version(const std::string& id,
                                                                std::uint32_t version);

    /**
     * @brief Retrieves a single trade party role by its primary key.
     *
     * @return The trade party role if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_party_role> get_role(const std::string& id);

    /**
     * @brief Saves a trade party role (creates or updates).
     *
     * @param role The trade party role to save.
     * @throws std::exception on failure.
     */
    void save_role(const domain::trade_party_role& role);

    /**
     * @brief Saves a batch of trade party roles.
     *
     * @param roles The trade party roles to save.
     * @throws std::exception on failure.
     */
    void save_roles(const std::vector<domain::trade_party_role>& roles);

    /**
     * @brief Deletes a trade party role by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_role(const std::string& id);

    /**
     * @brief Deletes trade party roles by their primary keys.
     */
    void delete_roles(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a trade party role.
     */
    std::vector<domain::trade_party_role> get_role_history(const std::string& id);

private:
    context ctx_;
    repository::trade_party_role_repository repo_;
};

}

#endif
