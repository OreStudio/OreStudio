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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_IDENTIFIER_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_IDENTIFIER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_identifier.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/trade_identifier_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing trade identifiers.
 *
 * Provides a higher-level interface for trade identifier operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT trade_identifier_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_identifier_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a trade_identifier_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit trade_identifier_service(context ctx);

    /**
     * @brief Lists trade identifiers with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trade identifiers for the requested page.
     */
    std::vector<domain::trade_identifier> list_identifiers(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade identifiers.
     *
     * @return Total number of active trade identifiers.
     */
    std::uint32_t count_identifiers();


    /**
     * @brief Retrieves a single trade identifier as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The trade identifier at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_identifier> get_identifier_at_version(const std::string& id,
                                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single trade identifier by its primary key.
     *
     * @return The trade identifier if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_identifier> get_identifier(const std::string& id);

    /**
     * @brief Saves a trade identifier (creates or updates).
     *
     * @param identifier The trade identifier to save.
     * @throws std::exception on failure.
     */
    void save_identifier(const domain::trade_identifier& identifier);

    /**
     * @brief Saves a batch of trade identifiers.
     *
     * @param identifiers The trade identifiers to save.
     * @throws std::exception on failure.
     */
    void save_identifiers(const std::vector<domain::trade_identifier>& identifiers);

    /**
     * @brief Deletes a trade identifier by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_identifier(const std::string& id);

    /**
     * @brief Deletes trade identifiers by their primary keys.
     */
    void delete_identifiers(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a trade identifier.
     */
    std::vector<domain::trade_identifier> get_identifier_history(const std::string& id);

private:
    context ctx_;
    repository::trade_identifier_repository repo_;
};

}

#endif
