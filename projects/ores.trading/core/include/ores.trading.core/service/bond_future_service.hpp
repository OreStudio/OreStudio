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
#ifndef ORES_TRADING_CORE_SERVICE_BOND_FUTURE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_FUTURE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_future.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/bond_future_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing bond futures.
 *
 * Provides a higher-level interface for bond future operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT bond_future_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.bond_future_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_future_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_future_service(context ctx);

    /**
     * @brief Lists bond futures with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond futures for the requested page.
     */
    std::vector<domain::bond_future> list_futures(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond futures.
     *
     * @return Total number of active bond futures.
     */
    std::uint32_t count_futures();


    /**
     * @brief Retrieves a single bond future as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond future at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_future> get_future_at_version(const std::string& instrument_id,
                                                             std::uint32_t version);

    /**
     * @brief Retrieves a single bond future by its primary key.
     *
     * @return The bond future if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_future> get_future(const std::string& instrument_id);

    /**
     * @brief Saves a bond future (creates or updates).
     *
     * @param future The bond future to save.
     * @throws std::exception on failure.
     */
    void save_future(const domain::bond_future& future);

    /**
     * @brief Saves a batch of bond futures.
     *
     * @param futures The bond futures to save.
     * @throws std::exception on failure.
     */
    void save_futures(const std::vector<domain::bond_future>& futures);

    /**
     * @brief Deletes a bond future by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_future(const std::string& instrument_id);

    /**
     * @brief Deletes bond futures by their primary keys.
     */
    void delete_futures(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a bond future.
     */
    std::vector<domain::bond_future> get_future_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::bond_future_repository repo_;
};

}

#endif
