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
#ifndef ORES_TRADING_CORE_SERVICE_BOND_LEG_RATE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_LEG_RATE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_leg_rate.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/bond_leg_rate_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing bond leg rates.
 *
 * Provides a higher-level interface for bond leg rate operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT bond_leg_rate_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.bond_leg_rate_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_leg_rate_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_leg_rate_service(context ctx);

    /**
     * @brief Lists bond leg rates with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond leg rates for the requested page.
     */
    std::vector<domain::bond_leg_rate> list_bond_leg_rates(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond leg rates.
     *
     * @return Total number of active bond leg rates.
     */
    std::uint32_t count_bond_leg_rates();


    /**
     * @brief Retrieves a single bond leg rate as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond leg rate at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_leg_rate>
    get_bond_leg_rate_at_version(const std::string& instrument_id,
                                 const std::string& leg_role,
                                 const std::string& leg_number,
                                 std::uint32_t version);

    /**
     * @brief Retrieves a single bond leg rate by its primary key.
     *
     * @return The bond leg rate if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_leg_rate> get_bond_leg_rate(const std::string& instrument_id,
                                                           const std::string& leg_role,
                                                           const std::string& leg_number);

    /**
     * @brief Saves a bond leg rate (creates or updates).
     *
     * @param bond_leg_rate The bond leg rate to save.
     * @throws std::exception on failure.
     */
    void save_bond_leg_rate(const domain::bond_leg_rate& bond_leg_rate);

    /**
     * @brief Saves a batch of bond leg rates.
     *
     * @param bond_leg_rates The bond leg rates to save.
     * @throws std::exception on failure.
     */
    void save_bond_leg_rates(const std::vector<domain::bond_leg_rate>& bond_leg_rates);

    /**
     * @brief Deletes a bond leg rate by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_bond_leg_rate(const std::string& instrument_id,
                              const std::string& leg_role,
                              const std::string& leg_number);

    /**
     * @brief Deletes bond leg rates by their primary keys.
     */
    void delete_bond_leg_rates(const std::vector<std::string>& instrument_ids,
                               const std::vector<std::string>& leg_roles,
                               const std::vector<std::string>& leg_numbers);

    /**
     * @brief Retrieves all historical versions of a bond leg rate.
     */
    std::vector<domain::bond_leg_rate> get_bond_leg_rate_history(const std::string& instrument_id,
                                                                 const std::string& leg_role,
                                                                 const std::string& leg_number);

private:
    context ctx_;
    repository::bond_leg_rate_repository repo_;
};

}

#endif
