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
#ifndef ORES_TRADING_CORE_SERVICE_BOND_LEG_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_LEG_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_leg.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/bond_leg_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing bond legs.
 *
 * Provides a higher-level interface for bond leg operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT bond_leg_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.bond_leg_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_leg_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_leg_service(context ctx);

    /**
     * @brief Lists bond legs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond legs for the requested page.
     */
    std::vector<domain::bond_leg> list_bond_legs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond legs.
     *
     * @return Total number of active bond legs.
     */
    std::uint32_t count_bond_legs();


    /**
     * @brief Retrieves a single bond leg as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond leg at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_leg> get_bond_leg_at_version(const std::string& instrument_id,
                                                            const std::string& leg_role,
                                                            const std::string& leg_number,
                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single bond leg by its primary key.
     *
     * @return The bond leg if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_leg> get_bond_leg(const std::string& instrument_id,
                                                 const std::string& leg_role,
                                                 const std::string& leg_number);

    /**
     * @brief Saves a bond leg (creates or updates).
     *
     * @param bond_leg The bond leg to save.
     * @throws std::exception on failure.
     */
    void save_bond_leg(const domain::bond_leg& bond_leg);

    /**
     * @brief Saves a batch of bond legs.
     *
     * @param bond_legs The bond legs to save.
     * @throws std::exception on failure.
     */
    void save_bond_legs(const std::vector<domain::bond_leg>& bond_legs);

    /**
     * @brief Deletes a bond leg by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_bond_leg(const std::string& instrument_id,
                         const std::string& leg_role,
                         const std::string& leg_number);

    /**
     * @brief Deletes bond legs by their primary keys.
     */
    void delete_bond_legs(const std::vector<std::string>& instrument_ids,
                          const std::vector<std::string>& leg_roles,
                          const std::vector<std::string>& leg_numbers);

    /**
     * @brief Retrieves all historical versions of a bond leg.
     */
    std::vector<domain::bond_leg> get_bond_leg_history(const std::string& instrument_id,
                                                       const std::string& leg_role,
                                                       const std::string& leg_number);

private:
    context ctx_;
    repository::bond_leg_repository repo_;
};

}

#endif
