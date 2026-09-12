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
#ifndef ORES_TRADING_CORE_SERVICE_BOND_LEG_AMOUNT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_LEG_AMOUNT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_leg_amount.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/bond_leg_amount_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing bond leg amounts.
 *
 * Provides a higher-level interface for bond leg amount operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT bond_leg_amount_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.bond_leg_amount_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_leg_amount_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_leg_amount_service(context ctx);

    /**
     * @brief Lists bond leg amounts with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond leg amounts for the requested page.
     */
    std::vector<domain::bond_leg_amount> list_bond_leg_amounts(std::uint32_t offset,
                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond leg amounts.
     *
     * @return Total number of active bond leg amounts.
     */
    std::uint32_t count_bond_leg_amounts();


    /**
     * @brief Retrieves a single bond leg amount as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond leg amount at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_leg_amount>
    get_bond_leg_amount_at_version(const std::string& instrument_id,
                                   const std::string& leg_role,
                                   const std::string& leg_number,
                                   const std::string& amount_role,
                                   const std::string& sequence_number,
                                   std::uint32_t version);

    /**
     * @brief Retrieves a single bond leg amount by its primary key.
     *
     * @return The bond leg amount if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_leg_amount> get_bond_leg_amount(const std::string& instrument_id,
                                                               const std::string& leg_role,
                                                               const std::string& leg_number,
                                                               const std::string& amount_role,
                                                               const std::string& sequence_number);

    /**
     * @brief Saves a bond leg amount (creates or updates).
     *
     * @param bond_leg_amount The bond leg amount to save.
     * @throws std::exception on failure.
     */
    void save_bond_leg_amount(const domain::bond_leg_amount& bond_leg_amount);

    /**
     * @brief Saves a batch of bond leg amounts.
     *
     * @param bond_leg_amounts The bond leg amounts to save.
     * @throws std::exception on failure.
     */
    void save_bond_leg_amounts(const std::vector<domain::bond_leg_amount>& bond_leg_amounts);

    /**
     * @brief Deletes a bond leg amount by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_bond_leg_amount(const std::string& instrument_id,
                                const std::string& leg_role,
                                const std::string& leg_number,
                                const std::string& amount_role,
                                const std::string& sequence_number);

    /**
     * @brief Deletes bond leg amounts by their primary keys.
     */
    void delete_bond_leg_amounts(const std::vector<std::string>& instrument_ids,
                                 const std::vector<std::string>& leg_roles,
                                 const std::vector<std::string>& leg_numbers,
                                 const std::vector<std::string>& amount_roles,
                                 const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a bond leg amount.
     */
    std::vector<domain::bond_leg_amount>
    get_bond_leg_amount_history(const std::string& instrument_id,
                                const std::string& leg_role,
                                const std::string& leg_number,
                                const std::string& amount_role,
                                const std::string& sequence_number);

private:
    context ctx_;
    repository::bond_leg_amount_repository repo_;
};

}

#endif
