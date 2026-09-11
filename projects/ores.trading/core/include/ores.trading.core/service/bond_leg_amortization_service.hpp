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
#ifndef ORES_TRADING_CORE_SERVICE_BOND_LEG_AMORTIZATION_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_LEG_AMORTIZATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_leg_amortization.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/bond_leg_amortization_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing bond leg amortizations.
 *
 * Provides a higher-level interface for bond leg amortization operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT bond_leg_amortization_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.bond_leg_amortization_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_leg_amortization_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_leg_amortization_service(context ctx);

    /**
     * @brief Lists bond leg amortizations with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond leg amortizations for the requested page.
     */
    std::vector<domain::bond_leg_amortization> list_bond_leg_amortizations(std::uint32_t offset,
                                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond leg amortizations.
     *
     * @return Total number of active bond leg amortizations.
     */
    std::uint32_t count_bond_leg_amortizations();


    /**
     * @brief Retrieves a single bond leg amortization as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond leg amortization at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_leg_amortization>
    get_bond_leg_amortization_at_version(const std::string& instrument_id,
                                         const std::string& leg_role,
                                         const std::string& leg_number,
                                         const std::string& sequence_number,
                                         std::uint32_t version);

    /**
     * @brief Retrieves a single bond leg amortization by its primary key.
     *
     * @return The bond leg amortization if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_leg_amortization>
    get_bond_leg_amortization(const std::string& instrument_id,
                              const std::string& leg_role,
                              const std::string& leg_number,
                              const std::string& sequence_number);

    /**
     * @brief Saves a bond leg amortization (creates or updates).
     *
     * @param bond_leg_amortization The bond leg amortization to save.
     * @throws std::exception on failure.
     */
    void save_bond_leg_amortization(const domain::bond_leg_amortization& bond_leg_amortization);

    /**
     * @brief Saves a batch of bond leg amortizations.
     *
     * @param bond_leg_amortizations The bond leg amortizations to save.
     * @throws std::exception on failure.
     */
    void save_bond_leg_amortizations(
        const std::vector<domain::bond_leg_amortization>& bond_leg_amortizations);

    /**
     * @brief Deletes a bond leg amortization by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_bond_leg_amortization(const std::string& instrument_id,
                                      const std::string& leg_role,
                                      const std::string& leg_number,
                                      const std::string& sequence_number);

    /**
     * @brief Deletes bond leg amortizations by their primary keys.
     */
    void delete_bond_leg_amortizations(const std::vector<std::string>& instrument_ids,
                                       const std::vector<std::string>& leg_roles,
                                       const std::vector<std::string>& leg_numbers,
                                       const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a bond leg amortization.
     */
    std::vector<domain::bond_leg_amortization>
    get_bond_leg_amortization_history(const std::string& instrument_id,
                                      const std::string& leg_role,
                                      const std::string& leg_number,
                                      const std::string& sequence_number);

private:
    context ctx_;
    repository::bond_leg_amortization_repository repo_;
};

}

#endif
