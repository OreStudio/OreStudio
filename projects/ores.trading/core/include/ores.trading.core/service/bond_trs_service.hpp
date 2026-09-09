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
#ifndef ORES_TRADING_CORE_SERVICE_BOND_TRS_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_TRS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_trs.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/bond_trs_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing bond trs.
 *
 * Provides a higher-level interface for bond trs operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT bond_trs_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.bond_trs_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_trs_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_trs_service(context ctx);

    /**
     * @brief Lists bond trs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond trs for the requested page.
     */
    std::vector<domain::bond_trs> list_trs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond trs.
     *
     * @return Total number of active bond trs.
     */
    std::uint32_t count_trs();


    /**
     * @brief Retrieves a single bond trs as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond trs at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_trs> get_trs_at_version(const std::string& instrument_id,
                                                       std::uint32_t version);

    /**
     * @brief Retrieves a single bond trs by its primary key.
     *
     * @return The bond trs if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_trs> get_trs(const std::string& instrument_id);

    /**
     * @brief Saves a bond trs (creates or updates).
     *
     * @param trs The bond trs to save.
     * @throws std::exception on failure.
     */
    void save_trs(const domain::bond_trs& trs);

    /**
     * @brief Saves a batch of bond trs.
     *
     * @param trs The bond trs to save.
     * @throws std::exception on failure.
     */
    void save_trs(const std::vector<domain::bond_trs>& trs);

    /**
     * @brief Deletes a bond trs by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_trs(const std::string& instrument_id);

    /**
     * @brief Deletes bond trs by their primary keys.
     */
    void delete_trs(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a bond trs.
     */
    std::vector<domain::bond_trs> get_trs_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::bond_trs_repository repo_;
};

}

#endif
