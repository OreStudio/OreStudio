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
#ifndef ORES_TRADING_CORE_SERVICE_BOND_ISSUE_CALL_DATE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_ISSUE_CALL_DATE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_issue_call_date.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/bond_issue_call_date_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing bond issue call dates.
 *
 * Provides a higher-level interface for bond issue call date operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT bond_issue_call_date_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.bond_issue_call_date_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_issue_call_date_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_issue_call_date_service(context ctx);

    /**
     * @brief Lists bond issue call dates with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond issue call dates for the requested page.
     */
    std::vector<domain::bond_issue_call_date> list_call_dates(std::uint32_t offset,
                                                              std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond issue call dates.
     *
     * @return Total number of active bond issue call dates.
     */
    std::uint32_t count_call_dates();


    /**
     * @brief Retrieves a single bond issue call date as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond issue call date at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_issue_call_date> get_call_date_at_version(
        const std::string& issue_id, const std::string& sequence_number, std::uint32_t version);

    /**
     * @brief Retrieves a single bond issue call date by its primary key.
     *
     * @return The bond issue call date if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_issue_call_date> get_call_date(const std::string& issue_id,
                                                              const std::string& sequence_number);

    /**
     * @brief Saves a bond issue call date (creates or updates).
     *
     * @param call_date The bond issue call date to save.
     * @throws std::exception on failure.
     */
    void save_call_date(const domain::bond_issue_call_date& call_date);

    /**
     * @brief Saves a batch of bond issue call dates.
     *
     * @param call_dates The bond issue call dates to save.
     * @throws std::exception on failure.
     */
    void save_call_dates(const std::vector<domain::bond_issue_call_date>& call_dates);

    /**
     * @brief Deletes a bond issue call date by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_call_date(const std::string& issue_id, const std::string& sequence_number);

    /**
     * @brief Deletes bond issue call dates by their primary keys.
     */
    void delete_call_dates(const std::vector<std::string>& issue_ids,
                           const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a bond issue call date.
     */
    std::vector<domain::bond_issue_call_date>
    get_call_date_history(const std::string& issue_id, const std::string& sequence_number);

private:
    context ctx_;
    repository::bond_issue_call_date_repository repo_;
};

}

#endif
