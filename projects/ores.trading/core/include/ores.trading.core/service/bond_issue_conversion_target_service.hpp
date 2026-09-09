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
#ifndef ORES_TRADING_CORE_SERVICE_BOND_ISSUE_CONVERSION_TARGET_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_ISSUE_CONVERSION_TARGET_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_issue_conversion_target.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/bond_issue_conversion_target_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing bond issue conversion targets.
 *
 * Provides a higher-level interface for bond issue conversion target operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT bond_issue_conversion_target_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.bond_issue_conversion_target_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_issue_conversion_target_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_issue_conversion_target_service(context ctx);

    /**
     * @brief Lists bond issue conversion targets with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond issue conversion targets for the requested page.
     */
    std::vector<domain::bond_issue_conversion_target> list_conversion_targets(std::uint32_t offset,
                                                                              std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond issue conversion targets.
     *
     * @return Total number of active bond issue conversion targets.
     */
    std::uint32_t count_conversion_targets();


    /**
     * @brief Retrieves a single bond issue conversion target as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond issue conversion target at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_issue_conversion_target> get_conversion_target_at_version(
        const std::string& issue_id, const std::string& sequence_number, std::uint32_t version);

    /**
     * @brief Retrieves a single bond issue conversion target by its primary key.
     *
     * @return The bond issue conversion target if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_issue_conversion_target>
    get_conversion_target(const std::string& issue_id, const std::string& sequence_number);

    /**
     * @brief Saves a bond issue conversion target (creates or updates).
     *
     * @param conversion_target The bond issue conversion target to save.
     * @throws std::exception on failure.
     */
    void save_conversion_target(const domain::bond_issue_conversion_target& conversion_target);

    /**
     * @brief Saves a batch of bond issue conversion targets.
     *
     * @param conversion_targets The bond issue conversion targets to save.
     * @throws std::exception on failure.
     */
    void save_conversion_targets(
        const std::vector<domain::bond_issue_conversion_target>& conversion_targets);

    /**
     * @brief Deletes a bond issue conversion target by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_conversion_target(const std::string& issue_id, const std::string& sequence_number);

    /**
     * @brief Deletes bond issue conversion targets by their primary keys.
     */
    void delete_conversion_targets(const std::vector<std::string>& issue_ids,
                                   const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a bond issue conversion target.
     */
    std::vector<domain::bond_issue_conversion_target>
    get_conversion_target_history(const std::string& issue_id, const std::string& sequence_number);

private:
    context ctx_;
    repository::bond_issue_conversion_target_repository repo_;
};

}

#endif
