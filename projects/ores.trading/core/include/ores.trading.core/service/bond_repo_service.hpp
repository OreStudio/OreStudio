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
#ifndef ORES_TRADING_CORE_SERVICE_BOND_REPO_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_REPO_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_repo.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/bond_repo_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing bond repos.
 *
 * Provides a higher-level interface for bond repo operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT bond_repo_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.bond_repo_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_repo_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_repo_service(context ctx);

    /**
     * @brief Lists bond repos with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond repos for the requested page.
     */
    std::vector<domain::bond_repo> list_repos(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond repos.
     *
     * @return Total number of active bond repos.
     */
    std::uint32_t count_repos();


    /**
     * @brief Retrieves a single bond repo as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond repo at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_repo> get_repo_at_version(const std::string& instrument_id,
                                                         std::uint32_t version);

    /**
     * @brief Retrieves a single bond repo by its primary key.
     *
     * @return The bond repo if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_repo> get_repo(const std::string& instrument_id);

    /**
     * @brief Saves a bond repo (creates or updates).
     *
     * @param repo The bond repo to save.
     * @throws std::exception on failure.
     */
    void save_repo(const domain::bond_repo& repo);

    /**
     * @brief Saves a batch of bond repos.
     *
     * @param repos The bond repos to save.
     * @throws std::exception on failure.
     */
    void save_repos(const std::vector<domain::bond_repo>& repos);

    /**
     * @brief Deletes a bond repo by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_repo(const std::string& instrument_id);

    /**
     * @brief Deletes bond repos by their primary keys.
     */
    void delete_repos(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a bond repo.
     */
    std::vector<domain::bond_repo> get_repo_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::bond_repo_repository repo_;
};

}

#endif
