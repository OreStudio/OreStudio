/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_ACCOUNTS_REPOSITORY_FEATURE_FLAGS_REPOSITORY_HPP
#define ORES_ACCOUNTS_REPOSITORY_FEATURE_FLAGS_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/repository/context.hpp"
#include "ores.utility/repository/helpers.hpp"
#include "ores.utility/repository/bitemporal_operations.hpp"
#include "ores.accounts/domain/feature_flags.hpp"

namespace ores::accounts::repository {

/**
 * @brief Reads and writes feature flags from data storage.
 */
class feature_flags_repository {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(
            "ores.accounts.repository.feature_flags_repository");
        return instance;
    }

public:
    using context = ores::utility::repository::context;

    explicit feature_flags_repository(context ctx);

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes feature flags to database. Expects the feature flag set to
     * have unique names.
     */
    /**@{*/
    void write(const domain::feature_flags& flag);
    void write(const std::vector<domain::feature_flags>& flags);
    /**@}*/

    /**
     * @brief Reads latest feature flags, possibly filtered by name.
     */
    /**@{*/
    std::vector<domain::feature_flags> read_latest();
    std::vector<domain::feature_flags> read_latest(const std::string& name);
    /**@}*/

    /**
     * @brief Reads all feature flags, possibly filtered by name.
     */
    /**@{*/
    std::vector<domain::feature_flags> read_all();
    std::vector<domain::feature_flags> read_all(const std::string& name);
    /**@}*/

    /**
     * @brief Deletes a feature flag by closing its temporal validity.
     *
     * Sets the valid_to timestamp to now, effectively "deleting" the feature flag
     * from the current point in time onwards while preserving history.
     */
    void remove(const std::string& name);

private:
    context ctx_;
};

}

#endif
