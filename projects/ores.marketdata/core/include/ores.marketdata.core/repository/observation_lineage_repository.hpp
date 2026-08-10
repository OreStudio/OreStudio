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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_OBSERVATION_LINEAGE_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_OBSERVATION_LINEAGE_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/observation_lineage.hpp"
#include "ores.marketdata.core/export.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief Reads and writes observation lineages to data storage.
 */
class ORES_MARKETDATA_CORE_EXPORT observation_lineage_repository {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.observation_lineage_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes observation lineages to database.
     */
    /**@{*/
    void write(context ctx, const domain::observation_lineage& v);
    void write(context ctx, const std::vector<domain::observation_lineage>& v);
    /**@}*/

    /**
     * @brief Reads latest observation lineages, possibly filtered by primary key.
     */
    /**@{*/
    std::vector<domain::observation_lineage> read_latest(context ctx);
    std::vector<domain::observation_lineage> read_latest(context ctx, const std::string& id);
    /**@}*/

    /**
     * @brief Reads all observation lineages, possibly filtered by primary key.
     */
    std::vector<domain::observation_lineage> read_all(context ctx, const std::string& id);

    /**
     * @brief Reads a single observation lineage as it stood at a specific
     * version — the version's own [valid_from, valid_to) window is returned
     * verbatim, so the caller can compose child entities "as of" the same
     * window. See the "Temporal composite entity versioning" architecture
     * doc.
     * @param ctx Repository context with database connection
     * @param version The version to fetch
     */
    std::optional<domain::observation_lineage>
    read_at_version(context ctx, const std::string& id, std::uint32_t version);

    /**
     * @brief Reads latest observation lineages with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::observation_lineage>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active observation lineages.
     * @param ctx Repository context with database connection
     * @return Total number of active observation lineages
     */
    std::uint32_t get_total_observation_lineage_count(context ctx);

    /**
     * @brief Deletes a observation lineage by closing its temporal validity.
     */
    void remove(context ctx, const std::string& id);

    /**
     * @brief Deletes observation lineages by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& ids);

    std::optional<domain::observation_lineage>
    read_latest_by_observation(context ctx,
                               const boost::uuids::uuid& series_id,
                               std::chrono::system_clock::time_point observation_datetime,
                               const std::string& point_id);
};

}

#endif
