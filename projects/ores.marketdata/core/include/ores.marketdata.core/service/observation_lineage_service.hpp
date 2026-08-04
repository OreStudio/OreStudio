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
#ifndef ORES_MARKETDATA_CORE_SERVICE_OBSERVATION_LINEAGE_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_OBSERVATION_LINEAGE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/observation_lineage.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/observation_lineage_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing observation lineages.
 *
 * Provides a higher-level interface for observation lineage operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT observation_lineage_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.observation_lineage_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a observation_lineage_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit observation_lineage_service(context ctx);

    /**
     * @brief Lists observation lineages with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of observation lineages for the requested page.
     */
    std::vector<domain::observation_lineage> list_observation_lineages(std::uint32_t offset,
                                                                       std::uint32_t limit);

    /**
     * @brief Gets the total count of active observation lineages.
     *
     * @return Total number of active observation lineages.
     */
    std::uint32_t count_observation_lineages();


    /**
     * @brief Retrieves a single observation lineage as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The observation lineage at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::observation_lineage>
    get_observation_lineage_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single observation lineage by its primary key.
     *
     * @return The observation lineage if found, std::nullopt otherwise.
     */
    std::optional<domain::observation_lineage> get_observation_lineage(const std::string& id);

    /**
     * @brief Saves a observation lineage (creates or updates).
     *
     * @param observation_lineage The observation lineage to save.
     * @throws std::exception on failure.
     */
    void save_observation_lineage(const domain::observation_lineage& observation_lineage);

    /**
     * @brief Saves a batch of observation lineages.
     *
     * @param observation_lineages The observation lineages to save.
     * @throws std::exception on failure.
     */
    void
    save_observation_lineages(const std::vector<domain::observation_lineage>& observation_lineages);

    /**
     * @brief Deletes a observation lineage by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_observation_lineage(const std::string& id);

    /**
     * @brief Deletes observation lineages by their primary keys.
     */
    void delete_observation_lineages(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a observation lineage.
     */
    std::vector<domain::observation_lineage> get_observation_lineage_history(const std::string& id);

private:
    context ctx_;
    repository::observation_lineage_repository repo_;
};

}

#endif
