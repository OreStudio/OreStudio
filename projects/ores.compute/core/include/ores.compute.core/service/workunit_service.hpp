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
#ifndef ORES_COMPUTE_CORE_SERVICE_WORKUNIT_SERVICE_HPP
#define ORES_COMPUTE_CORE_SERVICE_WORKUNIT_SERVICE_HPP

#include "ores.compute.api/domain/workunit.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.compute.core/repository/workunit_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::compute::service {

/**
 * @brief Service for managing workunits.
 *
 * Provides a higher-level interface for workunit operations,
 * wrapping the underlying repository.
 */
class ORES_COMPUTE_CORE_EXPORT workunit_service {
private:
    inline static std::string_view logger_name = "ores.compute.service.workunit_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a workunit_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit workunit_service(context ctx);

    /**
     * @brief Lists workunits with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of workunits for the requested page.
     */
    std::vector<domain::workunit> list_workunits(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active workunits.
     *
     * @return Total number of active workunits.
     */
    std::uint32_t count_workunits();


    /**
     * @brief Lists workunits filtered by batch_id, with pagination.
     *
     * @param batch_id The batch_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching workunits for the requested page.
     */
    std::vector<domain::workunit> list_workunits_by_batch_id(const std::string& batch_id,
                                                             std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active workunits filtered by batch_id.
     *
     * @param batch_id The batch_id to filter by.
     * @return Total number of matching workunits.
     */
    std::uint32_t count_workunits_by_batch_id(const std::string& batch_id);


    /**
     * @brief Retrieves a single workunit as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The workunit at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::workunit> get_workunit_at_version(const std::string& id,
                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single workunit by its primary key.
     *
     * @return The workunit if found, std::nullopt otherwise.
     */
    std::optional<domain::workunit> get_workunit(const std::string& id);

    /**
     * @brief Saves a workunit (creates or updates).
     *
     * @param workunit The workunit to save.
     * @throws std::exception on failure.
     */
    void save_workunit(const domain::workunit& workunit);

    /**
     * @brief Saves a batch of workunits.
     *
     * @param workunits The workunits to save.
     * @throws std::exception on failure.
     */
    void save_workunits(const std::vector<domain::workunit>& workunits);

    /**
     * @brief Deletes a workunit by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_workunit(const std::string& id);

    /**
     * @brief Deletes workunits by their primary keys.
     */
    void delete_workunits(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a workunit.
     */
    std::vector<domain::workunit> get_workunit_history(const std::string& id);

private:
    context ctx_;
    repository::workunit_repository repo_;
};

}

#endif
