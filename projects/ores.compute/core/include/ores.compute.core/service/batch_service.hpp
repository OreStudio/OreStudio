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
#ifndef ORES_COMPUTE_CORE_SERVICE_BATCH_SERVICE_HPP
#define ORES_COMPUTE_CORE_SERVICE_BATCH_SERVICE_HPP

#include "ores.compute.api/domain/batch.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.compute.core/repository/batch_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::compute::service {

/**
 * @brief Service for managing compute batches.
 *
 * Provides a higher-level interface for compute batch operations,
 * wrapping the underlying repository.
 */
class ORES_COMPUTE_CORE_EXPORT batch_service {
private:
    inline static std::string_view logger_name = "ores.compute.service.batch_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a batch_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit batch_service(context ctx);

    /**
     * @brief Lists compute batches with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of compute batches for the requested page.
     */
    std::vector<domain::batch> list_batches(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active compute batches.
     *
     * @return Total number of active compute batches.
     */
    std::uint32_t count_batches();


    /**
     * @brief Retrieves a single compute batch as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The compute batch at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::batch> get_batch_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single compute batch by its primary key.
     *
     * @return The compute batch if found, std::nullopt otherwise.
     */
    std::optional<domain::batch> get_batch(const std::string& id);

    /**
     * @brief Saves a compute batch (creates or updates).
     *
     * @param batch The compute batch to save.
     * @throws std::exception on failure.
     */
    void save_batch(const domain::batch& batch);

    /**
     * @brief Saves a batch of compute batches.
     *
     * @param batches The compute batches to save.
     * @throws std::exception on failure.
     */
    void save_batches(const std::vector<domain::batch>& batches);

    /**
     * @brief Deletes a compute batch by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_batch(const std::string& id);

    /**
     * @brief Deletes compute batches by their primary keys.
     */
    void delete_batches(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a compute batch.
     */
    std::vector<domain::batch> get_batch_history(const std::string& id);

private:
    context ctx_;
    repository::batch_repository repo_;
};

}

#endif
