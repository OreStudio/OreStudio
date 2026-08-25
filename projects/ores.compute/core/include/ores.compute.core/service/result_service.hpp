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
#ifndef ORES_COMPUTE_CORE_SERVICE_RESULT_SERVICE_HPP
#define ORES_COMPUTE_CORE_SERVICE_RESULT_SERVICE_HPP

#include "ores.compute.api/domain/result.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.compute.core/repository/result_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::compute::service {

/**
 * @brief Service for managing compute results.
 *
 * Provides a higher-level interface for compute result operations,
 * wrapping the underlying repository.
 */
class ORES_COMPUTE_CORE_EXPORT result_service {
private:
    inline static std::string_view logger_name = "ores.compute.service.result_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a result_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit result_service(context ctx);

    /**
     * @brief Lists compute results with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of compute results for the requested page.
     */
    std::vector<domain::result> list_results(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active compute results.
     *
     * @return Total number of active compute results.
     */
    std::uint32_t count_results();


    /**
     * @brief Lists compute results filtered by workunit_id, with pagination.
     *
     * @param workunit_id The workunit_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching compute results for the requested page.
     */
    std::vector<domain::result> list_results_by_workunit_id(const std::string& workunit_id,
                                                            std::uint32_t offset,
                                                            std::uint32_t limit);

    /**
     * @brief Gets the total count of active compute results filtered by workunit_id.
     *
     * @param workunit_id The workunit_id to filter by.
     * @return Total number of matching compute results.
     */
    std::uint32_t count_results_by_workunit_id(const std::string& workunit_id);


    /**
     * @brief Retrieves a single compute result as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The compute result at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::result> get_result_at_version(const std::string& id,
                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single compute result by its primary key.
     *
     * @return The compute result if found, std::nullopt otherwise.
     */
    std::optional<domain::result> get_result(const std::string& id);

    /**
     * @brief Saves a compute result (creates or updates).
     *
     * @param result The compute result to save.
     * @throws std::exception on failure.
     */
    void save_result(const domain::result& result);

    /**
     * @brief Saves a batch of compute results.
     *
     * @param results The compute results to save.
     * @throws std::exception on failure.
     */
    void save_results(const std::vector<domain::result>& results);

    /**
     * @brief Deletes a compute result by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_result(const std::string& id);

    /**
     * @brief Deletes compute results by their primary keys.
     */
    void delete_results(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a compute result.
     */
    std::vector<domain::result> get_result_history(const std::string& id);

    /**
     * @brief Lists results in a given server state, newest first.
     */
    std::vector<domain::result> list_by_state(int server_state);

private:
    context ctx_;
    repository::result_repository repo_;
};

}

#endif
