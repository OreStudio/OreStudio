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
#ifndef ORES_DQ_CORE_SERVICE_DATASET_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_DATASET_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/dataset.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/dataset_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing datasets.
 *
 * This service provides functionality for:
 * - Managing datasets (CRUD operations)
 */
class ORES_DQ_CORE_EXPORT dataset_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.dataset_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a dataset_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit dataset_service(context ctx);

    // ========================================================================
    // Dataset Management
    // ========================================================================

    /**
     * @brief Lists all datasets.
     */
    std::vector<domain::dataset> list_datasets();

    /**
     * @brief Lists datasets with pagination.
     */
    std::vector<domain::dataset> list_datasets(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active datasets.
     */
    std::uint32_t get_dataset_count();

    /**
     * @brief Finds a dataset by its ID.
     */
    std::optional<domain::dataset> find_dataset(const boost::uuids::uuid& id);

    /**
     * @brief Saves a dataset (creates or updates).
     *
     * @param dataset The dataset to save
     */
    void save_dataset(const domain::dataset& dataset);

    /**
     * @brief Saves multiple datasets (creates or updates).
     *
     * @param datasets The datasets to save
     */
    void save_datasets(const std::vector<domain::dataset>& datasets);

    /**
     * @brief Removes a dataset.
     *
     * @param id The ID of the dataset to remove
     */
    void remove_dataset(const boost::uuids::uuid& id);

    /**
     * @brief Gets the version history for a dataset.
     *
     * @param id The dataset ID
     * @return Vector of all versions, newest first
     */
    std::vector<domain::dataset> get_dataset_history(const boost::uuids::uuid& id);

private:
    repository::dataset_repository dataset_repo_;
};

}

#endif
