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
#ifndef ORES_DQ_CORE_SERVICE_DATASET_BUNDLE_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_DATASET_BUNDLE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/dataset_bundle.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/dataset_bundle_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing dataset bundles.
 *
 * Provides a higher-level interface for dataset bundle operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT dataset_bundle_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.dataset_bundle_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a dataset_bundle_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit dataset_bundle_service(context ctx);

    /**
     * @brief Lists dataset bundles with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of dataset bundles for the requested page.
     */
    std::vector<domain::dataset_bundle> list_bundles(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active dataset bundles.
     *
     * @return Total number of active dataset bundles.
     */
    std::uint32_t count_bundles();


    /**
     * @brief Retrieves a single dataset bundle as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The dataset bundle at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::dataset_bundle> get_bundle_at_version(const std::string& id,
                                                                std::uint32_t version);

    /**
     * @brief Retrieves a single dataset bundle by its primary key.
     *
     * @return The dataset bundle if found, std::nullopt otherwise.
     */
    std::optional<domain::dataset_bundle> get_bundle(const std::string& id);

    /**
     * @brief Saves a dataset bundle (creates or updates).
     *
     * @param bundle The dataset bundle to save.
     * @throws std::exception on failure.
     */
    void save_bundle(const domain::dataset_bundle& bundle);

    /**
     * @brief Saves a batch of dataset bundles.
     *
     * @param bundles The dataset bundles to save.
     * @throws std::exception on failure.
     */
    void save_bundles(const std::vector<domain::dataset_bundle>& bundles);

    /**
     * @brief Deletes a dataset bundle by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_bundle(const std::string& id);

    /**
     * @brief Deletes dataset bundles by their primary keys.
     */
    void delete_bundles(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a dataset bundle.
     */
    std::vector<domain::dataset_bundle> get_bundle_history(const std::string& id);

private:
    context ctx_;
    repository::dataset_bundle_repository repo_;
};

}

#endif
