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
#ifndef ORES_DQ_CORE_SERVICE_DATA_ORGANIZATION_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_DATA_ORGANIZATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/dataset_dependency.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/dataset_dependency_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing data organization entities.
 *
 * This service provides functionality for:
 * - Managing subject areas and their associated domains
 * - Managing dataset dependencies
 *
 * Catalog and data domain management moved to the standalone, generated
 * catalog_service/data_domain_service.
 */
class ORES_DQ_CORE_EXPORT data_organization_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.data_organization_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a data_organization_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit data_organization_service(context ctx);

    // ========================================================================
    // Dataset Dependency Management
    // ========================================================================

    /**
     * @brief Lists all dataset dependencies.
     */
    std::vector<domain::dataset_dependency> list_dataset_dependencies();

    /**
     * @brief Lists dataset dependencies for a specific dataset.
     * @param dataset_code The code of the dataset to query dependencies for
     */
    std::vector<domain::dataset_dependency>
    list_dataset_dependencies_by_dataset(const std::string& dataset_code);

private:
    context ctx_;
    repository::dataset_dependency_repository dataset_dependency_repo_;
};

}

#endif
