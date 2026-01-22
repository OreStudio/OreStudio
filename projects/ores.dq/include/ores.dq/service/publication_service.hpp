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
#ifndef ORES_DQ_SERVICE_PUBLICATION_SERVICE_HPP
#define ORES_DQ_SERVICE_PUBLICATION_SERVICE_HPP

#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq/domain/dataset.hpp"
#include "ores.dq/domain/publication.hpp"
#include "ores.dq/domain/publication_mode.hpp"
#include "ores.dq/domain/publication_result.hpp"
#include "ores.dq/repository/dataset_repository.hpp"
#include "ores.dq/repository/dataset_dependency_repository.hpp"
#include "ores.dq/repository/publication_repository.hpp"

namespace ores::dq::service {

/**
 * @brief Service for publishing datasets to production tables.
 *
 * This service handles the publication workflow:
 * 1. Resolves dataset dependencies using a directed graph
 * 2. Determines publication order (dependencies first)
 * 3. Calls appropriate population functions for each dataset
 * 4. Records publication history for auditing
 *
 * The service can be used from both the binary protocol handler
 * and HTTP endpoints.
 */
class publication_service {
private:
    inline static std::string_view logger_name =
        "ores.dq.service.publication_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a publication_service.
     *
     * @param ctx The database context for executing queries.
     */
    explicit publication_service(context ctx);

    /**
     * @brief Publishes one or more datasets to production tables.
     *
     * This is the main entry point for dataset publication. It:
     * 1. Validates that all dataset IDs exist
     * 2. Resolves dependencies and determines publication order
     * 3. Publishes each dataset in dependency order
     * 4. Records publication history for auditing
     *
     * @param dataset_ids The IDs of the datasets to publish.
     * @param mode How to handle conflicts with existing data.
     * @param published_by Username of the person initiating publication.
     * @param resolve_dependencies If true, automatically include dependencies.
     * @return Results for each dataset published.
     */
    std::vector<domain::publication_result> publish(
        const std::vector<boost::uuids::uuid>& dataset_ids,
        domain::publication_mode mode,
        const std::string& published_by,
        bool resolve_dependencies = true);

    /**
     * @brief Resolves the publication order for datasets.
     *
     * Uses boost.graph to build a dependency graph and performs
     * topological sort to determine the correct order. Dependencies
     * are placed before the datasets that depend on them.
     *
     * @param dataset_ids The IDs of the datasets to order.
     * @return Ordered list of datasets (dependencies first).
     */
    std::vector<domain::dataset> resolve_publication_order(
        const std::vector<boost::uuids::uuid>& dataset_ids);

    /**
     * @brief Gets the publication history for a dataset.
     *
     * @param dataset_id The ID of the dataset.
     * @return All publication records for this dataset, newest first.
     */
    std::vector<domain::publication> get_publication_history(
        const boost::uuids::uuid& dataset_id);

    /**
     * @brief Gets recent publication history across all datasets.
     *
     * @param limit Maximum number of records to return.
     * @return Recent publication records, newest first.
     */
    std::vector<domain::publication> get_recent_publications(
        std::uint32_t limit = 100);

private:
    /**
     * @brief Publishes a single dataset.
     *
     * Determines the artefact type and calls the appropriate
     * dq_populate_* function.
     *
     * @param dataset The dataset to publish.
     * @param mode The publication mode.
     * @return Publication result with counts.
     */
    domain::publication_result publish_dataset(
        const domain::dataset& dataset,
        domain::publication_mode mode);

    /**
     * @brief Records a publication in the audit table.
     *
     * @param result The publication result to record.
     * @param mode The publication mode used.
     * @param published_by The username of the publisher.
     */
    void record_publication(
        const domain::publication_result& result,
        domain::publication_mode mode,
        const std::string& published_by);

    /**
     * @brief Calls the dataset's populate function.
     *
     * @param dataset The dataset to publish.
     * @param mode The publication mode.
     * @return Publication result from the database function.
     */
    domain::publication_result call_populate_function(
        const domain::dataset& dataset,
        domain::publication_mode mode);

    context ctx_;
    repository::dataset_repository dataset_repo_;
    repository::dataset_dependency_repository dependency_repo_;
    repository::publication_repository publication_repo_;
};

}

#endif
