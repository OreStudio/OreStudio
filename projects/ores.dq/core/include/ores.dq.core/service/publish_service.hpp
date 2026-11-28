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
#ifndef ORES_DQ_CORE_SERVICE_PUBLISH_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_PUBLISH_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/artefact_type.hpp"
#include "ores.dq.api/domain/dataset.hpp"
#include "ores.dq.api/domain/dataset_dependency.hpp"
#include "ores.dq.api/domain/publication_mode.hpp"
#include "ores.dq.api/domain/publication_result.hpp"
#include "ores.dq.api/messaging/bundle_roster_protocol.hpp"
#include "ores.dq.api/messaging/dataset_publications_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/artefact_type_repository.hpp"
#include "ores.dq.core/repository/dataset_dependency_repository.hpp"
#include "ores.dq.core/repository/dataset_repository.hpp"
#include "ores.logging/logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <map>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

struct bundle_publishable_dataset {
    std::string dataset_id;
    std::string dataset_code;
    std::string target_subject;
    // True only for a dataset that is a direct, opt-in ("optional") member
    // of the bundle being published -- always false for a dependency pulled
    // in by resolve_dependencies rather than listed in the bundle itself,
    // since a prerequisite is never something the caller opts in or out of.
    bool optional = false;
};

/**
 * @brief The order to publish datasets in: the requested codes and every dataset
 * they depend on, each after the datasets it depends on.
 *
 * Where no dependency decides between two datasets, the one requested first is
 * published first, so a bundle publishes in the order its members state. A
 * dependency, requested or not, takes the place of the first dataset that needs
 * it. Throws std::runtime_error when the dependencies form a cycle.
 */
ORES_DQ_CORE_EXPORT std::vector<std::string>
publication_order(const std::vector<std::string>& requested,
                  const std::vector<domain::dataset_dependency>& dependencies);

/**
 * @brief Service for publishing datasets to production tables.
 *
 * This service handles the publication workflow:
 * 1. Resolves dataset dependencies using a directed graph
 * 2. Determines publication order (dependencies first)
 * 3. Calls appropriate population functions for each dataset
 *
 * It records no publication history: a published row names the dataset that
 * wrote it, and that provenance is the record.
 *
 * The service can be used from both the binary protocol handler
 * and HTTP endpoints.
 */
class ORES_DQ_CORE_EXPORT publish_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.publish_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a publish_service.
     *
     * @param ctx The database context for executing queries.
     */
    explicit publish_service(const context& ctx);

    /**
     * @brief Publishes one or more datasets to production tables.
     *
     * This is the main entry point for dataset publication. It:
     * 1. Validates that all dataset IDs exist
     * 2. Resolves dependencies and determines publication order
     * 3. Publishes each dataset in dependency order
     *
     * @param dataset_ids The IDs of the datasets to publish.
     * @param mode How to handle conflicts with existing data.
     * @param published_by Username of the person initiating publication.
     * @param resolve_dependencies If true, automatically include dependencies.
     * @return Results for each dataset published.
     */
    std::vector<domain::publication_result>
    publish(const std::vector<boost::uuids::uuid>& dataset_ids,
            domain::publication_mode mode,
            const std::string& published_by,
            bool resolve_dependencies = true);

    /**
     * @brief Lists publishable datasets in a bundle for workflow dispatch.
     *
     * A thin adapter, not a second resolution path: resolves the bundle's direct member
     * dataset codes to IDs, then delegates to list_publishable_datasets() -- the same
     * dependency-graph-resolving core the ID-based publish path uses. This is what makes a
     * dependency declared in ores_dq_dataset_dependencies_tbl (see
     * dataset_dependency_repository) actually take effect for a bundle publish: previously this
     * method only read direct bundle members (ores_dq_dataset_bundle_members_tbl) and never
     * consulted the dependency graph at all, so bundles whose members depended on
     * un-declared-as-members datasets (e.g. a dataset depending on the real market data it
     * resolves against) silently omitted them unless a bundle's own membership
     * happened to be hand-ordered to include them too.
     *
     * The publish verbs are its only callers: nothing previews a bundle through it, so a member
     * that names no dataset is refused here rather than listed leniently. A preview that must
     * tolerate a missing member asks the store instead, through
     * ores_dq_bundle_datasets_list_fn and ores_dq_bundle_preview_fn, which report the gap as a
     * metric and do not raise.
     *
     * @param bundle_code The bundle to query (e.g., 'world', 'crypto').
     * @param resolve_dependencies If true (the default), transitive dependencies of bundle
     * members are included and ordered before their dependents. Kept as a caller-visible option,
     * matching list_publishable_datasets(), rather than silently hardcoded, for the (currently
     * unused) case of previewing bundle membership without its dependency closure.
     * @return Ordered list of publishable dataset entries.
     */
    std::vector<bundle_publishable_dataset>
    list_bundle_publishable_datasets(const std::string& bundle_code,
                                     bool resolve_dependencies = true);

    /**
     * @brief Reads the publication level a bundle states.
     *
     * The level decides the tenant the publication writes into and the callers
     * it admits. A platform or world bundle publishes into the system tenant;
     * a tenant bundle publishes into the tenant that asked.
     *
     * @param bundle_code The bundle to read (e.g., 'world').
     * @return The level the bundle states, or nothing when no such bundle exists.
     */
    std::optional<std::string> bundle_level(const std::string& bundle_code);

    /**
     * @brief Reads the roster: every bundle, the level it publishes at and the
     * number of datasets it carries.
     *
     * The staged count is not in this answer. It costs one count per member's
     * artefact table, so it is taken only for a bundle a caller names.
     *
     * @return The bundles, ordered by code.
     */
    std::vector<messaging::bundle_roster_bundle> list_bundle_roster();

    /**
     * @brief Reads one bundle's members, each with the number of rows its
     * artefact table has staged.
     *
     * A member whose artefact type names a family of tables states the sum of
     * what all of them hold, because a row is staged if it is in any of them.
     *
     * @param bundle_code The bundle to read (e.g., 'world').
     * @return The members, in the bundle's own order; empty for a bundle that
     *         carries none.
     */
    std::vector<messaging::bundle_roster_member>
    list_bundle_members(const std::string& bundle_code);

    /**
     * @brief Reads the runs that published one dataset, newest first.
     *
     * The log is append-only, so a page is the whole history of a dataset that
     * anyone reads. A run that failed is in it, with what it said.
     *
     * @param dataset_code The dataset whose runs to read.
     * @return The runs, newest first.
     */
    std::vector<messaging::dataset_publication>
    list_dataset_publications(const std::string& dataset_code);

    /**
     * @brief Reads the bundle that carries a dataset.
     *
     * A dataset's own page knows the dataset and not the bundle, so the roster
     * resolves it here. Membership rows of a shared bundle live in the system
     * tenant, so the read states no tenant predicate and the policy decides, as
     * the roster's own functions do.
     *
     * @param dataset_code The dataset to look up (e.g., 'iso.countries').
     * @return The bundle's code, or nothing when no bundle carries the dataset.
     */
    std::optional<std::string> bundle_of_dataset(const std::string& dataset_code);

    /**
     * @brief Resolves a list of dataset IDs into ordered, publishable entries.
     *
     * Combines dependency resolution with artefact-type lookup to produce
     * the same bundle_publishable_dataset shape used by the workflow engine.
     * Datasets without a configured target_subject are silently skipped.
     *
     * @param dataset_ids The IDs requested by the caller.
     * @param resolve_dependencies If true, transitive dependencies are included.
     * @return Ordered entries ready for workflow dispatch.
     */
    std::vector<bundle_publishable_dataset>
    list_publishable_datasets(const std::vector<boost::uuids::uuid>& dataset_ids,
                              bool resolve_dependencies);

    /**
     * @brief Resolves the publication order for datasets.
     *
     * Reads the datasets and their dependencies and orders them with
     * publication_order, so dependencies come first and the rest keep the
     * order of @p dataset_ids.
     *
     * @param dataset_ids The IDs of the datasets to order.
     * @return Ordered list of datasets (dependencies first).
     */
    std::vector<domain::dataset>
    resolve_publication_order(const std::vector<boost::uuids::uuid>& dataset_ids);


private:
    /**
     * @brief Builds a cache of artefact types for the given datasets.
     *
     * @param datasets The datasets to build cache for.
     * @return Map from artefact type code to artefact type.
     */
    std::map<std::string, domain::artefact_type>
    build_artefact_type_cache(const std::vector<domain::dataset>& datasets);

    /**
     * @brief Publishes a single dataset.
     *
     * Determines the artefact type and calls the appropriate
     * dq_populate_* function.
     *
     * @param dataset The dataset to publish.
     * @param mode The publication mode.
     * @param artefact_type_cache Cache of artefact types to avoid DB queries.
     * @return Publication result with counts.
     */
    domain::publication_result
    publish_dataset(const domain::dataset& dataset,
                    domain::publication_mode mode,
                    const std::string& published_by,
                    const std::map<std::string, domain::artefact_type>& artefact_type_cache);

    /**
     * @brief Records a publication in the per-dataset run log.
     *
     * The workflow path does not call this: it logs no run of its own, and the
     * provenance on each published row is the record. Only the retired
     * in-process publish path still reaches it.
     *
     * @param result The publication result to record.
     * @param mode The publication mode used.
     * @param published_by The username of the publisher.
     */
    void record_publication(const domain::publication_result& result,
                            domain::publication_mode mode,
                            const std::string& published_by);

    /**
     * @brief Calls the artefact type's NATS target subject via SQL (single dataset).
     *
     * Used by the `publish` path (individual dataset publication).
     */
    domain::publication_result call_populate_function(const domain::dataset& dataset,
                                                      const domain::artefact_type& artefact_type,
                                                      domain::publication_mode mode,
                                                      const std::string& published_by);

    context ctx_;
    repository::dataset_repository dataset_repo_;
    repository::dataset_dependency_repository dependency_repo_;
    repository::artefact_type_repository artefact_type_repo_;
};

}

#endif
