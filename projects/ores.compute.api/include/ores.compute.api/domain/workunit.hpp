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
#ifndef ORES_COMPUTE_DOMAIN_WORKUNIT_HPP
#define ORES_COMPUTE_DOMAIN_WORKUNIT_HPP

#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::compute::domain {

/**
 * @brief An abstract job template within a compute batch.
 *
 * Defines the problem to be solved: which app version to use, where the input
 * data lives, and how many times to run it for redundancy. The BOINC equivalent
 * of 'workunit'. Does not track execution state — that belongs to results.
 */
struct workunit final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID primary key for the workunit.
     */
    boost::uuids::uuid id;

    /**
     * @brief FK reference to ores_compute_batches_tbl.
     */
    boost::uuids::uuid batch_id;

    /**
     * @brief FK reference to ores_compute_app_versions_tbl.
     */
    boost::uuids::uuid app_version_id;

    /**
     * @brief URI pointing to the zipped input data bundle in object storage.
     */
    std::string input_uri;

    /**
     * @brief URI pointing to the engine config file (ORE XML, Llama JSON, etc.).
     */
    std::string config_uri;

    /**
     * @brief Scheduling priority; higher values are dispatched sooner.
     */
    int priority;

    /**
     * @brief Number of independent results required before this workunit is complete.
     */
    int target_redundancy;

    /**
     * @brief FK to the validated canonical result; NULL until the Validator accepts a result.
     */
    boost::uuids::uuid canonical_result_id;

    /**
     * @brief Username of the person who last modified this workunit.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
