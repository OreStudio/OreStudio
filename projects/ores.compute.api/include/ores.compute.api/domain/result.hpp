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
#ifndef ORES_COMPUTE_DOMAIN_RESULT_HPP
#define ORES_COMPUTE_DOMAIN_RESULT_HPP

#include <string>
#include <cstdint>
#include <chrono>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::compute::domain {

/**
 * @brief A specific execution instance of a workunit assigned to a host.
 *
 * Bridges the workunit definition and the actual execution on a grid node.
 * Tracks PGMQ lease state, server-side lifecycle (Inactive/Unsent/InProgress/Done),
 * and the location of output data. The BOINC equivalent of 'result'.
 */
struct result final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID primary key for the result.
     */
    boost::uuids::uuid id;

    /**
     * @brief FK reference to ores_compute_workunits_tbl.
     */
    boost::uuids::uuid workunit_id;

    /**
     * @brief FK reference to ores_compute_hosts_tbl; NULL until the result is dispatched.
     */
    boost::uuids::uuid host_id;

    /**
     * @brief PGMQ lease pointer; NULL when not actively queued.
     */
    std::int64_t pgmq_msg_id;

    /**
     * @brief State machine: 1=Inactive, 2=Unsent, 4=InProgress, 5=Done.
     */
    int server_state;

    /**
     * @brief Result outcome code: 1=Success, 3=ClientError, 4=NoReply.
     */
    int outcome;

    /**
     * @brief URI where the wrapper uploaded the zipped output; NULL until completed.
     */
    std::string output_uri;

    /**
     * @brief Human-readable error description from the wrapper; empty on success.
     */
    std::string error_message;

    /**
     * @brief Timestamp when the output was received by the server pool.
     */
    std::chrono::system_clock::time_point received_at;

    /**
     * @brief Username of the person who last modified this compute result.
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
