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
#ifndef ORES_DQ_API_MESSAGING_PUBLISH_FROM_DQ_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_PUBLISH_FROM_DQ_PROTOCOL_HPP

#include <cstdint>
#include <string>

namespace ores::dq::messaging {

/**
 * @brief Command payload sent by the DQ workflow engine to a target service's
 *        publish-from-dq NATS subject.
 *
 * The receiving handler calls a SECURITY DEFINER SQL function that reads the
 * DQ artefact table for dataset_id and writes to the target service's tables.
 */
struct publish_from_dq_command {
    std::string dataset_id;    // UUID of the DQ dataset to publish
    std::string tenant_id;     // UUID of the target tenant
    std::string mode;          // "upsert" | "replace_all" | "insert_only"
    std::string params_json;   // extra per-artefact parameters (may be "{}")
};

/**
 * @brief Result returned by the target service's publish-from-dq handler.
 *
 * Serialised as JSON and passed to wf->complete() so the workflow engine can
 * record counts and propagate them to the bundle publish result.
 */
struct publish_from_dq_result {
    bool success = false;
    std::string error_message;
    std::uint64_t records_inserted = 0;
    std::uint64_t records_updated  = 0;
    std::uint64_t records_skipped  = 0;
    std::uint64_t records_deleted  = 0;
};

}

#endif
