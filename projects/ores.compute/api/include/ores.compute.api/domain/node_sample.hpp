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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_COMPUTE_API_DOMAIN_NODE_SAMPLE_HPP
#define ORES_COMPUTE_API_DOMAIN_NODE_SAMPLE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string_view>

namespace ores::compute::domain {

/**
 * @brief One wrapper node's report of what it has done since its last sample; a TimescaleDB
 * hypertable partitioned by sampled_at.
 *
 * One wrapper node's report, published fire-and-forget on
 * compute.v1.telemetry.node_samples and written here by the service. The
 * rows are append-only, one per node per interval, and the dashboard shows
 * the newest row of each node.
 *
 * The model states :current_state: for the same reason grid_sample does:
 * a sample is a fact about an instant, so the bi-temporal pair has no
 * meaning on it.
 *
 * The per-node newest read stays hand-written. It is a
 * DISTINCT ON (host_id) over the tenant's rows, which no generated read
 * expresses: a read by host_id returns one node's rows, and a read of the
 * newest returns the grid's newest and not one row per node.
 */
struct node_sample final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key, so the partition column can sit in the primary key beside it.
     */
    boost::uuids::uuid id;

    /**
     * @brief When the sample was taken.
     */
    std::chrono::system_clock::time_point sampled_at;

    /**
     * @brief The reporting node.
     */
    boost::uuids::uuid host_id;

    /**
     * @brief Tasks the node finished since its wrapper started.
     */
    int tasks_completed = 0;

    /**
     * @brief Tasks that failed since the wrapper started.
     */
    int tasks_failed = 0;

    /**
     * @brief Tasks the node finished since its previous sample.
     */
    int tasks_since_last = 0;

    /**
     * @brief Mean task duration over the tasks since the last sample.
     */
    std::int64_t avg_task_duration_ms;

    /**
     * @brief Longest task duration over the tasks since the last sample.
     */
    std::int64_t max_task_duration_ms;

    /**
     * @brief Bytes the node fetched for those tasks.
     */
    std::int64_t input_bytes_fetched;

    /**
     * @brief Bytes the node uploaded for those tasks.
     */
    std::int64_t output_bytes_uploaded;

    /**
     * @brief Seconds since the node last heartbeated.
     */
    int seconds_since_hb = 0;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const node_sample&, const node_sample&) = default;
};

/**
 * @brief Dispatch-key identifier for node_sample, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const node_sample&) {
    return "ores.compute.node_sample";
}

}

#endif
