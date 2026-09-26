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
#ifndef ORES_COMPUTE_API_DOMAIN_GRID_SAMPLE_HPP
#define ORES_COMPUTE_API_DOMAIN_GRID_SAMPLE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string_view>

namespace ores::compute::domain {

/**
 * @brief One point-in-time snapshot of server-side compute grid metrics; a TimescaleDB hypertable
 * partitioned by sampled_at.
 *
 * One snapshot of the grid, written by the compute service's poller on its
 * own interval. The counters mirror what the database holds at that moment,
 * so a dashboard reads one stored row rather than aggregating on every
 * refresh.
 *
 * The table is append-only and carries no validity window: a sample is a
 * fact about an instant, and nothing edits it. That is why the model states
 * :current_state:, which drops the transaction-time pair the bi-temporal
 * shape would add and lets the partition column sit in the primary key.
 *
 * The reads the codegen cannot express live beside it. A per-node newest row
 * is a DISTINCT ON (host_id) and the live summary is a call to
 * ores_compute_grid_stats_fn, so both stay hand-written in
 * compute_telemetry_repository.
 */
struct grid_sample final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key. A hypertable's partition column must sit in the primary key, and a
     * surrogate key lets it do so without the tenant joining the key.
     */
    boost::uuids::uuid id;

    /**
     * @brief When the sample was taken.
     */
    std::chrono::system_clock::time_point sampled_at;

    /**
     * @brief Total registered hosts.
     */
    int total_hosts = 0;

    /**
     * @brief Hosts whose last heartbeat is inside the reaper window.
     */
    int online_hosts = 0;

    /**
     * @brief Online hosts with no result in flight.
     */
    int idle_hosts = 0;

    /**
     * @brief Results in server state 1.
     */
    int results_inactive = 0;

    /**
     * @brief Results in server state 2.
     */
    int results_unsent = 0;

    /**
     * @brief Results in server state 4.
     */
    int results_in_progress = 0;

    /**
     * @brief Results in server state 5.
     */
    int results_done = 0;

    /**
     * @brief Workunits the tenant holds.
     */
    int total_workunits = 0;

    /**
     * @brief Batches the tenant holds.
     */
    int total_batches = 0;

    /**
     * @brief Batches that are not closed.
     */
    int active_batches = 0;

    /**
     * @brief Results that finished with outcome 1 in the last day.
     */
    int outcomes_success = 0;

    /**
     * @brief Results that finished with outcome 3 in the last day.
     */
    int outcomes_client_error = 0;

    /**
     * @brief Results that finished with outcome 4 in the last day.
     */
    int outcomes_no_reply = 0;

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
    friend bool operator==(const grid_sample&, const grid_sample&) = default;
};

/**
 * @brief Dispatch-key identifier for grid_sample, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const grid_sample&) {
    return "ores.compute.grid_sample";
}

}

#endif
