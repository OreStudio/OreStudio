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
#ifndef ORES_SCHEDULER_API_DOMAIN_RETAINED_STORE_SAMPLE_HPP
#define ORES_SCHEDULER_API_DOMAIN_RETAINED_STORE_SAMPLE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <cstdint>
#include <string>
#include <string_view>

namespace ores::scheduler::domain {

/**
 * @brief One measurement of one store's size at one instant; a TimescaleDB hypertable partitioned
 * by sampled_at.
 *
 * A store's size is a series, not an adjective. The sampler measures each store it
 * can reach and writes one row here per store per interval, so a screen can say what
 * a store holds now and which way it has been going.
 *
 * The measurement is keyed by the store's code, not by its target, so the series
 * survives a table being replaced. The catalogue is what says which code a target
 * belongs to, and a store the sampler cannot reach — an object prefix or a folder on
 * a host — simply has no rows: an absent series is the honest answer, and better
 * than a zero.
 *
 * The model states :current_state: and carries no audit columns: a sample is a
 * fact about an instant, so the bi-temporal pair and the actor add nothing to it.
 */
struct retained_store_sample final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key, so the partition column can sit in the primary key beside it.
     */
    boost::uuids::uuid id;

    /**
     * @brief When the measurement was taken.
     */
    std::chrono::system_clock::time_point sampled_at;

    /**
     * @brief The catalogue store measured, as retained_stores.code names it.
     */
    std::string store_code;

    /**
     * @brief What the store held, in bytes, when it was measured.
     */
    std::int64_t size_bytes{};

    /**
     * @brief How many things it held: rows, objects or files, counted the way the catalogue says
     * its size is measured. The count is the store's own estimate where the store keeps one, and
     * zero where it does not.
     */
    std::int64_t measure_count{};

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
    friend bool operator==(const retained_store_sample&, const retained_store_sample&) = default;
};

/**
 * @brief Dispatch-key identifier for retained_store_sample, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const retained_store_sample&) {
    return "ores.scheduler.retained_store_sample";
}

}

#endif
