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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_RATE_ENGINE_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_RATE_ENGINE_HPP

#include "ores.analytics.quant/domain/crm_topology.hpp"
#include "ores.analytics.quant/domain/derived_rate.hpp"
#include "ores.analytics.quant/domain/driver_quote.hpp"
#include "ores.analytics.quant/domain/rate_snapshot.hpp"
#include "ores.analytics.quant/domain/staleness_policy.hpp"
#include "ores.analytics.quant/export.hpp"
#include <chrono>
#include <immer/atom.hpp>
#include <string>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief The CRM's runtime rate engine (phase 2): continuously ingests
 * driver ticks and serves batched derived-rate reads.
 *
 * Holds a fixed @c domain::crm_topology (never mutated after construction)
 * plus an @c immer::atom<domain::rate_snapshot> for the per-vertex
 * cumulative rate state. Not multi-threaded itself -- it spawns no
 * threads -- but is thread-safe: @c update() is meant to be called from a
 * single producer thread; @c rate()/@c rates() may be called concurrently
 * from any number of reader threads without ever blocking the producer or
 * each other, and a reader never observes a torn mix of old/new values
 * within one @c rates() batch because it loads exactly one snapshot for
 * the whole call.
 *
 * Updating one driver edge only recomputes the subtree hanging below it
 * (the tree structure bounds the blast radius), via structural sharing so
 * only the touched entries are copied into the new snapshot.
 */
class ORES_ANALYTICS_QUANT_EXPORT rate_engine {
public:
    rate_engine(domain::crm_topology topology,
                domain::staleness_policy policy,
                std::chrono::system_clock::time_point now = std::chrono::system_clock::now());

    /// Applies one tick to a driver edge already present in the topology.
    /// Throws @c std::invalid_argument if either code is unknown or the
    /// pair is not an edge of this engine's topology.
    void update(const domain::driver_quote& quote);

    /// @param now defaults to the wall clock; tests may inject a fixed
    /// value for deterministic staleness assertions.
    [[nodiscard]] domain::derived_rate
    rate(const std::string& base_code,
         const std::string& quote_code,
         std::chrono::system_clock::time_point now = std::chrono::system_clock::now()) const;

    /// One atomic snapshot load for the whole batch, then O(N) pure
    /// arithmetic -- no per-pair locking.
    [[nodiscard]] std::vector<domain::derived_rate>
    rates(const std::vector<std::pair<std::string, std::string>>& pairs,
          std::chrono::system_clock::time_point now = std::chrono::system_clock::now()) const;

private:
    domain::derived_rate rate_from_snapshot(const domain::rate_snapshot& snapshot,
                                            const std::string& base_code,
                                            const std::string& quote_code,
                                            std::chrono::system_clock::time_point now) const;

    domain::crm_topology topology_;
    domain::staleness_policy policy_;
    /// children_[v.index()] lists every vertex whose parent is v; built
    /// once from the fixed topology so update() can walk a subtree without
    /// re-deriving tree structure on every tick.
    std::vector<std::vector<domain::currency_id>> children_;
    immer::atom<domain::rate_snapshot> snapshot_;
};

} // namespace ores::analytics::quant::service

#endif
