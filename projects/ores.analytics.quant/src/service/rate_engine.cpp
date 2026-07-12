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
#include "ores.analytics.quant/service/rate_engine.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>

namespace ores::analytics::quant::service {

using domain::ccy_pair;
using domain::currency_id;
using domain::derived_rate;
using domain::driver_quote;
using domain::rate_snapshot;
using domain::rate_status;
using domain::vertex_state;

namespace {

immer::vector<vertex_state> make_initial_states(const domain::crm_topology& topology,
                                                std::chrono::system_clock::time_point now) {
    immer::vector<vertex_state> states;
    auto transient = states.transient();
    for (std::size_t v = 0; v < topology.vertex_count(); ++v)
        transient.push_back(vertex_state{});

    vertex_state pivot_state;
    pivot_state.valid = true;
    pivot_state.edge_valid = true; // trivially: the pivot needs no edge
    pivot_state.as_of = now;
    transient.set(topology.pivot().index(), pivot_state);

    return transient.persistent();
}

} // namespace

rate_engine::rate_engine(domain::crm_topology topology,
                         domain::staleness_policy policy,
                         std::chrono::system_clock::time_point now)
    : topology_(std::move(topology))
    , policy_(policy)
    , children_(topology_.vertex_count())
    , snapshot_(rate_snapshot(make_initial_states(topology_, now))) {
    for (std::size_t v = 0; v < topology_.vertex_count(); ++v) {
        const currency_id id(static_cast<std::uint16_t>(v));
        if (id == topology_.pivot())
            continue;
        children_[topology_.parent(id).index()].push_back(id);
    }
}

void rate_engine::update(const driver_quote& quote) {
    const auto base_id = topology_.currency_id_for(quote.base_code);
    const auto quote_id = topology_.currency_id_for(quote.quote_code);
    if (!base_id || !quote_id) {
        throw std::invalid_argument("rate_engine::update: unknown currency in " + quote.base_code +
                                    "/" + quote.quote_code);
    }

    // Determine which side is the child in the tree, and the sign of the
    // log-rate delta for that direction: quote = base * rate, so
    // log_rate[quote] = log_rate[base] + log(rate); if base is the child
    // (parent is quote), the sign inverts.
    currency_id child;
    double sign;
    if (topology_.parent(*base_id) == *quote_id) {
        child = *base_id;
        sign = -1.0;
    } else if (topology_.parent(*quote_id) == *base_id) {
        child = *quote_id;
        sign = 1.0;
    } else {
        throw std::invalid_argument("rate_engine::update: " + quote.base_code + "/" +
                                    quote.quote_code + " is not an edge of this topology");
    }

    const auto current = snapshot_.load().get();
    auto transient = current.transient();

    const double log_delta = sign * std::log(quote.rate);

    // Depth-first walk of the subtree rooted at `child`: each vertex's
    // edge_log_delta/edge_observed_at is only touched for `child` itself
    // (everything below keeps its own edge unchanged), but every
    // descendant's cumulative log_rate/as_of/valid must be recomputed
    // because its parent's cumulative values just changed.
    std::vector<currency_id> stack{child};
    bool first = true;
    while (!stack.empty()) {
        const auto v = stack.back();
        stack.pop_back();

        vertex_state state = transient[v.index()];
        if (first) {
            state.edge_log_delta = log_delta;
            state.edge_observed_at = quote.observed_at;
            state.edge_valid = true;
            first = false;
        }
        const auto& parent_state = transient[topology_.parent(v).index()];
        state.valid = parent_state.valid && state.edge_valid;
        state.log_rate = parent_state.log_rate + state.edge_log_delta;
        state.as_of = std::min(parent_state.as_of, state.edge_observed_at);
        transient.set(v.index(), state);

        for (const auto& descendant : children_[v.index()])
            stack.push_back(descendant);
    }

    snapshot_.store(rate_snapshot(transient.persistent()));
}

derived_rate rate_engine::rate_from_snapshot(const rate_snapshot& snapshot,
                                             const std::string& base_code,
                                             const std::string& quote_code,
                                             std::chrono::system_clock::time_point now) const {
    const auto base_id = topology_.currency_id_for(base_code);
    const auto quote_id = topology_.currency_id_for(quote_code);
    if (!base_id || !quote_id) {
        return derived_rate{base_code,
                            quote_code,
                            0.0,
                            rate_status::unavailable,
                            std::chrono::system_clock::time_point{}};
    }

    const auto& base_state = snapshot.at(*base_id);
    const auto& quote_state = snapshot.at(*quote_id);
    if (!base_state.valid || !quote_state.valid) {
        return derived_rate{base_code,
                            quote_code,
                            0.0,
                            rate_status::unavailable,
                            std::chrono::system_clock::time_point{}};
    }

    const double rate = std::exp(quote_state.log_rate - base_state.log_rate);
    const auto oldest = std::min(base_state.as_of, quote_state.as_of);
    const auto status = policy_.evaluate(now - oldest);
    return derived_rate{base_code, quote_code, rate, status, oldest};
}

derived_rate rate_engine::rate(const std::string& base_code,
                               const std::string& quote_code,
                               std::chrono::system_clock::time_point now) const {
    const auto snapshot = snapshot_.load().get();
    return rate_from_snapshot(snapshot, base_code, quote_code, now);
}

std::vector<derived_rate>
rate_engine::rates(const std::vector<std::pair<std::string, std::string>>& pairs,
                   std::chrono::system_clock::time_point now) const {
    // Exactly one atomic snapshot load for the whole batch.
    const auto snapshot = snapshot_.load().get();
    std::vector<derived_rate> result;
    result.reserve(pairs.size());
    for (const auto& [base_code, quote_code] : pairs) {
        result.push_back(rate_from_snapshot(snapshot, base_code, quote_code, now));
    }
    return result;
}

} // namespace ores::analytics::quant::service
