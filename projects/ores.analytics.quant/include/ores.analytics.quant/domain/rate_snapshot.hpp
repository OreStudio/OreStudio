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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_RATE_SNAPSHOT_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_RATE_SNAPSHOT_HPP

#include "ores.analytics.quant/domain/currency_id.hpp"
#include "ores.analytics.quant/domain/vertex_state.hpp"
#include <immer/vector.hpp>
#include <immer/vector_transient.hpp>

namespace ores::analytics::quant::domain {

/**
 * @brief An immutable, point-in-time view of every currency's cumulative
 * rate state, indexed by @c currency_id.
 *
 * Wraps an @c immer::vector so producing a new snapshot after an update
 * only copies the touched entries (structural sharing) -- never the whole
 * vector. Held behind an @c immer::atom by @c rate_engine so readers get a
 * wait-free, torn-read-free view without any locking.
 */
class rate_snapshot {
public:
    explicit rate_snapshot(immer::vector<vertex_state> states)
        : states_(std::move(states)) {}

    [[nodiscard]] const vertex_state& at(currency_id id) const {
        return states_.at(id.index());
    }

    [[nodiscard]] std::size_t size() const noexcept {
        return states_.size();
    }

    /// A transient copy for a batch of updates; caller persists it back
    /// into a new @c rate_snapshot once done.
    [[nodiscard]] immer::vector<vertex_state>::transient_type transient() const {
        return states_.transient();
    }

private:
    immer::vector<vertex_state> states_;
};

} // namespace ores::analytics::quant::domain

#endif
