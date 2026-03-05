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
#include "ores.mq/broker/routing_table.hpp"

#include <algorithm>

namespace ores::mq::broker {

using namespace ores::logging;

void routing_table::register_route(std::uint16_t min, std::uint16_t max,
    nng_pipe pipe_id) {
    std::lock_guard lock(mutex_);
    BOOST_LOG_SEV(lg(), debug) << "Registering route [" << std::hex << min
                               << ", " << max << std::dec << "] pipe="
                               << nng_pipe_id(pipe_id);
    routes_.push_back({.min = min, .max = max, .pipe_id = pipe_id});
}

void routing_table::deregister_pipe(nng_pipe pipe_id) {
    std::lock_guard lock(mutex_);
    const int id = nng_pipe_id(pipe_id);
    const auto before = routes_.size();
    routes_.erase(
        std::remove_if(routes_.begin(), routes_.end(),
            [id](const route_entry& e) { return nng_pipe_id(e.pipe_id) == id; }),
        routes_.end());
    BOOST_LOG_SEV(lg(), info) << "Deregistered " << (before - routes_.size())
                              << " routes for pipe=" << id;
}

std::optional<nng_pipe> routing_table::find_pipe(std::uint16_t msg_type) const {
    std::lock_guard lock(mutex_);
    for (const auto& e : routes_) {
        if (msg_type >= e.min && msg_type <= e.max)
            return e.pipe_id;
    }
    return std::nullopt;
}

}
