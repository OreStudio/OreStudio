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
#include "ores.mq/broker/service_registry.hpp"

#include <algorithm>
#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/uuid/uuid_v7_generator.hpp"

namespace ores::mq::broker {

using namespace ores::logging;

boost::uuids::uuid service_registry::register_service(
    const std::string& service_name,
    const std::vector<std::pair<std::uint16_t, std::uint16_t>>& handled_ranges,
    nng_pipe pipe_id) {

    const auto id = utility::uuid::uuid_v7_generator{}();
    std::lock_guard lock(mutex_);
    service_entry entry{
        .assigned_id = id,
        .service_name = service_name,
        .handled_ranges = handled_ranges,
        .pipe_id = pipe_id,
        .connected_at = std::chrono::system_clock::now()
    };
    BOOST_LOG_SEV(lg(), info) << "Registered service '" << service_name
                              << "' assigned_id=" << boost::uuids::to_string(id)
                              << " pipe=" << nng_pipe_id(pipe_id)
                              << " ranges=" << handled_ranges.size();
    services_.push_back(std::move(entry));
    return id;
}

void service_registry::deregister_pipe(nng_pipe pipe_id) {
    std::lock_guard lock(mutex_);
    const int id = nng_pipe_id(pipe_id);
    const auto before = services_.size();
    services_.erase(
        std::remove_if(services_.begin(), services_.end(),
            [id](const service_entry& e) { return nng_pipe_id(e.pipe_id) == id; }),
        services_.end());
    BOOST_LOG_SEV(lg(), info) << "Deregistered " << (before - services_.size())
                              << " service(s) for pipe=" << id;
}

std::vector<service_registry::service_entry> service_registry::all_services() const {
    std::lock_guard lock(mutex_);
    return services_;
}

}
