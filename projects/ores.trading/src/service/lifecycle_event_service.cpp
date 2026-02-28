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
#include "ores.trading/service/lifecycle_event_service.hpp"

#include <stdexcept>

namespace ores::trading::service {

using namespace ores::logging;

lifecycle_event_service::lifecycle_event_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::lifecycle_event> lifecycle_event_service::list_events() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all lifecycle events";
    return repo_.read_latest(ctx_);
}

std::optional<domain::lifecycle_event>
lifecycle_event_service::find_event(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding lifecycle event: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void lifecycle_event_service::save_event(const domain::lifecycle_event& v) {
    if (v.code.empty())
        throw std::invalid_argument("Lifecycle Event code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving lifecycle event: " << v.code;
    repo_.write(ctx_, v);
    BOOST_LOG_SEV(lg(), info) << "Saved lifecycle event: " << v.code;
}

void lifecycle_event_service::save_events(
    const std::vector<domain::lifecycle_event>& events) {
    for (const auto& e : events) {
        if (e.code.empty())
            throw std::invalid_argument("Lifecycle Event code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << events.size() << " lifecycle events";
    repo_.write(ctx_, events);
}

void lifecycle_event_service::remove_event(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing lifecycle event: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed lifecycle event: " << code;
}

void lifecycle_event_service::remove_events(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::lifecycle_event>
lifecycle_event_service::get_event_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for lifecycle event: " << code;
    return repo_.read_all(ctx_, code);
}

}
