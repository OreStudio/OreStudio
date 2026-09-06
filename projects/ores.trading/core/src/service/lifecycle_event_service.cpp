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
#include "ores.trading.core/service/lifecycle_event_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

lifecycle_event_service::lifecycle_event_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::lifecycle_event> lifecycle_event_service::list_events(std::uint32_t offset,
                                                                          std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all lifecycle events";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t lifecycle_event_service::count_events() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total lifecycle events count";
    return repo_.get_total_event_count(ctx_);
}


std::optional<domain::lifecycle_event>
lifecycle_event_service::get_event_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting lifecycle event at version. " << "code: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::lifecycle_event> lifecycle_event_service::get_event(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting lifecycle event. " << "code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void lifecycle_event_service::save_event(const domain::lifecycle_event& v) {
    if (v.code.empty())
        throw std::invalid_argument("Lifecycle Event code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving lifecycle event. " << "code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved lifecycle event. " << "code: " << v.code;
}

void lifecycle_event_service::save_events(const std::vector<domain::lifecycle_event>& events) {
    for (const auto& e : events) {
        if (e.code.empty())
            throw std::invalid_argument("Lifecycle Event code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << events.size() << " lifecycle events";
    auto ts = events;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void lifecycle_event_service::delete_event(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing lifecycle event. " << "code: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed lifecycle event. " << "code: " << code;
}

void lifecycle_event_service::delete_events(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::lifecycle_event>
lifecycle_event_service::get_event_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for lifecycle event. " << "code: " << code;
    return repo_.read_all(ctx_, code);
}

}
