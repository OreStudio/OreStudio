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
 * Template: cpp_service.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.iam.core/service/session_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::iam::service {

using namespace ores::logging;

session_service::session_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::session> session_service::list_sessions(std::uint32_t offset,
                                                            std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all sessions";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t session_service::count_sessions() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total sessions count";
    return repo_.get_total_session_count(ctx_);
}


std::optional<domain::session> session_service::get_session(const std::string& id,
                                                            const std::string& start_time) {
    BOOST_LOG_SEV(lg(), debug) << "Getting session. " << "id: " << id
                               << " start_time: " << start_time;
    auto results = repo_.read_latest(ctx_, id, start_time);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::session>
session_service::get_sessions(const std::vector<std::string>& ids,
                              const std::vector<std::string>& start_times) {
    return repo_.read_latest(ctx_, ids, start_times);
}

void session_service::save_session(const domain::session& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Session id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving session. " << "id: " << v.id
                               << " start_time: " << v.start_time;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved session. " << "id: " << v.id
                              << " start_time: " << v.start_time;
}

void session_service::save_sessions(const std::vector<domain::session>& sessions) {
    for (const auto& e : sessions) {
        if (e.id.is_nil())
            throw std::invalid_argument("Session id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << sessions.size() << " sessions";
    auto ts = sessions;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void session_service::delete_session(const std::string& id, const std::string& start_time) {
    BOOST_LOG_SEV(lg(), debug) << "Removing session. " << "id: " << id
                               << " start_time: " << start_time;
    repo_.remove(ctx_, id, start_time);
    BOOST_LOG_SEV(lg(), info) << "Removed session. " << "id: " << id
                              << " start_time: " << start_time;
}

void session_service::delete_sessions(const std::vector<std::string>& ids,
                                      const std::vector<std::string>& start_times) {
    repo_.remove(ctx_, ids, start_times);
}


}
