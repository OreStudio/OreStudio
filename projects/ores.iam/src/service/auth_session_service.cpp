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
#include "ores.iam/service/auth_session_service.hpp"

#include <vector>

namespace ores::iam::service {

using namespace ores::logging;

void auth_session_service::add_session(const std::string& token,
    ores::iam::domain::session session) {
    std::lock_guard<std::mutex> lock(mutex_);
    sessions_.emplace(token, std::move(session));
    BOOST_LOG_SEV(lg(), debug) << "Session added for token (length "
                               << token.size() << ")";
}

void auth_session_service::remove_session(const std::string& token) {
    std::lock_guard<std::mutex> lock(mutex_);
    sessions_.erase(token);
    BOOST_LOG_SEV(lg(), debug) << "Session removed for token (length "
                               << token.size() << ")";
}

std::optional<ores::iam::domain::session>
auth_session_service::find_session(const std::string& token) const {
    std::lock_guard<std::mutex> lock(mutex_);
    auto it = sessions_.find(token);
    if (it == sessions_.end()) {
        return std::nullopt;
    }
    return it->second;
}

std::vector<ores::iam::domain::session>
auth_session_service::get_all_sessions() const {
    std::lock_guard<std::mutex> lock(mutex_);
    std::vector<ores::iam::domain::session> result;
    result.reserve(sessions_.size());
    for (const auto& [token, sess] : sessions_) {
        result.push_back(sess);
    }
    return result;
}

}
