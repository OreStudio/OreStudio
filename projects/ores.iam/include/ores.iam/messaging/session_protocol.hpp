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
#ifndef ORES_IAM_MESSAGING_SESSION_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_SESSION_PROTOCOL_HPP

#include <chrono>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.iam/domain/session.hpp"

namespace ores::iam::messaging {

struct list_sessions_request {
    boost::uuids::uuid account_id;
    int limit = 50;
    int offset = 0;
};

struct list_sessions_response {
    std::vector<ores::iam::domain::session> sessions;
    int total_count = 0;
};

struct get_active_sessions_request {};

struct get_active_sessions_response {
    std::vector<ores::iam::domain::session> sessions;
};

struct get_session_statistics_request {
    boost::uuids::uuid account_id;
    std::chrono::system_clock::time_point start_time;
    std::chrono::system_clock::time_point end_time;
};

struct get_session_statistics_response {
    std::vector<ores::iam::domain::session_statistics> statistics;
};

}

#endif
