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
#ifndef ORES_IAM_REPOSITORY_AUTH_EVENT_ENTITY_HPP
#define ORES_IAM_REPOSITORY_AUTH_EVENT_ENTITY_HPP

#include "sqlgen/PrimaryKey.hpp"
#include <cstdint>
#include <ostream>
#include <string>

namespace ores::iam::repository {

/**
 * @brief Entity for a single authentication event in the auth events hypertable.
 *
 * Records logins, logouts, token refreshes, and related outcomes.
 * No RLS -- this is a system-level audit log.
 *
 * It lived in session_entity.hpp until the session entity model took that
 * file over; the auth events table has no model of its own yet, so the
 * struct keeps its own header. The two primary-key columns are bound as
 * strings, the estate-wide convention for a timestamp column read through
 * the text protocol.
 */
struct auth_event_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_iam_auth_events_tbl";

    /**
     * @brief UUID for this event -- part of composite primary key.
     */
    sqlgen::PrimaryKey<std::string> id;

    /**
     * @brief Event timestamp -- part of composite primary key and partition column.
     */
    sqlgen::PrimaryKey<std::string> event_time;

    /**
     * @brief Tenant identifier. Empty string if unknown (e.g. failed login
     * before tenant resolution).
     */
    std::string tenant_id;

    /**
     * @brief Account identifier. Empty string if unknown.
     */
    std::string account_id;

    /**
     * @brief Event type: login_success, login_failure, logout,
     * token_refresh, max_session_exceeded, signup_success, signup_failure.
     */
    std::string event_type;

    /**
     * @brief Username associated with the event. May be empty for
     * events without a resolved account.
     */
    std::string username;

    /**
     * @brief Session UUID. Empty string if no session was established.
     */
    std::string session_id;

    /**
     * @brief Party UUID. Empty string if no party was selected.
     */
    std::string party_id;

    /**
     * @brief Error detail for failure events. Empty string for success events.
     */
    std::string error_detail;
};

inline std::ostream& operator<<(std::ostream& s, const auth_event_entity& v) {
    s << "id: " << v.id.value() << ", event_time: " << v.event_time.value()
      << ", tenant_id: " << v.tenant_id << ", account_id: " << v.account_id
      << ", event_type: " << v.event_type << ", username: " << v.username
      << ", session_id: " << v.session_id << ", party_id: " << v.party_id
      << ", error_detail: " << v.error_detail;
    return s;
}

}

#endif
