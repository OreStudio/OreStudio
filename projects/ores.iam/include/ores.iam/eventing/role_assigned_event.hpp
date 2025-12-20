/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_IAM_EVENTING_ROLE_ASSIGNED_EVENT_HPP
#define ORES_IAM_EVENTING_ROLE_ASSIGNED_EVENT_HPP

#include <chrono>
#include <boost/uuid/uuid.hpp>
#include "ores.eventing/domain/event_traits.hpp"

namespace ores::iam::eventing {

/**
 * @brief Domain event indicating that a role has been assigned to an account.
 *
 * This event is published when a role is assigned to an account.
 * Subscribers can use this to invalidate cached permissions or
 * update session information.
 */
struct role_assigned_event final {
    /**
     * @brief The account that received the role.
     */
    boost::uuids::uuid account_id;

    /**
     * @brief The role that was assigned.
     */
    boost::uuids::uuid role_id;

    /**
     * @brief The timestamp of when the assignment occurred (in UTC).
     */
    std::chrono::system_clock::time_point timestamp;
};

}

namespace ores::eventing::domain {

/**
 * @brief Event traits specialization for role_assigned_event.
 */
template<>
struct event_traits<ores::iam::eventing::role_assigned_event> {
    static constexpr std::string_view name = "ores.iam.role_assigned";
};

}

#endif
