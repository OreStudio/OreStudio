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
#ifndef ORES_ACCOUNTS_EVENTING_PERMISSIONS_CHANGED_EVENT_HPP
#define ORES_ACCOUNTS_EVENTING_PERMISSIONS_CHANGED_EVENT_HPP

#include <chrono>
#include <vector>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.eventing/domain/event_traits.hpp"

namespace ores::iam::eventing {

/**
 * @brief Domain event indicating that an account's effective permissions
 * have changed.
 *
 * This event is published when the effective permissions of an account
 * change due to role assignment, revocation, or modification of role
 * permissions. It can be used to update session caches or notify clients.
 */
struct permissions_changed_event final {
    /**
     * @brief The account whose permissions changed.
     */
    boost::uuids::uuid account_id;

    /**
     * @brief The new set of effective permission codes.
     *
     * Contains the complete list of permission codes the account now has,
     * after computing all roles and their associated permissions.
     */
    std::vector<std::string> permission_codes;

    /**
     * @brief The timestamp of when the change occurred (in UTC).
     */
    std::chrono::system_clock::time_point timestamp;
};

}

namespace ores::eventing::domain {

/**
 * @brief Event traits specialization for permissions_changed_event.
 */
template<>
struct event_traits<ores::iam::eventing::permissions_changed_event> {
    static constexpr std::string_view name = "ores.iam.permissions_changed";
};

}

#endif
