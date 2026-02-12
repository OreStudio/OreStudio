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
#ifndef ORES_REFDATA_EVENTING_BUSINESS_CENTRE_CHANGED_EVENT_HPP
#define ORES_REFDATA_EVENTING_BUSINESS_CENTRE_CHANGED_EVENT_HPP

#include <chrono>
#include <vector>
#include <string>
#include "ores.eventing/domain/event_traits.hpp"

namespace ores::refdata::eventing {

/**
 * @brief Domain event indicating that business centre data has changed.
 *
 * This event is published when any business centre entity is created, updated,
 * or deleted in the database.
 */
struct business_centre_changed_event final {
    /**
     * @brief The timestamp of when the change occurred (in UTC).
     */
    std::chrono::system_clock::time_point timestamp;

    /**
     * @brief Codes of business centres that changed.
     */
    std::vector<std::string> codes;

    /**
     * @brief The tenant that owns the changed entity.
     */
    std::string tenant_id;
};

}

namespace ores::eventing::domain {

/**
 * @brief Event traits specialization for business_centre_changed_event.
 */
template<>
struct event_traits<ores::refdata::eventing::business_centre_changed_event> {
    static constexpr std::string_view name = "ores.refdata.business_centre_changed";
};

}

#endif
