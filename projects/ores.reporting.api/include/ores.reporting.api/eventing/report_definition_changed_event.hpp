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
#ifndef ORES_REPORTING_EVENTING_REPORT_DEFINITION_CHANGED_EVENT_HPP
#define ORES_REPORTING_EVENTING_REPORT_DEFINITION_CHANGED_EVENT_HPP

#include <chrono>
#include <vector>
#include <string>
#include "ores.eventing/domain/event_traits.hpp"

namespace ores::reporting::eventing {

/**
 * @brief Domain event indicating that report definition data has changed.
 *
 * This event is published when any report definition entity is created,
 * updated, or deleted in the database. Subscribers can use the timestamp
 * to query for changes since that point.
 */
struct report_definition_changed_event final {
    /**
     * @brief The timestamp of when the change occurred (in UTC).
     */
    std::chrono::system_clock::time_point timestamp;

    /**
     * @brief UUIDs of report definitions that changed.
     */
    std::vector<std::string> ids;

    /**
     * @brief The tenant that owns the changed entity.
     */
    std::string tenant_id;
};

}

namespace ores::eventing::domain {

template<>
struct event_traits<ores::reporting::eventing::report_definition_changed_event> {
    static constexpr std::string_view name = "ores.reporting.report_definition_changed";
};

}

#endif
