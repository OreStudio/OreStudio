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
#ifndef ORES_DQ_API_EVENTING_SYNTHETIC_FX_SPOT_CONFIG_CHANGED_EVENT_HPP
#define ORES_DQ_API_EVENTING_SYNTHETIC_FX_SPOT_CONFIG_CHANGED_EVENT_HPP

#include "ores.eventing.api/domain/event_traits.hpp"
#include <chrono>
#include <string>
#include <vector>

namespace ores::dq::eventing {

/**
 * @brief Domain event indicating that synthetic fx spot config data has changed.
 *
 * Published when any synthetic fx spot config entity is created, updated, or
 * deleted. Subscribers use the timestamp to query for changes since that point.
 */
struct synthetic_fx_spot_config_changed_event final {
    /**
     * @brief The timestamp of when the change occurred (in UTC).
     */
    std::chrono::system_clock::time_point timestamp;

    /**
     * @brief Changed synthetic fx spot config UUIDs (as strings).
     */
    std::vector<std::string> config_ids;

    /**
     * @brief The tenant that owns the changed entity.
     */
    std::string tenant_id;
};

}

namespace ores::eventing::domain {

/**
 * @brief Event traits specialization for synthetic_fx_spot_config_changed_event.
 */
template <>
struct event_traits<ores::dq::eventing::synthetic_fx_spot_config_changed_event> {
    static constexpr std::string_view name = "ores.dq.synthetic_fx_spot_config_changed";
};

}

#endif
