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
#ifndef ORES_EVENTING_DOMAIN_EVENT_CHANNEL_INFO_HPP
#define ORES_EVENTING_DOMAIN_EVENT_CHANNEL_INFO_HPP

#include <string>

namespace ores::eventing::domain {

/**
 * @brief Information about an available event channel.
 *
 * Describes an event type that clients can subscribe to for notifications.
 */
struct event_channel_info final {
    /**
     * @brief The fully qualified event channel name.
     *
     * Uses the pattern: ores.<module>.<event_name>
     * Example: "ores.risk.currency_changed"
     */
    std::string name;

    /**
     * @brief Human-readable description of the event channel.
     *
     * Example: "Currency data modified"
     */
    std::string description;
};

}

#endif
