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
#ifndef ORES_QT_EVENTS_CONNECTION_EVENTS_HPP
#define ORES_QT_EVENTS_CONNECTION_EVENTS_HPP

#include <chrono>
#include <string>
#include "ores.eventing/domain/event_traits.hpp"

namespace ores::qt::events {

/**
 * @brief Event published when successfully connected to the server.
 */
struct connected_event final {
    std::chrono::system_clock::time_point timestamp;
    std::string host;
    std::uint16_t port;
};

/**
 * @brief Event published when disconnected from the server.
 */
struct disconnected_event final {
    std::chrono::system_clock::time_point timestamp;
};

/**
 * @brief Event published when attempting to reconnect to the server.
 */
struct reconnecting_event final {
    std::chrono::system_clock::time_point timestamp;
};

/**
 * @brief Event published when successfully reconnected to the server.
 */
struct reconnected_event final {
    std::chrono::system_clock::time_point timestamp;
};

}

namespace ores::eventing::domain {

template<>
struct event_traits<ores::qt::events::connected_event> {
    static constexpr std::string_view name = "ores.qt.connected";
};

template<>
struct event_traits<ores::qt::events::disconnected_event> {
    static constexpr std::string_view name = "ores.qt.disconnected";
};

template<>
struct event_traits<ores::qt::events::reconnecting_event> {
    static constexpr std::string_view name = "ores.qt.reconnecting";
};

template<>
struct event_traits<ores::qt::events::reconnected_event> {
    static constexpr std::string_view name = "ores.qt.reconnected";
};

}

#endif
