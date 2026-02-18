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
#ifndef ORES_REFDATA_EVENTING_PARTY_IDENTIFIER_CHANGED_EVENT_HPP
#define ORES_REFDATA_EVENTING_PARTY_IDENTIFIER_CHANGED_EVENT_HPP

#include <chrono>
#include <vector>
#include <string>
#include "ores.eventing/domain/event_traits.hpp"

namespace ores::refdata::eventing {

/**
 * @brief Domain event indicating that party identifier data has changed.
 */
struct party_identifier_changed_event final {
    std::chrono::system_clock::time_point timestamp;
    std::vector<std::string> ids;
    std::string tenant_id;
};

}

namespace ores::eventing::domain {

template<>
struct event_traits<ores::refdata::eventing::party_identifier_changed_event> {
    static constexpr std::string_view name = "ores.refdata.party_identifier_changed";
};

}

#endif
