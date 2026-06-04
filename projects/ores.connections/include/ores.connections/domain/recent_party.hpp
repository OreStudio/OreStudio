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
#ifndef ORES_CONNECTIONS_DOMAIN_RECENT_PARTY_HPP
#define ORES_CONNECTIONS_DOMAIN_RECENT_PARTY_HPP

#include <boost/uuid/uuid.hpp>
#include <string>

namespace ores::connections::domain {

/**
 * @brief A recently-selected party, persisted locally for quick re-selection.
 */
struct recent_party final {
    boost::uuids::uuid party_id;
    std::string party_name;
    std::string last_selected_at; ///< ISO datetime, e.g. "2026-05-16 14:30:00"
};

}

#endif
