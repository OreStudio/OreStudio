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
#ifndef ORES_REFDATA_DOMAIN_PARTY_COUNTERPARTY_HPP
#define ORES_REFDATA_DOMAIN_PARTY_COUNTERPARTY_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::refdata::domain {

/**
 * @brief Links a party to a counterparty it can see.
 *
 * Junction table controlling which counterparties are visible to which
 * parties. Counterparty identity is shared at tenant level; this junction
 * controls party-level visibility.
 */
struct party_counterparty final {
    int version = 0;
    std::string tenant_id;
    boost::uuids::uuid party_id;
    boost::uuids::uuid counterparty_id;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
