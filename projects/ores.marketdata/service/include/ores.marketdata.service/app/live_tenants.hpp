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
#ifndef ORES_MARKETDATA_SERVICE_APP_LIVE_TENANTS_HPP
#define ORES_MARKETDATA_SERVICE_APP_LIVE_TENANTS_HPP

#include <boost/uuid/uuid.hpp>
#include <set>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief Leaves out the rows of the tenants that are not in @p live.
 *
 * A read across tenants still returns a terminated tenant's rows. Its demand would start feeds
 * nobody reads, its ticks would be stored for nobody, and every usage write for it fails.
 */
template <typename Row>
void keep_live_tenants(std::vector<Row>& rows, const std::set<boost::uuids::uuid>& live) {
    std::erase_if(rows, [&live](const Row& r) { return !live.contains(r.tenant_id.to_uuid()); });
}

}

#endif
