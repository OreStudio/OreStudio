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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#ifndef ORES_MARKETDATA_SERVICE_APP_ROUTED_ASSET_CLASSES_HPP
#define ORES_MARKETDATA_SERVICE_APP_ROUTED_ASSET_CLASSES_HPP

#include "ores.marketdata.service/export.hpp"
#include <set>
#include <string>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief The asset classes a party is routed to on one feed, in a stable order.
 *
 * A route names the class of data a party takes from a publisher, and two facts
 * bound the choice. The publisher's catalogue states what it maps, by the asset
 * class of every oresmd address. The feed states what it carries, by its scopes,
 * and a feed that fetches FX alone refuses an interest rate route.
 *
 * So the answer is the mapped classes the feed serves. A feed that states no
 * scope serves whatever its publisher maps, which is how the simulated feeds are
 * installed. A feed whose scope matches none of the mapped classes is routed by
 * its own scope, because the store refuses a class the feed disowns. A publisher
 * that maps nothing is reached as FX: the route still names a class, and the
 * first tick that arrives says whether it was the right one.
 */
ORES_MARKETDATA_SERVICE_EXPORT std::vector<std::string> routed_asset_classes(
    const std::set<std::string>& mapped, const std::set<std::string>& served);

}

#endif
