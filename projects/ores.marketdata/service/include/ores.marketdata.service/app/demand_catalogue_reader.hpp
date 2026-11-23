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
#ifndef ORES_MARKETDATA_SERVICE_APP_DEMAND_CATALOGUE_READER_HPP
#define ORES_MARKETDATA_SERVICE_APP_DEMAND_CATALOGUE_READER_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.marketdata.service/app/demand_calculator.hpp"
#include "ores.marketdata.service/export.hpp"

namespace ores::marketdata::service::app {

/**
 * @brief Reads the catalogue a demand is computed from.
 *
 * The providers, feeds, scopes, instruments and mappings are the installation's, read in
 * the system tenant. The routes and subscriptions are every live tenant's.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT demand_catalogue
read_demand_catalogue(const ores::database::context& ctx);

}

#endif
