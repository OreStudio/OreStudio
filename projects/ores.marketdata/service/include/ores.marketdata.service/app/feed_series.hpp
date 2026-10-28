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
#ifndef ORES_MARKETDATA_SERVICE_APP_FEED_SERIES_HPP
#define ORES_MARKETDATA_SERVICE_APP_FEED_SERIES_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.core/classification/series_classifier.hpp"
#include "ores.marketdata.service/app/tick_plan.hpp"
#include "ores.marketdata.service/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>

namespace ores::marketdata::service::app {

/**
 * @brief The series a tick creates when it lands in one for the first time.
 *
 * Identity and classification come from the datum, and the owner of the series
 * comes from the route the tick arrived under. A series already present keeps
 * its owner, which is why the caller builds one of these only on the create
 * path.
 */
ORES_MARKETDATA_SERVICE_EXPORT domain::market_series
make_feed_series(const consumer& owner,
                 const boost::uuids::uuid& id,
                 const std::string& series_uri,
                 const std::string& series_subclass,
                 const std::string& service_account);

/**
 * @brief Writes the series a tick or a fixing lands in for the first time, with the asset
 * classes its classification names, and returns it.
 */
ORES_MARKETDATA_SERVICE_EXPORT domain::market_series
create_feed_series(const ores::database::context& tenant_ctx,
                   const consumer& owner,
                   const boost::uuids::uuid& id,
                   const std::string& series_uri,
                   const core::series_classification& classification,
                   const std::string& service_account);

}

#endif
