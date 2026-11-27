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

#include "ores.database/domain/context_fwd.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.core/classification/series_classifier.hpp"
#include "ores.marketdata.service/app/tick_plan.hpp"
#include "ores.marketdata.service/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>

namespace ores::marketdata::service::app {

/**
 * @brief How the value of an item was made, as a series of that item states it.
 *
 * The producer made the value once, so every series of the item states the same
 * derivation: a party's series takes the one the item already carries rather
 * than starting as an observed value.
 */
struct series_derivation final {
    std::string kind;
    boost::uuids::uuid config_id;
    int config_version = 0;

    bool operator==(const series_derivation& other) const = default;
};

/**
 * @brief The derivation of @p series_uri under @p provider_code, as a series of
 * the item already states it, or nothing when it states none.
 *
 * A computed index is seeded with its derivation under the system party before
 * any fixing lands, so a series created later for a party reads it from there.
 */
ORES_MARKETDATA_SERVICE_EXPORT std::optional<series_derivation>
derivation_of(const ores::database::context& tenant_ctx,
              const std::string& series_uri,
              const std::string& provider_code);

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
                   const std::string& service_account,
                   const std::optional<series_derivation>& derivation = std::nullopt);

}

#endif
