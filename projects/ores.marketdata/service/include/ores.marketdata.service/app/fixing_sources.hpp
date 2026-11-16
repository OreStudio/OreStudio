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
#ifndef ORES_MARKETDATA_SERVICE_APP_FIXING_SOURCES_HPP
#define ORES_MARKETDATA_SERVICE_APP_FIXING_SOURCES_HPP

#include "ores.analytics.quant/service/topology_from_sources.hpp"
#include "ores.database/domain/context_fwd.hpp"
#include "ores.marketdata.service/export.hpp"
#include <expected>
#include <map>
#include <string>
#include <utility>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief What one party's fixing series say about the publishers it can read.
 *
 * A fixing series records the currency it prices and the currency it prices it in,
 * which is what a rate composition needs. This is that, grouped by publisher, with
 * the rank the publisher carries.
 */
struct party_fixing_sources {
    std::vector<ores::analytics::quant::service::fixing_source> sources;
    /**
     * @brief Why this party cannot be served, or empty when it can.
     *
     * A series that states no currency, or no currency it is priced in, is a fault in
     * the data rather than something to skip quietly: naming it lets an operator find
     * the row instead of wondering why a party has no rates.
     */
    std::string refusal;
};

/**
 * @brief Reads every party's fixing sources from the series and identity the store
 * holds, keyed by (tenant, party).
 *
 * A fixing series is one whose identity is an index of the FX class, because a fixing
 * is a published rate rather than something a service observed. The publishers are
 * read once for the rank resolution follows.
 *
 * @return The sources per party, or why the store could not be read at all.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT
    std::expected<std::map<std::pair<std::string, std::string>, party_fixing_sources>, std::string>
    read_fixing_sources(const ores::database::context& ctx);

}

#endif
