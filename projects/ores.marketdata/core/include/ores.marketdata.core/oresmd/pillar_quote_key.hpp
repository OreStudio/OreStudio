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
#ifndef ORES_MARKETDATA_CORE_ORESMD_PILLAR_QUOTE_KEY_HPP
#define ORES_MARKETDATA_CORE_ORESMD_PILLAR_QUOTE_KEY_HPP

#include "ores.marketdata.core/export.hpp"
#include <chrono>
#include <string>

namespace ores::marketdata::core {

/**
 * @brief The four parts of one curve pillar's ORE quote key: the datum's series
 * type, metric, qualifier and point.
 *
 * A pillar of a short-end curve is the meeting-dated OIS quote that starts it, and
 * the corpus spells that instrument as IR_SWAP/RATE/<CCY>/<start>/1D/<end>. The
 * qualifier therefore carries the currency, the start slot and the overnight index
 * tenor, and the point is where the pillar ends.
 */
struct pillar_quote_key final {
    std::string series_type;
    std::string metric;
    std::string qualifier;
    std::string point;
};

/**
 * @brief Builds the key above from the currency the curve is quoted in and the
 * pillar's own dates.
 *
 * ORE reads a swap's start and end as two periods or two dates. The pillar that
 * starts at spot -- which the config spells by setting its start tenor code to
 * SPOT -- holds 0D in the start slot and, as its point, the number of days its
 * end date lies after @p start_date, such as 50D. Every other pillar holds its
 * start date and, as its point, its end date. Every dated OIS quote in the corpus
 * carries 1D as its index tenor.
 *
 * Both the feed that publishes the pillar and the bootstrap that reads it call this,
 * so the series one writes is the series the other looks for.
 */
ORES_MARKETDATA_CORE_EXPORT pillar_quote_key
make_pillar_quote_key(const std::string& ccy,
                      const std::string& start_tenor_code,
                      std::chrono::year_month_day start_date,
                      std::chrono::year_month_day end_date);

/**
 * @brief The series identity the key above projects to.
 *
 * One pillar is one series, so the identity drops the key's point: the pillar's end
 * date is the observation's coordinate, not part of what the series is. This is the
 * same projection the ingest loop applies to a live tick, which is what makes a
 * published pillar and a read pillar one identity rather than two spellings of it.
 */
ORES_MARKETDATA_CORE_EXPORT std::string pillar_series_uri(const pillar_quote_key& key);

/**
 * @brief The datum identity the key above projects to: the series URI with the
 * pillar's coordinate left in.
 *
 * The observations table stores this, so a reader that holds the pillar's key can
 * find the row by the same string the row carries rather than by decomposing the
 * key into qualifier and point and rebuilding both.
 */
ORES_MARKETDATA_CORE_EXPORT std::string pillar_datum_uri(const pillar_quote_key& key);

}

#endif
