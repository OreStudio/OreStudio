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
 * qualifier therefore carries the currency, the start slot and the tenor of the
 * index the curve stands for, and the point is where the pillar ends.
 */
struct pillar_quote_key final {
    std::string series_type;
    std::string metric;
    std::string qualifier;
    std::string point;
};

/**
 * @brief Builds the key above from the currency the curve is quoted in, the tenor
 * of its index and the pillar's own dates.
 *
 * ORE reads a swap's start and end as two periods or two dates. A pillar the
 * template names both ends of is two periods: a pillar that starts at spot -- which
 * the config spells by setting its start tenor code to SPOT -- holds 0D in the start
 * slot, and one that starts at a period holds that period, so a 3M to 6M pillar is
 * =IR_SWAP/RATE/USD/3M/1D/6M=. The index tenor is =1D= for an overnight index such as
 * SOFR, which is what every dated OIS quote in the corpus carries, and a period such
 * as =6M= for a term index. It is the =tenor= identity field of the series, so curves
 * of two index tenors are two series.
 *
 * @p point_tenor_code is the tenor the pillar matures at, named the way the curve
 * template names it, and it decides the point. A pillar whose template names one
 * takes that tenor: a 1M pillar ends at =1M=, so its quote is
 * =IR_SWAP/RATE/GBP/0D/1D/1M= and not =IR_SWAP/RATE/GBP/0D/1D/31D=. A curve
 * configuration names its pillars by those same tenors, so the quote a reader asks
 * for is the quote the writer published, and the same quote on every day it is read.
 *
 * A pillar the template does not name a period end for is two dates instead, and its
 * start slot holds its start date: this is the meeting-dated pillar, whose start is
 * an event such as =1F= and whose end is whatever the meeting leaves. A spot pillar
 * whose template names no end takes the days its end date lies after @p start_date,
 * 50D, which is the one case a spot pillar is not a period to a period.
 *
 * Both the feed that publishes the pillar and the bootstrap that reads it call this,
 * so the series one writes is the series the other looks for.
 */
ORES_MARKETDATA_CORE_EXPORT pillar_quote_key
make_pillar_quote_key(const std::string& ccy,
                      const std::string& index_tenor,
                      const std::string& start_tenor_code,
                      std::chrono::year_month_day start_date,
                      std::chrono::year_month_day end_date,
                      const std::string& point_tenor_code = {});

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
