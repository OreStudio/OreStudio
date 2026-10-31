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
#ifndef ORES_MARKETDATA_CORE_ORESMD_PILLAR_QUOTE_HPP
#define ORES_MARKETDATA_CORE_ORESMD_PILLAR_QUOTE_HPP

#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.core/export.hpp"
#include <chrono>
#include <expected>
#include <string>

namespace ores::marketdata::core {

/**
 * @brief One curve pillar's quote: the currency it is quoted in, the index the
 * curve stands for, the tenors that bound the pillar, and the dates those tenors
 * resolve to.
 *
 * A pillar is one quote. The series it belongs to is named on the pillar itself,
 * so this struct says nothing about which series that is; it says what the quote
 * is, so that the feed that publishes a pillar and anything that reads one build
 * one datum and not two spellings of it.
 */
struct pillar_quote final {
    std::string currency_code;

    /// The tenor of the floating index the curve stands for: 1D for an overnight
    /// index such as SOFR, 6M for a term index.
    std::string index_tenor;

    /// The tenor the pillar starts at, or SPOT for a spot-starting pillar.
    std::string start_tenor_code;

    /// The tenor the pillar matures at.
    std::string end_tenor_code;

    /// @p start_tenor_code resolved through the config's tenor convention.
    std::chrono::year_month_day start_date{};

    /// @p end_tenor_code resolved the same way.
    std::chrono::year_month_day end_date{};
};

/**
 * @brief The term of the datum a curve pillar's quote is: the coordinate that
 * tells one rung of a ladder from the next.
 *
 * A pillar names the series it reads, and its end tenor names the term within
 * that series, so a template that names both ends of its pillar reads the same
 * quote on every day it is read: a 1M pillar ends at 1M. A pillar the template
 * names no period end for -- a meeting-dated pillar, or one whose end is a date
 * -- takes the days its end date lies after its start, which is the only case
 * the dates decide the term.
 *
 * The reader resolves both dates through the config's tenor convention and
 * passes them here, so the term is the pillar's end tenor resolved through the
 * conventions and not a fresh spelling of it.
 *
 * @throws std::invalid_argument if a spot pillar's end date does not lie after
 *         its start date.
 */
ORES_MARKETDATA_CORE_EXPORT std::string pillar_term_text(const std::string& start_tenor_code,
                                                         const std::string& end_tenor_code,
                                                         std::chrono::year_month_day start_date,
                                                         std::chrono::year_month_day end_date);

/**
 * @brief The market datum a pillar's quote is, built from the market data model.
 *
 * An interest-rate swap rate quoted in @p quote 's currency, starting at the
 * pillar's start slot, on the tenor of the curve's index, and maturing at the
 * pillar's term. The start slot holds 0D for a spot-starting pillar, the
 * pillar's own period for one that starts later, and the resolved start date for
 * a meeting-dated one.
 *
 * The feed that publishes a pillar and any writer that needs a pillar's identity
 * call this, so the datum one writes is the datum the other reads. It builds the
 * datum directly and never composes a key: the identity of a market datum is its
 * oresmd URI, and a key is a boundary format.
 *
 * @return the datum, or why the schema refuses it.
 * @throws std::invalid_argument if the pillar's own tenors name no term, as
 *         pillar_term_text() does.
 */
ORES_MARKETDATA_CORE_EXPORT std::expected<datum::market_datum, std::string>
pillar_quote_datum(const pillar_quote& quote);

}

#endif
