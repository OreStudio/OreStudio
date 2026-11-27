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
#ifndef ORES_REPORTING_CORE_SERVICE_MARKET_READINESS_HPP
#define ORES_REPORTING_CORE_SERVICE_MARKET_READINESS_HPP

#include "ores.reporting.core/export.hpp"
#include <cstddef>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief The market's answer for one address, as the market data read states it.
 *
 * The cause travels as the text the market data component states, which is
 * =no_series= when nothing holds the address and =no_point= when a series holds
 * it but carries no point at or before the as-of. This component records the
 * cause rather than deciding it: the vocabulary belongs to the read that can
 * establish it.
 */
struct address_answer {
    std::string oresmd_uri;
    /// The series that answers the address, absent when nothing holds it.
    std::optional<std::string> series_id;
    /// Why the market does not answer it, empty when it does.
    std::string cause;
};

/**
 * @brief One trade and the addresses its instruments name.
 */
struct trade_requirement {
    std::string trade_id;
    std::vector<std::string> oresmd_uris;
};

/**
 * @brief What the run records about one address.
 */
struct address_readiness {
    std::string oresmd_uri;
    std::optional<std::string> series_id;
    std::string gap_cause;
};

/**
 * @brief What the run records about one trade.
 */
struct trade_readiness {
    std::string trade_id;
    bool complete = false;
    std::size_t gap_count = 0;
};

/**
 * @brief The run's readiness, decided from what each trade needs and what the
 * market answered.
 */
struct market_readiness {
    /// One entry per distinct address the run needs, in ascending address order.
    std::vector<address_readiness> addresses;
    /// One entry per trade the scope holds, in the order the trades were named.
    std::vector<trade_readiness> trades;
    /// How many trades the scope holds.
    std::size_t booked = 0;
    /// How many the market answered in full, which are the ones to submit.
    std::size_t complete = 0;
    /// How many the run leaves out.
    std::size_t excluded = 0;
    /// The trades the run may submit, in the order they were named.
    std::vector<std::string> survivors;

    /// Whether the run has a valuation to produce at all.
    [[nodiscard]] bool has_a_valuation() const {
        return complete > 0;
    }
};

/**
 * @brief Decides which of a run's trades its market can answer.
 *
 * A trade is complete when every address its instruments name is answered, and
 * a trade that names no address is complete because it needs nothing. An
 * incomplete trade is left out of the run rather than failed by the engine, so
 * the counts partition the population: booked is complete plus excluded.
 *
 * @param trade_ids Every trade the run's scope holds. A trade named twice is
 *                  counted once.
 * @param requirements What each of those trades needs, one entry per trade.
 * @param answers The market's answer for every address the trades name, one
 *                entry per distinct address.
 *
 * @throws std::invalid_argument when a trade the scope holds has no entry, or
 *         when an address a trade names is not answered by @p answers. Both are
 *         refusals of the caller's message, not a state of the market: treating
 *         an unstated answer as either complete or incomplete would send a
 *         trade the run cannot value to the engine, or frighten a reader with a
 *         gap nobody measured.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT market_readiness
decide_market_readiness(const std::vector<std::string>& trade_ids,
                        const std::vector<trade_requirement>& requirements,
                        const std::vector<address_answer>& answers);

}

#endif
