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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_TRADE_POPULATION_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_TRADE_POPULATION_SERVICE_HPP

#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/service/trade_population_writer.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief The result of one run of the generator over one configuration.
 */
struct trade_population_run final {
    boost::uuids::uuid config_id;
    boost::uuids::uuid party_id;
    std::uint32_t planned = 0;
    std::uint32_t written = 0;
    /**
     * @brief Trades the run found on the books and left alone. This is what
     * makes a second run a no-op rather than a second population, and the
     * count is the convergence evidence.
     */
    std::uint32_t already_present = 0;
    std::vector<trade_population_outcome> outcomes;
};

/**
 * @brief Generates one configuration's trade population through the trading
 * operation.
 *
 * The run is a function of the configuration: it resolves the references the
 * configuration names, states the population from (seed, as-of), and writes
 * each trade whose id is not already on the books. A slot that is present is
 * left untouched rather than rewritten, because every write to a bitemporal
 * row is a new version and a re-provision must not manufacture history.
 *
 * The writer acts as the configuration's own party. The trading write path
 * takes the instrument's party from the session, so the caller establishes the
 * party before the run and the generator does not state it on a row.
 */
class ORES_SYNTHETIC_CORE_EXPORT trade_population_service final {
public:
    explicit trade_population_service(trade_population_writer& writer);

    /**
     * @brief Runs the generator for one configuration.
     *
     * @throws trade_population_error when the configuration cannot state a
     * population.
     * @throws trade_population_write_error when a reference does not resolve
     * or the store refuses a write.
     */
    [[nodiscard]] trade_population_run run(const boost::uuids::uuid& config_id);

private:
    trade_population_writer& writer_;
};

}

#endif
