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
#ifndef ORES_MARKETDATA_CORE_TESTS_MARKET_STATE_RUN_FIXTURE_HPP
#define ORES_MARKETDATA_CORE_TESTS_MARKET_STATE_RUN_FIXTURE_HPP

#include "ores.marketdata.core/service/market_state_reader.hpp"
#include <boost/uuid/random_generator.hpp>
#include <chrono>

namespace ores::marketdata::tests {

/**
 * @brief The run a market state was read for, as of one date, with fresh ids
 * for its party, instance and definition.
 */
inline service::market_state_run a_market_state_run(std::chrono::sys_days as_of) {
    return service::market_state_run{.party_id = boost::uuids::random_generator()(),
                                     .report_instance_id = boost::uuids::random_generator()(),
                                     .report_definition_id = boost::uuids::random_generator()(),
                                     .as_of = as_of};
}

}

#endif
