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
#ifndef ORES_SYNTHETIC_SERVICE_CONFIG_OPTIONS_HPP
#define ORES_SYNTHETIC_SERVICE_CONFIG_OPTIONS_HPP

#include "ores.database/domain/database_options.hpp"
#include "ores.logging/logging_options.hpp"
#include "ores.nats/config/nats_options.hpp"
#include <cstdint>
#include <iosfwd>
#include <optional>

namespace ores::synthetic::service::config {

/**
 * @brief All of the configuration options required by the service.
 */
struct options final {
    /**
     * @brief Configuration options related to logging, if any.
     */
    std::optional<ores::logging::logging_options> logging;
    /**
     * @brief Configuration related to NATS transport.
     */
    ores::nats::config::nats_options nats;
    /**
     * @brief Configuration related to database operations.
     */
    ores::database::database_options database;

    /**
     * @brief The seed every random stream of the run is keyed by.
     *
     * One seed reproduces a run: the same feeds tick at the same times.
     */
    std::uint64_t run_seed = 42;

    /**
     * @brief The simulated seconds one unit of market time spans.
     *
     * One number for the run: every feed's prices step by it, so spot and curve feeds live in
     * one simulated world. The default steps a curve at one tick a minute by a calendar day.
     */
    double market_speed = 1440.0;
};

std::ostream& operator<<(std::ostream& s, const options& v);

}

#endif
