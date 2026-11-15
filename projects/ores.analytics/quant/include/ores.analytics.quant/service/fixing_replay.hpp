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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_FIXING_REPLAY_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_FIXING_REPLAY_HPP

#include "ores.analytics.quant/domain/crm_topology.hpp"
#include "ores.analytics.quant/domain/driver_quote.hpp"
#include "ores.analytics.quant/domain/rate_status.hpp"
#include "ores.analytics.quant/domain/staleness_policy.hpp"
#include "ores.analytics.quant/export.hpp"
#include <chrono>
#include <expected>
#include <span>
#include <string>
#include <utility>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief One day of a publisher's fixings, each a rate of a currency against the
 * base that publisher quotes everything against.
 *
 * A fixing is known when its day ends, so the replay stamps every quote with that
 * day's close whatever the caller set: a history is read as of a day, not as of the
 * instant somebody happened to load it.
 */
struct fixing_day {
    std::chrono::year_month_day day;
    std::vector<domain::driver_quote> quotes;
};

/// A pair's level on one day, and what the engine made of the drivers behind it.
struct daily_level {
    std::chrono::year_month_day day;
    double rate{};
    domain::rate_status status{domain::rate_status::unavailable};
};

/// Every day one pair could be read on, oldest first.
struct pair_levels {
    std::string base_code;
    std::string quote_code;
    std::vector<daily_level> levels;
};

/**
 * @brief Steps a rate engine through a publisher's fixings and reads every pair after
 * each day, so a caller can ask for a history rather than an instant.
 *
 * The engine is the same one the live path uses: each day's quotes are applied with
 * @c rate_engine::update and every pair is read with @c rate_engine::rate at that day's
 * close. Nothing about the streaming path changes, and no triangulation is written
 * here: the levels come from the topology, exactly as a live read would.
 *
 * The days may arrive in any order and are stepped oldest first. A day a pair could not
 * be read is still a row, carrying its status, so a gap in the history is visible as a
 * gap rather than as a shorter series.
 *
 * @param topology The spanning tree the levels are read through.
 * @param days The published fixings, one entry per day.
 * @param pairs The pairs to read, each a base and a quote code.
 * @param policy How old a driver may be before its pair is no longer fresh.
 * @return One history per requested pair, or why the replay could not run.
 */
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT std::expected<std::vector<pair_levels>, std::string>
replay_fixings(const domain::crm_topology& topology,
               std::span<const fixing_day> days,
               std::span<const std::pair<std::string, std::string>> pairs,
               const domain::staleness_policy& policy);

}

#endif
