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
#include "ores.analytics.quant/service/fixing_replay.hpp"
#include "ores.analytics.quant/service/rate_engine.hpp"
#include <algorithm>
#include <format>
#include <stdexcept>

namespace ores::analytics::quant::service {

namespace {

/// The instant a fixing day's rates are known: the last second of that day.
std::chrono::system_clock::time_point close_of(std::chrono::year_month_day day) {
    return std::chrono::sys_days{day} + std::chrono::hours{24} - std::chrono::seconds{1};
}

}

std::expected<std::vector<pair_levels>, std::string>
replay_fixings(const domain::crm_topology& topology,
               std::span<const fixing_day> days,
               std::span<const std::pair<std::string, std::string>> pairs,
               const domain::staleness_policy& policy) {
    if (pairs.empty())
        return std::unexpected("A replay was asked for no pair.");

    // Resolving the pairs first turns a typo into a refusal that names it, rather than a
    // history that is quietly short of one series.
    for (const auto& [base, quote] : pairs) {
        if (!topology.currency_id_for(base))
            return std::unexpected(
                std::format("A replay was asked for {}/{}, and the topology does not hold {}.",
                            base,
                            quote,
                            base));
        if (!topology.currency_id_for(quote))
            return std::unexpected(
                std::format("A replay was asked for {}/{}, and the topology does not hold {}.",
                            base,
                            quote,
                            quote));
    }

    std::vector<pair_levels> out;
    out.reserve(pairs.size());
    for (const auto& [base, quote] : pairs)
        out.push_back({.base_code = base, .quote_code = quote, .levels = {}});

    if (days.empty())
        return out;

    std::vector<const fixing_day*> ordered;
    ordered.reserve(days.size());
    for (const auto& day : days)
        ordered.push_back(&day);
    std::ranges::sort(ordered, [](const fixing_day* lhs, const fixing_day* rhs) {
        return std::chrono::sys_days{lhs->day} < std::chrono::sys_days{rhs->day};
    });

    rate_engine engine(topology, policy);
    for (const auto* day : ordered) {
        const auto close = close_of(day->day);
        try {
            for (const auto& quote : day->quotes) {
                auto stamped = quote;
                stamped.observed_at = close;
                engine.update(stamped);
            }
        } catch (const std::exception& e) {
            return std::unexpected(
                std::format("A replay could not apply the fixings of {}: {}", day->day, e.what()));
        }

        for (auto& pair : out) {
            const auto rate = engine.rate(pair.base_code, pair.quote_code, close);
            pair.levels.push_back({.day = day->day, .rate = rate.rate, .status = rate.status});
        }
    }
    return out;
}

}
