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
#include "ores.synthetic.api/feeds/index_fixings.hpp"
#include "ores.analytics.quant/math/rng_stream.hpp"
#include "ores.marketdata.api/datum/market_index.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include <algorithm>
#include <cctype>
#include <cmath>
#include <deque>
#include <format>
#include <string>
#include <utility>

namespace ores::synthetic::feed {

namespace {

/**
 * The day the walk's integration starts from.
 *
 * The draws are keyed by the day's distance from here, and the integration runs from
 * here too, so a history is one slice of one series whatever window a run asks for: a
 * day's value moves only when the level does.
 */
constexpr std::chrono::year walk_origin_year{1970};

std::string upper(std::string text) {
    std::ranges::transform(
        text, text.begin(), [](unsigned char c) { return static_cast<char>(std::toupper(c)); });
    return text;
}

bool is_business_day(std::chrono::weekday day) {
    return day != std::chrono::Saturday && day != std::chrono::Sunday;
}

/// The distance of @p day from the walk's origin, in days.
std::uint64_t ordinal_of(std::chrono::year_month_day day) {
    const auto origin = std::chrono::sys_days{
        std::chrono::year_month_day{walk_origin_year, std::chrono::January, std::chrono::day{1}}};
    return static_cast<std::uint64_t>((std::chrono::sys_days{day} - origin).count());
}

}

std::expected<std::string, std::string> index_fixing_uri(const std::string& currency,
                                                         const std::string& index_family,
                                                         const std::string& tenor) {
    namespace datum = ores::marketdata::datum;

    if (currency.empty())
        return std::unexpected("The index has no currency, so its fixings have no address.");
    if (index_family.empty())
        return std::unexpected("The index has no family, so its fixings have no address.");

    std::vector<datum::market_index::field_text> fields;
    fields.push_back({.name = "name", .text = upper(index_family)});
    if (!tenor.empty())
        fields.push_back({.name = "tenor", .text = tenor});

    auto index = datum::market_index::make(datum::index_family::ibor, currency, std::move(fields));
    if (!index)
        return std::unexpected("The index '" + currency + "-" + upper(index_family) +
                               (tenor.empty() ? "" : "-" + tenor) +
                               "' has no address: " + index.error());
    return datum::oresmd_uri_codec::write_index(*index);
}

std::expected<std::vector<index_fixing>, std::string>
derive_fixing_history(double level,
                      std::chrono::year_month_day as_of,
                      int days,
                      std::uint64_t run_seed,
                      const std::string& index_uri,
                      const std::set<std::chrono::year_month_day>& held,
                      fixing_walk walk) {
    using namespace ores::analytics::quant::math;

    if (!std::isfinite(level) || level < 0.0)
        return std::unexpected(std::format(
            "The level '{}' of '{}' is not a rate, so no fixing can be stated.", level, index_uri));
    if (!as_of.ok())
        return std::unexpected(
            std::format("The day '{}' is no day of a month, so no fixing can be dated.", as_of));
    if (walk.daily_spread < 0.0 || walk.reversion < 0.0)
        return std::unexpected(
            std::format("The walk of '{}' has a negative spread '{}' or reversion '{}'.",
                        index_uri,
                        walk.daily_spread,
                        walk.reversion));

    if (days <= 0)
        return std::vector<index_fixing>{};

    const auto key = stream_key{
        .run_seed = run_seed, .series = series_key(index_uri), .purpose = draw_purpose::market};

    // The walk is integrated from the origin over the business days up to the as-of, so a
    // day's deviation depends on its place in the series and not on the window this call
    // asks for. Only the last `days` deviations are kept.
    const auto origin = std::chrono::sys_days{
        std::chrono::year_month_day{walk_origin_year, std::chrono::January, std::chrono::day{1}}};
    std::deque<std::pair<std::chrono::year_month_day, double>> window;
    double deviation = 0.0;
    for (auto day = origin; day <= std::chrono::sys_days{as_of}; day += std::chrono::days{1}) {
        const std::chrono::year_month_day date{day};
        if (!is_business_day(std::chrono::weekday{day}))
            continue;
        const auto z = standard_normal_at(key, ordinal_of(date));
        deviation = deviation * (1.0 - walk.reversion) + walk.daily_spread * z;
        window.emplace_back(date, deviation);
        if (window.size() > static_cast<std::size_t>(days))
            window.pop_front();
    }

    if (window.empty())
        return std::vector<index_fixing>{};

    // The last day, the as-of's own business day, is the level the curve states, so the
    // history and the curve cannot disagree on the day the run starts.
    const auto shift = window.back().second;
    std::vector<index_fixing> out;
    out.reserve(window.size());
    for (const auto& [date, e] : window) {
        if (held.contains(date))
            continue;
        const auto value = std::max(0.0, level + (e - shift));
        out.push_back({.date = date, .value = std::format("{:.6f}", value)});
    }
    return out;
}

}
