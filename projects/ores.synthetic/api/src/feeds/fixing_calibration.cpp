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
#include "ores.synthetic.api/feeds/fixing_calibration.hpp"
#include <algorithm>
#include <cmath>
#include <numeric>
#include <optional>
#include <regex>

namespace ores::synthetic::feed {

std::optional<std::pair<std::string, std::string>> pair_of_item(const std::string& item) {
    static const std::regex fx_fixing(
        R"(^oresmd://fx/([A-Z]{3})\?(?:.*&)?ccy=([A-Z]{3})(?:&.*)?$)");
    std::smatch m;
    if (!std::regex_match(item, m, fx_fixing) || item.find("type=fixing") == std::string::npos)
        return std::nullopt;
    return std::make_pair(m[1].str(), m[2].str());
}

std::expected<item_orientation, std::string>
orientation_of(const std::string& item, const std::string& base, const std::string& quote) {
    const auto pair = pair_of_item(item);
    if (!pair)
        return std::unexpected("'" + item + "' is not an FX fixing item.");
    const auto item_base = pair->first;
    const auto item_quote = pair->second;
    if (item_base == base && item_quote == quote)
        return item_orientation::direct;
    if (item_base == quote && item_quote == base)
        return item_orientation::inverted;
    return std::unexpected("'" + item + "' quotes " + item_base + "/" + item_quote + ", not " +
                           base + "/" + quote + ".");
}

std::optional<double> business_day_log_return_spread(const std::vector<double>& values) {
    std::vector<double> returns;
    for (std::size_t i = 1; i < values.size(); ++i) {
        if (values[i - 1] <= 0.0 || values[i] <= 0.0)
            return std::nullopt;
        returns.push_back(std::log(values[i] / values[i - 1]));
    }
    if (returns.size() < 2)
        return std::nullopt;
    const double mean =
        std::accumulate(returns.begin(), returns.end(), 0.0) / static_cast<double>(returns.size());
    double squares = 0.0;
    for (const double r : returns)
        squares += (r - mean) * (r - mean);
    return std::sqrt(squares / static_cast<double>(returns.size() - 1));
}

std::expected<std::vector<double>, std::string> scaled_stdevs(
    const std::vector<mixture_component>& components, double hourly_spread, int ticks_per_hour) {
    if (components.empty())
        return std::unexpected("The mixture has no components.");
    if (ticks_per_hour <= 0)
        return std::unexpected("The feed has no ticks in an hour.");
    double total_weight = 0.0;
    for (const auto& c : components)
        total_weight += c.weight;
    if (total_weight <= 0.0)
        return std::unexpected("The mixture's weights add up to nothing.");

    // The variance of one tick of the mixture is the weighted variance of its components plus
    // the spread of their means: sum w (s^2 + m^2) - (sum w m)^2. Scaling every s by k scales
    // only the first part, so k^2 = (target^2 - spread of the means) / sum w s^2.
    double scalable = 0.0;
    double mean = 0.0;
    double second = 0.0;
    for (const auto& c : components) {
        const double w = c.weight / total_weight;
        scalable += w * c.stdev * c.stdev;
        mean += w * c.mean;
        second += w * c.mean * c.mean;
    }
    const double means_spread = second - mean * mean;
    const double target = hourly_spread / std::sqrt(static_cast<double>(ticks_per_hour));
    if (scalable <= 0.0)
        return std::unexpected("The mixture has no deviation to scale.");
    const double k2 = (target * target - means_spread) / scalable;
    if (k2 <= 0.0)
        return std::unexpected("The mixture's means spread further than the measured spread.");
    const double k = std::sqrt(k2);
    std::vector<double> out;
    out.reserve(components.size());
    for (const auto& c : components)
        out.push_back(c.stdev * k);
    return out;
}

namespace {

/// The rank of @p provider in @p order, or nothing when a stated order leaves it out.
std::optional<int> provider_rank(const std::vector<std::string>& order,
                                 const std::string& provider) {
    if (order.empty())
        return 0;
    for (std::size_t i = 0; i < order.size(); ++i)
        if (order[i] == provider)
            return static_cast<int>(i);
    return std::nullopt;
}

} // namespace

std::vector<ores::synthetic::domain::fx_spot_fixing_item>
derive_fixing_items(const calibration_policy& policy,
                    const std::string& base,
                    const std::string& quote,
                    const std::vector<calibration_candidate>& candidates) {
    struct ranked final {
        int provider = 0;
        int direction = 0;
        std::string item;
        std::string provider_code;
    };
    std::vector<ranked> kept;
    for (const auto& candidate : candidates) {
        const bool direct = candidate.base_currency == base && candidate.quote_currency == quote;
        const bool inverted = candidate.base_currency == quote && candidate.quote_currency == base;
        if (!direct && !(inverted && policy.allow_inverse))
            continue;
        const auto rank = provider_rank(policy.provider_order, candidate.provider_code);
        if (!rank)
            continue;
        kept.push_back({.provider = *rank,
                        .direction = direct ? 0 : 1,
                        .item = candidate.item,
                        .provider_code = candidate.provider_code});
    }
    std::ranges::sort(kept, [](const ranked& a, const ranked& b) {
        if (a.provider != b.provider)
            return a.provider < b.provider;
        if (a.provider_code != b.provider_code)
            return a.provider_code < b.provider_code;
        if (a.direction != b.direction)
            return a.direction < b.direction;
        return a.item < b.item;
    });

    std::vector<ores::synthetic::domain::fx_spot_fixing_item> items;
    items.reserve(kept.size());
    int rank = 0;
    for (const auto& entry : kept) {
        ores::synthetic::domain::fx_spot_fixing_item item;
        item.rank = ++rank;
        item.item = entry.item;
        item.provider_code = entry.provider_code;
        item.policy = "latest_on_or_before_clock";
        item.window_days = policy.window_days;
        items.push_back(std::move(item));
    }
    return items;
}

}
