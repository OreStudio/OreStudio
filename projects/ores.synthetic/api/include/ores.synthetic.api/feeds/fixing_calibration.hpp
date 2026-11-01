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
#ifndef ORES_SYNTHETIC_API_FEEDS_FIXING_CALIBRATION_HPP
#define ORES_SYNTHETIC_API_FEEDS_FIXING_CALIBRATION_HPP

#include "ores.synthetic.api/domain/fx_spot_fixing_item.hpp"
#include "ores.synthetic.api/export.hpp"
#include <cmath>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace ores::synthetic::feed {

/**
 * @brief How a fixing item quotes a feed's pair.
 *
 * An item that publishes the pair as the feed quotes it is read as it is; one that
 * publishes it the other way round is read as one over its value. An item of any other
 * pair cannot start the feed.
 */
enum class item_orientation { direct, inverted };

/**
 * @brief The base and quote currencies an FX fixing's oresmd URI names, or nothing when it is
 * not an FX fixing.
 */
ORES_SYNTHETIC_API_EXPORT std::optional<std::pair<std::string, std::string>>
pair_of_item(const std::string& item);

/**
 * @brief The orientation of @p item, an FX fixing's oresmd URI, against @p base / @p quote.
 */
ORES_SYNTHETIC_API_EXPORT std::expected<item_orientation, std::string>
orientation_of(const std::string& item, const std::string& base, const std::string& quote);

/**
 * @brief The sample standard deviation of the log returns of @p values, one per business day.
 *
 * Inverting a series negates its log returns and leaves their spread, so the spread is the
 * same for an item read either way round. Two returns, three values, are the fewest that
 * have a spread.
 */
ORES_SYNTHETIC_API_EXPORT std::optional<double>
business_day_log_return_spread(const std::vector<double>& values);

/**
 * @brief What a window's business-day log returns did.
 *
 * Only the spread changes a calibration today; the rest are recorded for a later policy.
 */
struct window_measures final {
    double daily_spread = 0.0;
    double annual_drift = 0.0;
    double min_return = 0.0;
    double max_return = 0.0;
    int jumps = 0;
};

/**
 * @brief The measures of @p values, a window of fixings oldest first, in the feed's
 * orientation.
 *
 * An inverted item's returns are the negation of its own, so the drift changes sign and the
 * range turns round; the spread and the jumps are unchanged. A jump is a return more than three
 * spreads from zero. The window needs the same three values the spread needs.
 */
ORES_SYNTHETIC_API_EXPORT std::optional<window_measures>
measure_window(const std::vector<double>& values, bool inverted);

/**
 * @brief The distance of @p other from @p level, in basis points of @p level.
 */
[[nodiscard]] inline double basis_points_from(double level, double other) {
    return std::abs(other - level) / level * 10'000.0;
}

/**
 * @brief A business-day spread in the hourly terms of the feeds.
 *
 * The feeds count a year as every hour of it, 365 times 24, which is how their mixtures are
 * seeded from an annual volatility; a year holds 252 business days. So an hour's spread is the
 * business day's times the root of 252 over 8,760.
 */
[[nodiscard]] inline double hourly_spread_of(double business_day_spread) {
    constexpr double business_days = 252.0;
    constexpr double hours = 365.0 * 24.0;
    return business_day_spread * std::sqrt(business_days / hours);
}

/**
 * @brief The class rule that derives a feed's fixing items.
 *
 * A policy states the window, whether an item may be read the other way round, and the
 * provider order. An empty provider order means every provider, ordered by its code, which
 * is the automatic default when no policy row exists.
 */
struct calibration_policy final {
    int window_days = 90;
    bool allow_inverse = true;
    std::string fallback_code = "keep_fixed";
    /// Provider codes, lowest first. Empty means every provider, ordered by code.
    std::vector<std::string> provider_order;
};

/**
 * @brief One fixing series a feed's pair may be read from.
 */
struct calibration_candidate final {
    std::string item;
    std::string provider_code;
    std::string base_currency;
    std::string quote_currency;
};

/**
 * @brief Derive a feed's fixing items from a policy and the covering series.
 *
 * Keeps the series that quote @p base / @p quote directly, and, when the policy allows it,
 * those that quote it the other way round. Orders them by the policy's provider order, then
 * direct before inverted, then item. A provider that is not in a stated order is left out;
 * an empty order keeps every provider. Every item reads the latest fixing on or before the
 * clock and measures the policy's window.
 */
ORES_SYNTHETIC_API_EXPORT std::vector<ores::synthetic::domain::fx_spot_fixing_item>
derive_fixing_items(const calibration_policy& policy,
                    const std::string& base,
                    const std::string& quote,
                    const std::vector<calibration_candidate>& candidates);

/**
 * @brief One component of a feed's mixture, per tick.
 */
struct mixture_component final {
    double mean = 0.0;
    double stdev = 0.0;
    double weight = 0.0;
};

/**
 * @brief The deviations of @p components scaled so the mixture's spread over an hour of
 * @p ticks_per_hour ticks is @p hourly_spread.
 *
 * One factor scales every deviation, so the shape of the mixture, its tails and its jumps,
 * is kept and only its size changes. The means are kept, so the drift is kept. A mixture
 * whose means alone already spread further than the target cannot be scaled to it, and is
 * an error, as is a mixture with no spread to scale.
 */
ORES_SYNTHETIC_API_EXPORT std::expected<std::vector<double>, std::string> scaled_stdevs(
    const std::vector<mixture_component>& components, double hourly_spread, int ticks_per_hour);

}

#endif
