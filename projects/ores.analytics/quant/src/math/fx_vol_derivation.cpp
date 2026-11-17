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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.analytics.quant/math/fx_vol_derivation.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <format>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::quant::math {

namespace {

/// The longest expiry quoted: past it no history held covers the window.
constexpr double maximum_years = 50.0;

}

int fx_derivation_window(double years, const fx_derivation_settings& settings) {
    // A level is a sample variance, which needs two returns whatever the settings say.
    return std::max({2,
                     settings.minimum_window,
                     static_cast<int>(std::lround(years * settings.trading_days_per_year))});
}

std::expected<fx_derivation, std::string>
derive_fx_quotes(std::span<const double> daily_log_returns,
                 std::span<const fx_expiry> expiries,
                 const fx_derivation_settings& settings) {
    if (!(settings.trading_days_per_year > 0.0) || !std::isfinite(settings.trading_days_per_year))
        return std::unexpected("the trading days in a year must be positive and finite");
    const auto moments = moments_of(daily_log_returns);
    if (!moments)
        return std::unexpected(moments.error());
    fx_derivation out{
        .moments = *moments, .shape = shrink(*moments, settings.prior), .expiries = {}};
    out.expiries.reserve(expiries.size());
    for (const auto& expiry : expiries) {
        if (!(expiry.years > 0.0) || !(expiry.years <= maximum_years)) {
            out.expiries.emplace_back(std::unexpect,
                                      expiry.label + ": the expiry must be positive and at most " +
                                          std::to_string(static_cast<int>(maximum_years)) +
                                          " years");
            continue;
        }
        const double days = expiry.years * settings.trading_days_per_year;
        const int window = fx_derivation_window(expiry.years, settings);
        const auto level =
            realised_volatility(daily_log_returns, window, settings.trading_days_per_year);
        if (!level) {
            out.expiries.emplace_back(std::unexpect, expiry.label + ": " + level.error());
            continue;
        }
        const auto shape = hold_in_gram_charlier_region(over_days(out.shape, days));
        const gram_charlier_smile smile{.sigma = *level * settings.level_multiplier,
                                        .skewness = shape.skewness,
                                        .excess_kurtosis = shape.excess_kurtosis,
                                        .years = expiry.years};
        const auto quotes = quote_smile(smile, expiry.market, expiry.convention);
        if (!quotes) {
            out.expiries.emplace_back(std::unexpect, expiry.label + ": " + quotes.error());
            continue;
        }
        out.expiries.emplace_back(derived_fx_expiry{
            .label = expiry.label, .quotes = *quotes, .smile = smile, .window = window});
    }
    return out;
}

std::expected<void, std::string> derive_grid(std::span<const double> returns,
                                             std::span<const fx_expiry> expiries,
                                             const fx_derivation_settings& settings,
                                             std::span<fx_derived_cell> out) {
    if (!(settings.trading_days_per_year > 0.0) || !std::isfinite(settings.trading_days_per_year))
        return std::unexpected("the trading days in a year must be positive and finite");
    if (out.size() != returns.size() * expiries.size())
        return std::unexpected(
            std::format("the grid holds {} cell(s) for {} day(s) and {} expiry(ies)",
                        out.size(),
                        returns.size(),
                        expiries.size()));
    std::ranges::fill(out, fx_derived_cell{});

    // What each expiry asks of a day: how many returns its level is read over, and whether it
    // can be quoted at all.
    struct window final {
        std::size_t returns;
        double days;
        bool usable;
    };
    std::vector<window> windows;
    windows.reserve(expiries.size());
    for (const auto& expiry : expiries) {
        const auto days = expiry.years * settings.trading_days_per_year;
        const auto count = fx_derivation_window(expiry.years, settings);
        windows.push_back({.returns = static_cast<std::size_t>(count),
                           .days = days,
                           .usable = expiry.years > 0.0 && expiry.years <= maximum_years});
    }

    // Prefix sums of the returns and of their second, third and fourth powers: one pass, and
    // then any range's moments are a subtraction.
    const auto n = returns.size();
    std::vector<std::array<double, 4>> prefix(n + 1, {0.0, 0.0, 0.0, 0.0});
    for (std::size_t i = 0; i < n; ++i) {
        const double x = returns[i];
        const double x2 = x * x;
        prefix[i + 1] = {
            prefix[i][0] + x, prefix[i][1] + x2, prefix[i][2] + x2 * x, prefix[i][3] + x2 * x2};
    }

    // The moments of the returns in [from, to), or nothing where they carry no spread -- the
    // same refusal moments_of makes.
    const auto moments_between = [&](std::size_t from,
                                     std::size_t to) -> std::optional<return_moments> {
        const auto count = static_cast<double>(to - from);
        if (count < 4.0)
            return std::nullopt;
        const double s1 = prefix[to][0] - prefix[from][0];
        const double s2 = prefix[to][1] - prefix[from][1];
        const double s3 = prefix[to][2] - prefix[from][2];
        const double s4 = prefix[to][3] - prefix[from][3];
        const double mean = s1 / count;
        const double m2 = s2 / count - mean * mean;
        if (!std::isfinite(m2) || !(m2 > 1e-30 + 1e-24 * mean * mean))
            return std::nullopt;
        const double m3 = s3 / count - 3.0 * mean * s2 / count + 2.0 * mean * mean * mean;
        const double m4 = s4 / count - 4.0 * mean * s3 / count + 6.0 * mean * mean * s2 / count -
                          3.0 * mean * mean * mean * mean;
        return return_moments{.count = static_cast<int>(count),
                              .mean = mean,
                              .stdev = std::sqrt(m2 * count / (count - 1.0)),
                              .skewness = m3 / (m2 * std::sqrt(m2)),
                              .excess_kurtosis = m4 / (m2 * m2) - 3.0};
    };

    for (std::size_t count = 1; count <= n; ++count) {
        const auto sample = moments_between(0, count);
        if (!sample)
            continue;
        const auto shape = shrink(*sample, settings.prior);
        const double root = std::sqrt(settings.trading_days_per_year);
        for (std::size_t e = 0; e < expiries.size(); ++e) {
            if (!windows[e].usable || windows[e].returns > count)
                continue;
            const auto level = moments_between(count - windows[e].returns, count);
            if (!level)
                continue;
            const auto held = hold_in_gram_charlier_region(over_days(shape, windows[e].days));
            const gram_charlier_smile smile{.sigma =
                                                level->stdev * root * settings.level_multiplier,
                                            .skewness = held.skewness,
                                            .excess_kurtosis = held.excess_kurtosis,
                                            .years = expiries[e].years};
            const auto quotes = quote_smile(smile, expiries[e].market, expiries[e].convention);
            if (!quotes)
                continue;
            out[(count - 1) * expiries.size() + e] = {.quotes = *quotes, .quoted = true};
        }
    }
    return {};
}

}
