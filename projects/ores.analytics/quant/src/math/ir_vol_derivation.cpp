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
#include "ores.analytics.quant/math/ir_vol_derivation.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <format>
#include <optional>
#include <utility>

namespace ores::analytics::quant::math {

namespace {

/**
 * The mean reversion below which the driftless limit is read instead of the model's own expression,
 * where the two divisions would lose their precision or divide by zero.
 */
constexpr double minimum_mean_reversion = 1e-8;

/// The skewness the smile's slope carries, over six, as the Gram-Charlier correction states it.
constexpr double skew_over_six = 6.0;

/// One table for both directions, so a name and a model cannot disagree.
struct model_name final {
    short_rate_model model;
    std::string_view name;
    bool closed_form;
};

constexpr std::array<model_name, 5> model_names{{
    {short_rate_model::vasicek, "vasicek", true},
    {short_rate_model::hull_white, "hull_white", true},
    {short_rate_model::cox_ingersoll_ross, "cox_ingersoll_ross", false},
    {short_rate_model::black_karasinski, "black_karasinski", false},
    {short_rate_model::two_factor_gaussian, "two_factor_gaussian", false},
}};

}

std::string_view name_of(short_rate_model model) {
    for (const auto& entry : model_names)
        if (entry.model == model)
            return entry.name;
    return {};
}

std::optional<short_rate_model> short_rate_model_named(std::string_view name) {
    for (const auto& entry : model_names)
        if (entry.name == name)
            return entry.model;
    return std::nullopt;
}

bool has_closed_form(short_rate_model model) noexcept {
    for (const auto& entry : model_names)
        if (entry.model == model)
            return entry.closed_form;
    return false;
}

std::expected<double, std::string> projection_factor(swaption_pillar pillar,
                                                     short_rate_projection model) {
    if (!(pillar.expiry_years > 0.0) || !std::isfinite(pillar.expiry_years))
        return std::unexpected("the expiry must be positive and finite");
    if (!(pillar.term_years > 0.0) || !std::isfinite(pillar.term_years))
        return std::unexpected("the term must be positive and finite");
    if (!std::isfinite(model.mean_reversion) || model.mean_reversion < 0.0)
        return std::unexpected("the mean reversion must be finite and not negative");
    if (!has_closed_form(model.model))
        return std::unexpected(std::format("the model {} is not one this projection is closed form "
                                           "for, and it is not approximated",
                                           name_of(model.model)));

    const double a = model.mean_reversion;
    const bool has_reversion = a > minimum_mean_reversion;
    const double variance = has_reversion ?
                                (1.0 - std::exp(-2.0 * a * pillar.expiry_years)) / (2.0 * a) :
                                pillar.expiry_years;
    const double average =
        has_reversion ? (1.0 - std::exp(-a * pillar.term_years)) / (a * pillar.term_years) : 1.0;
    return std::sqrt(variance) * average;
}

std::expected<ir_pillar_vol, std::string> atm_vol_of(double level,
                                                     int window,
                                                     swaption_pillar pillar,
                                                     short_rate_projection model,
                                                     return_shape horizon_shape) {
    if (!std::isfinite(level) || level < 0.0)
        return std::unexpected("the level must be finite and not negative");
    const auto factor = projection_factor(pillar, model);
    if (!factor)
        return std::unexpected(factor.error());
    const double atm = level * *factor;
    return ir_pillar_vol{.window = window,
                         .level = level,
                         .factor = *factor,
                         .atm = atm,
                         .smile = smile_of(atm, horizon_shape)};
}

normal_skew_smile smile_of(double atm_volatility, return_shape shape) noexcept {
    return {.alpha = atm_volatility, .skewness = shape.skewness};
}

double smile_at(const normal_skew_smile& smile,
                double forward,
                double strike,
                double expiry_years) noexcept {
    if (!(smile.alpha > 0.0) || !(expiry_years > 0.0) || !std::isfinite(forward) ||
        !std::isfinite(strike))
        return smile.alpha;
    const double standardised = (strike - forward) / (smile.alpha * std::sqrt(expiry_years));
    return std::max(smile.alpha * (1.0 + smile.skewness * standardised / skew_over_six), 0.0);
}

std::expected<ir_term_matrix, std::string> derive_term_matrix(std::span<const dated_level> levels,
                                                              double term_years,
                                                              std::span<const double> expiry_years,
                                                              const measurement_settings& settings,
                                                              short_rate_projection model) {
    if (!(settings.periods_per_year > 0.0) || !std::isfinite(settings.periods_per_year))
        return std::unexpected("the periods in a year must be positive and finite");
    const auto returns = returns_of(levels, volatility_transform::difference);
    if (returns.returns.empty())
        return std::unexpected("the series has no return: it holds fewer than two observations");
    const auto series = measure_series(returns.returns, settings);
    if (!series)
        return std::unexpected(series.error());

    ir_term_matrix out{.series = *series, .cells = {}};
    out.cells.reserve(expiry_years.size());
    for (const double years : expiry_years) {
        const auto measured = measure_pillar(returns.returns, years, settings);
        if (!measured) {
            out.cells.emplace_back(
                std::unexpect,
                std::format("the pillar of {} years needs {} returns; the sample holds {}",
                            years,
                            measurement_window(years, settings),
                            returns.returns.size()));
            continue;
        }
        // The smile's slope is the shape of a return over the pillar's own life, so a year's
        // skewness is the daily one over the root of the periods it holds.
        const auto horizon = over_days(series->shape, years * settings.periods_per_year);
        out.cells.emplace_back(atm_vol_of(measured->level,
                                          measured->window,
                                          {.expiry_years = years, .term_years = term_years},
                                          model,
                                          horizon));
    }
    return out;
}

std::expected<ir_vol_projection, std::string> project_ir_vols(const vol_batch& measured,
                                                              std::span<const double> term_years,
                                                              short_rate_projection model) {
    if (term_years.size() != measured.series_count())
        return std::unexpected(std::format("the batch holds {} series and {} tenor(s) were stated",
                                           measured.series_count(),
                                           term_years.size()));
    if (!has_closed_form(model.model))
        return std::unexpected(std::format("the model {} is not one this projection is closed form "
                                           "for, and it is not approximated",
                                           name_of(model.model)));

    // A pillar no batch cell could be measured at is not this projection's to answer; the batch
    // refused it and says why.
    const auto cells = measured.states.size();
    ir_vol_projection out{.factors = std::vector<double>(cells, 0.0),
                          .atms = std::vector<double>(cells, 0.0),
                          .smile_skewnesses = std::vector<double>(cells, 0.0)};
    for (std::size_t series = 0; series < measured.series_count(); ++series) {
        if (!(term_years[series] > 0.0) || !std::isfinite(term_years[series]))
            return std::unexpected(
                std::format("the term of series {} must be positive and finite", series));
        const std::size_t first = measured.offset_of(series);
        const std::size_t last = measured.offsets[series + 1];
        for (std::size_t cell = first; cell < last; ++cell) {
            if (measured.states[cell] != vol_cell_state::quoted)
                continue;
            const double years = measured.pillars[cell - first];
            const return_shape shape{.skewness = measured.skewnesses[cell],
                                     .excess_kurtosis = measured.excess_kurtoses[cell]};
            const auto vol = atm_vol_of(measured.levels[cell],
                                        measured.windows[cell],
                                        {.expiry_years = years, .term_years = term_years[series]},
                                        model,
                                        shape);
            if (!vol)
                return std::unexpected(vol.error());
            out.factors[cell] = vol->factor;
            out.atms[cell] = vol->atm;
            out.smile_skewnesses[cell] = vol->smile.skewness;
        }
    }
    return out;
}

}
