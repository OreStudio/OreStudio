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
#include "ores.synthetic.api/feeds/fx_vol_surface_emitter.hpp"
#include <cmath>
#include <format>
#include <stdexcept>
#include <utility>

namespace ores::synthetic::feed {

using ores::analytics::quant::service::fx_vol_noise_profile_error;
using ores::analytics::quant::service::fx_vol_slice_market;
using ores::analytics::quant::service::fx_vol_surface_generator;
using ores::analytics::quant::service::fx_vol_surface_quotes;
using ores::analytics::quant::service::observe_fx_vol_point;
using ores::analytics::quant::service::ssvi_parameters;

namespace {

constexpr double atm_delta = 0.5;

constexpr double identity_tolerance = 1e-12;

}

std::string fx_vol_point_label(double delta, bool butterfly) {
    return std::format(
        "{}{}", static_cast<int>(std::lround(delta * 100.0)), butterfly ? "BF" : "RR");
}

std::expected<fx_vol_surface_emitter, std::string> fx_vol_surface_emitter::make(
    ssvi_parameters start,
    ores::analytics::quant::service::fx_vol_surface_dynamics dynamics,
    double dt,
    std::vector<ores::analytics::quant::math::fx_quote_convention> conventions,
    std::vector<double> deltas,
    std::vector<fx_vol_contributor_spec> contributors) {
    if (conventions.size() != start.expiries.size())
        return std::unexpected(
            std::format("The surface has {} expiry(ies) and {} quote convention(s).",
                        start.expiries.size(),
                        conventions.size()));
    for (const double delta : deltas)
        if (!(delta > 0.0 && delta <= atm_delta))
            return std::unexpected(std::format("The delta {} is not inside (0, 0.5].", delta));
    if (contributors.empty())
        return std::unexpected(std::string("A surface needs at least one contributor."));
    for (const auto& c : contributors)
        if (const auto why = fx_vol_noise_profile_error(c.profile); why)
            return std::unexpected(
                std::format("Contributor '{}' cannot be applied: {}.", c.source_name, *why));

    const auto& correlation = dynamics.correlation;
    if (correlation.rows() != correlation.cols() || correlation.rows() == 0 ||
        !correlation.isIdentity(identity_tolerance))
        return std::unexpected(std::string(
            "A surface's shocks arrive correlated from the latent market, so its own matrix must "
            "be the identity. Correlating them here as well would apply the correlation twice."));

    auto generator = fx_vol_surface_generator::make(start, std::move(dynamics), dt);
    if (!generator)
        return std::unexpected(generator.error());

    fx_vol_surface_emitter emitter(std::move(*generator));
    emitter.conventions_ = std::move(conventions);
    emitter.slices_.reserve(start.expiries.size());
    emitter.expiry_ids_.reserve(start.expiries.size());
    for (const auto& expiry : start.expiries)
        emitter.expiry_ids_.push_back(expiry.expiry_id);
    emitter.deltas_ = std::move(deltas);
    emitter.per_contributor_ = start.expiries.size() * (1 + 2 * emitter.deltas_.size());
    emitter.names_.reserve(contributors.size());
    emitter.profiles_.reserve(contributors.size());
    emitter.streams_.reserve(contributors.size());
    for (auto& c : contributors) {
        emitter.names_.push_back(std::move(c.source_name));
        emitter.profiles_.push_back(c.profile);
        emitter.streams_.push_back(std::move(c.stream));
    }

    // The layout is fixed once, so a step writes values and nothing else.
    const std::size_t total = emitter.names_.size() * emitter.per_contributor_;
    emitter.points_.reserve(total);
    for (const auto& name : emitter.names_) {
        static_cast<void>(name);
        for (const auto& expiry : start.expiries) {
            emitter.points_.push_back(fx_vol_point_tick{.expiry_id = expiry.expiry_id,
                                                        .label = std::string(fx_vol_atm_label)});
            for (const double delta : emitter.deltas_) {
                for (const bool butterfly : {false, true})
                    emitter.points_.push_back(
                        fx_vol_point_tick{.expiry_id = expiry.expiry_id,
                                          .label = fx_vol_point_label(delta, butterfly),
                                          .delta = delta});
            }
        }
    }
    emitter.clean_.assign(emitter.per_contributor_, 0.0);
    emitter.published_.assign(total, std::nullopt);
    return emitter;
}

std::span<const fx_vol_point_tick>
fx_vol_surface_emitter::step(std::span<const double> drivers,
                             std::span<const ores::analytics::quant::math::fx_market> markets) {
    if (drivers.size() != generator_.factor_count())
        throw std::invalid_argument(std::format(
            "A step reads {} driver(s), not {}.", generator_.factor_count(), drivers.size()));

    // Nothing is left for spot's own shock to move: the market has already correlated
    // the drivers this step advances on.
    const auto& surface = generator_.step(0.0, drivers);
    // The market of each expiry, as the caller hands it in: the feed resolved it from the
    // run's values this tick, so a curve's move reaches the strikes without a restart.
    if (markets.size() != conventions_.size())
        throw std::invalid_argument(
            std::format("A step reads {} market(s), not {}.", conventions_.size(), markets.size()));
    slices_.clear();
    for (std::size_t i = 0; i < markets.size(); ++i)
        slices_.push_back(
            {.expiry_id = expiry_ids_[i], .market = markets[i], .convention = conventions_[i]});
    const auto quotes = fx_vol_surface_quotes(surface, slices_, deltas_);
    if (!quotes)
        throw std::invalid_argument(quotes.error());

    std::size_t clean_index = 0;
    for (const auto& slice : *quotes) {
        clean_[clean_index++] = slice.atm;
        for (std::size_t k = 0; k < deltas_.size(); ++k) {
            clean_[clean_index++] = slice.risk_reversals[k];
            clean_[clean_index++] = slice.butterflies[k];
        }
    }

    // The points and the clean values share one order, expiry by expiry, so a point's
    // place in that order names both its clean value and its expiry.
    const std::size_t per_expiry = 1 + 2 * deltas_.size();
    for (std::size_t c = 0; c < names_.size(); ++c) {
        const std::size_t base = c * per_contributor_;
        for (std::size_t local = 0; local < per_contributor_; ++local) {
            const std::size_t index = base + local;
            auto& point = points_[index];
            const double years = surface.expiries[local / per_expiry].years;
            const auto observed = observe_fx_vol_point(
                clean_[local], point.delta, years, published_[index], profiles_[c], streams_[c]);
            point.value = observed.value;
            point.stale = observed.stale;
            if (observed.value)
                published_[index] = observed.value;
        }
    }
    return points_;
}

fx_vol_surface_emitter::fx_vol_surface_emitter(fx_vol_surface_generator generator)
    : generator_(std::move(generator)) {}

const ssvi_parameters& fx_vol_surface_emitter::surface() const noexcept {
    return generator_.surface();
}

}
