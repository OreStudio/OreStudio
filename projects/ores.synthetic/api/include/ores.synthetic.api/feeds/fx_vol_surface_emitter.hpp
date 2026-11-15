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
#ifndef ORES_SYNTHETIC_API_FEEDS_FX_VOL_SURFACE_EMITTER_HPP
#define ORES_SYNTHETIC_API_FEEDS_FX_VOL_SURFACE_EMITTER_HPP

#include "ores.analytics.quant/service/fx_vol_surface_generator.hpp"
#include "ores.synthetic.api/export.hpp"
#include <cstddef>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::feed {

/// The label an ATM point is quoted under, as the market data notation spells it.
inline constexpr std::string_view fx_vol_atm_label = "ATM";

/**
 * @brief The label of a point at @p delta, such as "25RR" for the risk reversal at 25%.
 *
 * The market data notation names a delta by its whole percent, so a delta of 0.25 is "25".
 * A risk reversal is "RR" and a butterfly is "BF".
 */
[[nodiscard]] ORES_SYNTHETIC_API_EXPORT std::string fx_vol_point_label(double delta,
                                                                       bool butterfly);

/**
 * @brief What one contributor published for one point of the surface, this step.
 *
 * A point with no value is one the contributor did not publish this step, which is a gap
 * its readers must tolerate rather than a zero. A stale point repeats the contributor's
 * last value for it.
 */
struct fx_vol_point_tick final {
    /// The expiry's label, as the feed's expiry rows name it.
    std::string expiry_id;
    /// "ATM", or the point's delta label such as "25RR" or "10BF".
    std::string label;
    /// The point's delta in (0, 0.5]; 0.5 for ATM.
    double delta = 0.5;
    /// The value the contributor published, or nothing when it published none.
    std::optional<double> value;
    /// Whether the value repeats the one the contributor published before.
    bool stale = false;
};

/// One contributor of a surface: who it is, how noisy it is, and where its noise comes from.
struct fx_vol_contributor_spec final {
    /// What a reader attributes the quotes to, such as the party's short name.
    std::string source_name;
    ores::analytics::quant::service::fx_vol_noise_profile profile;
    ores::analytics::quant::math::rng_stream stream;
};

/**
 * @brief A synthetic volatility surface and the contributors that observe it.
 *
 * One step advances the surface by the normals the latent market hands it and refreshes
 * every contributor's every point. The correlation is the market's: the emitter insists on
 * the identity for the generator's matrix, so the shocks it advances on arrive already
 * correlated with spot's and nothing here correlates them a second time.
 *
 * The points are laid out contributor by contributor, so a feed that publishes one
 * contributor's quotes reads one contiguous span: within a contributor, the expiries in the
 * order they were given, and for each expiry the ATM point then a risk reversal and a
 * butterfly for each delta in the order they were given.
 *
 * Every array is sized once, when the emitter is made, so a step allocates nothing: a run
 * may carry thousands of surfaces and every one of them steps on the loop's thread.
 */
class ORES_SYNTHETIC_API_EXPORT fx_vol_surface_emitter final {
public:
    /**
     * @brief A surface at its start, ready to step.
     *
     * Refuses what the generator refuses, a generator whose matrix is not the identity,
     * a contributor whose noise profile cannot be applied, and a delta outside (0, 0.5].
     *
     * @param start The surface to start at, which every process reverts to.
     * @param dynamics The laws of the drivers, with the identity for their matrix.
     * @param dt The year fraction one step represents.
     * @param convention The pair's quoting convention, which is one per pair and does not
     *        change from expiry to expiry. The market itself arrives with each step, because it
     *        moves when the run's curves do.
     * @param deltas The deltas, besides ATM, to quote a risk reversal and a butterfly at.
     * @param contributors The contributors that observe the surface.
     */
    [[nodiscard]] static std::expected<fx_vol_surface_emitter, std::string>
    make(ores::analytics::quant::service::ssvi_parameters start,
         ores::analytics::quant::service::fx_vol_surface_dynamics dynamics,
         double dt,
         ores::analytics::quant::math::fx_quote_convention convention,
         std::vector<double> deltas,
         std::vector<fx_vol_contributor_spec> contributors);

    /**
     * @brief Advances the surface one step and refreshes every point.
     *
     * @param drivers One standard normal for each driver the generator reads, in its order,
     *        correlated with spot's by the latent market.
     * @throws std::invalid_argument when @p drivers does not hold one normal per driver.
     */
    std::span<const fx_vol_point_tick>
    step(std::span<const double> drivers,
         std::span<const ores::analytics::quant::math::fx_market> markets);

    /// The points, oldest contributor first; each contributor's are contiguous.
    [[nodiscard]] std::span<const fx_vol_point_tick> points() const noexcept {
        return points_;
    }

    /// How many points one contributor publishes each step.
    [[nodiscard]] std::size_t points_per_contributor() const noexcept {
        return per_contributor_;
    }

    [[nodiscard]] std::size_t contributor_count() const noexcept {
        return names_.size();
    }

    /// What a reader attributes contributor @p c's points to.
    [[nodiscard]] const std::string& contributor_name(std::size_t c) const {
        return names_[c];
    }

    /// The surface the state gives now.
    [[nodiscard]] const ores::analytics::quant::service::ssvi_parameters& surface() const noexcept;

private:
    /// A surface at its start, which the maker alone builds.
    explicit fx_vol_surface_emitter(
        ores::analytics::quant::service::fx_vol_surface_generator generator);

    ores::analytics::quant::service::fx_vol_surface_generator generator_;
    /// The pair's convention, which every expiry of the surface is quoted in.
    ores::analytics::quant::math::fx_quote_convention convention_;
    /// The expiry names, in the surface's own order, so a step need not rebuild them.
    std::vector<std::string> expiry_ids_;
    /// One market per expiry, as the caller hands them in; reused so a tick does not allocate.
    std::vector<ores::analytics::quant::service::fx_vol_slice_market> slices_;
    std::vector<double> deltas_;
    std::vector<ores::analytics::quant::service::fx_vol_noise_profile> profiles_;
    std::vector<ores::analytics::quant::math::rng_stream> streams_;
    std::vector<std::string> names_;
    std::vector<fx_vol_point_tick> points_;
    /// Each point's clean value, rewritten every step before the contributors rewrite theirs.
    std::vector<double> clean_;
    /// What each contributor last published for each of its points, which a stale point repeats.
    std::vector<std::optional<double>> published_;
    std::size_t per_contributor_ = 0;
};

}

#endif
