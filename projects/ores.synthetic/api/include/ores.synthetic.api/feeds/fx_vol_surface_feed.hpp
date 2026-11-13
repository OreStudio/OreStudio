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
#ifndef ORES_SYNTHETIC_API_FEEDS_FX_VOL_SURFACE_FEED_HPP
#define ORES_SYNTHETIC_API_FEEDS_FX_VOL_SURFACE_FEED_HPP

#include "ores.analytics.quant/service/tick_scheduler.hpp"
#include "ores.marketdata.api/domain/i_feed.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.synthetic.api/domain/fx_vol_surface_feed.hpp"
#include "ores.synthetic.api/domain/fx_vol_surface_feed_expiry.hpp"
#include "ores.synthetic.api/export.hpp"
#include "ores.synthetic.api/feeds/fx_vol_surface_emitter.hpp"
#include <atomic>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::feed {

/// The kind string the fx_vol_surface producer registers under.
inline constexpr std::string_view fx_vol_surface_feed_kind = "fx_vol_surface";

/**
 * @brief The year fraction a tenor names, as a surface's expiries are counted.
 *
 * Days are counted over 365 and months over 12, which is what a quoted expiry grid
 * means before a calendar and a day count are applied. The generator task's later
 * work resolves tensors through the pair's convention; until then this is the whole
 * of the mapping and it is stated rather than assumed.
 *
 * @return The year fraction, or nothing when the tenor is not a count and a unit.
 */
[[nodiscard]] ORES_SYNTHETIC_API_EXPORT std::optional<double>
fx_vol_tenor_years(std::string_view tenor);

/**
 * @brief The market one expiry is quoted against.
 *
 * A surface's expiries do not share a market: each one is discounted to its own value date,
 * so the pair's two discount factors differ from one expiry to the next. The spot does not
 * travel here because a smile is read at a moneyness -- the strike over the forward -- so a
 * common scale of the spot cancels through the delta solve and the quoted volatilities are
 * the same at any spot level. What the quotes do depend on is the ratio of the two
 * discount factors, which is the forward over the spot.
 */
struct fx_vol_expiry_market final {
    /// The expiry row's tenor, as the feed's expiry rows spell it.
    std::string expiry_id;
    ores::analytics::quant::math::fx_market market;
};

/**
 * @brief Everything a surface feed reads, gathered by the caller.
 *
 * The caller resolves the pair's market and convention because it alone holds the
 * connections: the feed takes values, as the derived source does.
 */
struct fx_vol_surface_feed_build_input final {
    domain::fx_vol_surface_feed config;
    /// The expiry rows, in any order; the segment order is their year fraction order.
    std::vector<domain::fx_vol_surface_feed_expiry> expiries;
    /// Who observes the surface, one entry for each contributor row.
    std::vector<std::string> contributors;
    /// Their noise profiles, in the same order; empty means every one is the clean contributor.
    std::vector<ores::analytics::quant::service::fx_vol_noise_profile> contributor_profiles;
    /// The market of each expiry, one entry for each expiry row.
    std::vector<fx_vol_expiry_market> markets;
    /// The pair's quoting convention, shared by every expiry.
    ores::analytics::quant::math::fx_quote_convention convention;
    /// The deltas, besides ATM, to quote a risk reversal and a butterfly at.
    std::vector<double> deltas;
};

/**
 * @brief The emitter a row describes, at its start.
 *
 * The start surface is the level the feed states: its skew and curvature, and one ATM
 * volatility for each expiry. The laws are the feed's for the skew and the curvature and
 * each expiry's for its own segment. The correlation matrix is the identity, because the
 * latent market correlates the shocks and a surface that correlated them again would move
 * twice as far with spot as it says.
 *
 * Refuses a tenor that does not read, expiries that do not rise, ATM total variances that
 * do not rise with them, a segment with no expiry, and a contributor who is named twice.
 */
[[nodiscard]] ORES_SYNTHETIC_API_EXPORT std::expected<fx_vol_surface_emitter, std::string>
make_fx_vol_surface_emitter(const fx_vol_surface_feed_build_input& input, std::uint64_t run_seed);

/**
 * @brief A synthetic FX volatility surface, as a feed the run's loop ticks.
 *
 * Each tick advances the surface on the normals the latent market hands it, one for each
 * forward variance and one for each of the skew and the curvature, and publishes every
 * point each contributor observed under the market data notation.
 */
class ORES_SYNTHETIC_API_EXPORT fx_vol_surface_feed final
    : public ores::marketdata::domain::i_feed {
public:
    /**
     * @throws std::invalid_argument when the row cannot make an emitter, or when it names no
     * contributor.
     */
    fx_vol_surface_feed(ores::nats::service::client& nats,
                        std::string source_name,
                        std::string nats_subject,
                        const fx_vol_surface_feed_build_input& input,
                        std::uint64_t run_seed,
                        ores::analytics::quant::service::tick_scheduler& scheduler,
                        double sensitivity,
                        const ores::analytics::quant::domain::arrival_spec& arrival);

    const std::string& source_name() const override {
        return source_name_;
    }
    const std::vector<std::string>& series_uris() const override {
        return series_uris_;
    }
    const std::string& role() const override {
        return role_;
    }
    const std::string& nats_subject() const override {
        return nats_subject_;
    }
    std::string_view kind() const override {
        return fx_vol_surface_feed_kind;
    }
    void start() override;
    /**
     * @brief One tick, as the scheduler drives it.
     *
     * A surface whose drivers the run's market does not know -- one started after the market
     * was built, or a run with no market at all -- publishes nothing and says so. Stepping it
     * on zeros would move it on noise nobody chose, and throwing would fill the log with one
     * failure a tick.
     */
    std::string tick(const ores::analytics::quant::service::tick_context& context);
    void stop() override;
    std::uint64_t publish_count() const override {
        return published_.load(std::memory_order_relaxed);
    }

private:
    ores::nats::service::client& nats_;
    std::string source_name_;
    std::string nats_subject_;
    std::string role_;
    std::vector<std::string> series_uris_;
    fx_vol_surface_emitter emitter_;
    ores::analytics::quant::domain::arrival_spec arrival_;
    ores::analytics::quant::service::tick_scheduler& scheduler_;
    double ticks_per_second_ = 1.0;
    double sensitivity_ = 1.0;
    std::optional<std::size_t> handle_;
    std::atomic<std::uint64_t> published_{0};
};

}

#endif
