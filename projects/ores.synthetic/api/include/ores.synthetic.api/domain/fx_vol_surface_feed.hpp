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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SYNTHETIC_API_DOMAIN_FX_VOL_SURFACE_FEED_HPP
#define ORES_SYNTHETIC_API_DOMAIN_FX_VOL_SURFACE_FEED_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief A synthetic FX volatility surface for one pair: how its SSVI state moves and how it moves
 * with spot.
 *
 * A synthetic volatility surface of one currency pair, as
 * [[id:D4C7C4A7-CD2D-4307-AB06-DA9BE3C3A334][A synthetic contributor publishes noisy observations
 * of a latent surface]] describes. The surface is SSVI with gamma fixed at one half. Its state is
 * the forward variance of each segment between the feed's expiries, which the
 * fx_vol_surface_feed_expiry rows hold, a skew driver and a curvature driver,
 * each reverting to where the surface started.
 *
 * The correlation of the shocks is three numbers, and the feed builds the matrix
 * from them: spot with every forward variance, spot with the skew, and how strongly
 * neighbouring forward variances move together, which falls by that factor for each
 * segment between them. Every other shock is independent of spot. The surface is
 * quoted by the contributors fx_vol_surface_feed_contributor names.
 */
struct fx_vol_surface_feed final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this row.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party: the system party, which runs the installation's feeds.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Parent market_data_generation_config this feed belongs to.
     */
    boost::uuids::uuid config_id;

    /**
     * @brief The base currency of the pair, such as EUR in EUR/USD.
     */
    std::string base_currency_code;

    /**
     * @brief The quote currency of the pair.
     */
    std::string quote_currency_code;

    /**
     * @brief The source the feed publishes under, which names its subject.
     */
    std::string source_name;

    /**
     * @brief How many surfaces the feed emits an hour.
     */
    int ticks_per_hour = 0;

    /**
     * @brief Whether the feed may be started at all.
     */
    bool enabled = false;

    /**
     * @brief Whether the feed starts on its own when the service starts.
     */
    bool auto_start = false;

    /**
     * @brief The folder the feed lives under, as every feed of a collection does.
     */
    std::optional<boost::uuids::uuid> folder_id;

    /**
     * @brief The asset class the feed belongs to: always fx.
     */
    std::string asset_class_code = "fx";

    /**
     * @brief How the feed's ticks arrive: periodic, poisson or hawkes, as for every feed.
     */
    std::string arrival_mode = "periodic";

    /**
     * @brief For hawkes: the expected number of further ticks per tick, below one.
     */
    double branching_ratio = 0.0;

    /**
     * @brief For hawkes: how fast a burst fades, per second.
     */
    double decay = 1.0;

    /**
     * @brief How strongly a market-wide regime moves this feed, where one is the average.
     */
    double sensitivity = 1.0;

    /**
     * @brief How fast the skew driver reverts to its start, per year.
     */
    double skew_reversion{};

    /**
     * @brief The volatility of the skew driver, per square root of a year.
     */
    double skew_volatility{};

    /**
     * @brief How fast the curvature driver reverts to its start, per year.
     */
    double curvature_reversion{};

    /**
     * @brief The volatility of the curvature driver, per square root of a year.
     */
    double curvature_volatility{};

    /**
     * @brief The correlation of spot with every forward variance. It has no universal sign in FX: a
     * crisis lifts the volatility of a pair whichever way its safe-haven currency moves.
     */
    double spot_variance_correlation{};

    /**
     * @brief The correlation of spot with the skew driver, which moves the risk reversal toward the
     * side the market buys protection on.
     */
    double spot_skew_correlation{};

    /**
     * @brief The correlation of neighbouring forward variances; segments further apart correlate by
     * this factor once for each step between them, so the curve mostly moves as one.
     */
    double segment_correlation{};

    /**
     * @brief The skew the surface starts at, and the level the skew driver reverts to: the rho of
     * the SSVI parameterisation, inside (-1, 1). FX usually quotes a negative skew, with the
     * downside bid harder than the upside.
     */
    double start_skew{};

    /**
     * @brief The curvature the surface starts at, and the level the curvature driver reverts to:
     * the eta of the SSVI parameterisation with gamma fixed at one half, inside (0, the square root
     * of two). It sets how far the wings rise above the ATM, and the surface is free of butterfly
     * arbitrage only below that bound.
     */
    double start_curvature{};

    /**
     * @brief Username of the person who last modified this FX volatility surface feed.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const fx_vol_surface_feed&, const fx_vol_surface_feed&) = default;
};

/**
 * @brief Dispatch-key identifier for fx_vol_surface_feed, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_vol_surface_feed&) {
    return "ores.synthetic.fx_vol_surface_feed";
}

}

#endif
