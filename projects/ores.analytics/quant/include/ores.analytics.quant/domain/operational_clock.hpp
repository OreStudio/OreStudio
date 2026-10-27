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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_OPERATIONAL_CLOCK_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_OPERATIONAL_CLOCK_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/rng_stream.hpp"
#include <cstddef>
#include <cstdint>
#include <deque>
#include <optional>

namespace ores::analytics::quant::domain {

/// Whether the market is calm or busy.
enum class market_state { calm, active };

/**
 * @brief The numbers that set how the market's pace moves.
 */
struct market_config final {
    /// How often a calm market turns active, per second.
    double wake_rate = 1.0 / 600.0;
    /// How often an active market turns calm, per second.
    double calm_rate = 1.0 / 300.0;
    /// The pace of an active market. The calm pace follows, so the long-run
    /// mean pace is one.
    double active_multiplier = 2.0;
    /// The heat one event adds, as a rate.
    double jump = 0.0;
    /// How fast the heat fades, per second.
    double decay = 1.0;
};

/**
 * @brief The market's one clock: the regime chain, the heat, and tau.
 *
 * Every feed is scaled by one bracket, B = M + C / R: the regime's multiplier M,
 * plus the heat C shared over the enrolled rate R. So every feed shares one
 * operational time, tau, the integral of B over wall time. This object owns all
 * three, advances tau exactly between events, and says when tau will reach a
 * mark, so a scheduler can sleep until then.
 *
 * Between events B is a constant M plus a heat that decays exponentially, so
 * tau has a closed form. An event, a pin or an enrolment changes how fast tau
 * moves from that instant and never what has already passed.
 *
 * The chain draws its dwell times from a keyed stream. The k-th dwell is the
 * k-th draw whichever call asks for it first, so time_to_reach can look past a
 * transition and advance takes the same transition at the same time.
 *
 * Owned by one loop and not thread-safe. Times are seconds from the start of
 * the run.
 */
class ORES_ANALYTICS_QUANT_EXPORT operational_clock final {
public:
    /**
     * @throws std::invalid_argument when a rate or a multiplier is not
     *         positive, when the active multiplier leaves no positive calm one,
     *         or when the heat's loop gain, jump over decay, is one or more.
     */
    operational_clock(const market_config& config, math::rng_stream stream);

    /// Seconds since the start of the run.
    [[nodiscard]] double now() const {
        return now_;
    }

    /// The market's operational time.
    [[nodiscard]] double tau() const {
        return tau_;
    }

    /// The state in force: the pin if there is one, else the chain's.
    [[nodiscard]] market_state state() const {
        return pin_.value_or(chain_state_);
    }

    [[nodiscard]] bool is_pinned() const {
        return pin_.has_value();
    }

    [[nodiscard]] double heat() const {
        return heat_;
    }

    [[nodiscard]] double population_rate() const {
        return population_rate_;
    }

    /// The bracket B: how fast tau moves now, per second.
    [[nodiscard]] double pace() const;

    [[nodiscard]] double jump() const {
        return config_.jump;
    }

    [[nodiscard]] double decay() const {
        return config_.decay;
    }

    /// The heat's loop gain, jump over decay. Below one, so the heat settles.
    [[nodiscard]] double loop_gain() const {
        return config_.jump / config_.decay;
    }

    /// The multiplier of each state; the two average to one in the long run.
    [[nodiscard]] double multiplier(market_state s) const {
        return s == market_state::active ? config_.active_multiplier : calm_multiplier_;
    }

    /// Seconds until the chain next turns, or nothing while the state is pinned.
    [[nodiscard]] std::optional<double> next_transition_in() const;

    /// Move time on by @p seconds, taking every transition that falls inside.
    void advance(double seconds);

    /**
     * @brief The seconds from now at which tau reaches @p target, if nothing
     * else happens first.
     *
     * Exact up to the solver's tolerance, across any number of transitions. A
     * target already passed gives zero. Not const: looking past a transition may draw that
     * transition's dwell, which advance then uses.
     */
    [[nodiscard]] double time_to_reach(double target);

    void pin(market_state s);
    void unpin();

    /// One event anywhere in the market: the heat rises by the jump.
    void record_event();

    /// A series that runs at @p mean_rate a second joins the population.
    void enrol(double mean_rate);

    /// A series that ran at @p mean_rate a second leaves the population.
    void withdraw(double mean_rate);

private:
    /// What heat @p heat adds to the pace, shared over the enrolled rate.
    [[nodiscard]] double share_of(double heat) const;

    /// How much tau moves over @p seconds at multiplier @p m from heat @p heat.
    [[nodiscard]] double tau_over(double seconds, double m, double heat) const;

    /// The seconds over which tau moves by @p distance at multiplier @p m.
    [[nodiscard]] double seconds_for(double distance, double m, double heat) const;

    /// The unit-mean dwell of the @p k-th transition from now, drawn if needed.
    double unit_dwell(std::size_t k);

    market_config config_;
    double calm_multiplier_;
    math::rng_stream stream_;
    std::deque<double> upcoming_dwells_;

    double now_ = 0.0;
    double tau_ = 0.0;
    market_state chain_state_ = market_state::calm;
    double remaining_ = 0.0;
    std::optional<market_state> pin_;
    double heat_ = 0.0;
    double population_rate_ = 0.0;
    std::uint64_t enrolled_ = 0;
};

}

#endif
