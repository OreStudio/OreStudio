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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_MARKET_BOARD_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_MARKET_BOARD_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/fx_pair_market.hpp"
#include <atomic>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <mutex>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace ores::analytics::quant::domain {

/**
 * @brief One dated discount factor of a currency's curve.
 *
 * The first is the spot date at exactly one, so a reader interpolates from a spot date
 * outwards rather than from an absolute epoch. It is the quant math type because that is
 * what resolves a pair's market from two curves, and a reader hands these points to it
 * without a copy.
 */
using curve_point = math::fx_curve_point;

/// What a member of the board holds: a currency's curve, or a pair's spot.
enum class market_member_kind : std::uint8_t { curve, spot };

/**
 * @brief A member as a reader sees it.
 *
 * The span is valid until the member is published again, which on the ticking
 * thread is its next tick, so a reader copies what it needs. A member that has
 * never been published answers =published = false= with nothing in it, which is
 * how a reader tells "not yet" from a curve of zeros.
 */
struct market_member_view final {
    market_member_kind kind = market_member_kind::curve;
    std::string_view code;
    bool published = false;
    std::chrono::system_clock::time_point published_at{};
    /// A curve's spot date; unset for a spot member.
    std::chrono::year_month_day spot_date{};
    /// A curve's dated discount factors, ascending by date; empty for a spot.
    std::span<const curve_point> points{};
    /// A spot's level; zero for a curve.
    double spot = 0.0;
};

/**
 * @brief The values a run's feeds share with each other: their curves and their spots.
 *
 * A vendor does not publish its curves as market data, but its own feeds must
 * agree on them: a volatility surface solves its strikes against the forward of
 * its pair, and the forward is the ratio of two curves. The board is that shared
 * state, and only that: it holds no prices, no history and no lineage.
 *
 * *Data-oriented.* The board is sized once, when the run is built, so a member
 * never moves and an id is stable for the life of the run. A feed asks for its id
 * once, when it is built, and afterwards publishes and reads by index: no lookup,
 * no hashing and no string. Publishing reuses the member's own storage, so a feed
 * allocates on its first tick and never again unless its curve grows a point, and
 * a reader takes a view of that storage rather than a copy.
 *
 * *Threads.* Creating a member is the one operation that takes a lock, because
 * feeds are built by the verbs that start them rather than on the loop. A
 * member's values belong to the thread that ticks it -- the scheduler's loop --
 * and are read there by the feeds that consume them, so publishing and reading
 * take no lock at all.
 */
class ORES_ANALYTICS_QUANT_EXPORT market_board final {
public:
    /// A member's handle. Stable for the life of the board.
    using member_id = std::uint32_t;

    /**
     * @param capacity How many members the run may ever have. A curve per
     *        currency, a spot per quoted pair, and room to grow; the storage is
     *        allocated once here so that nothing below has to.
     * @throws std::invalid_argument if @p capacity is zero.
     */
    explicit market_board(std::size_t capacity = 256);

    market_board(const market_board&) = delete;
    market_board& operator=(const market_board&) = delete;
    market_board(market_board&&) = delete;
    market_board& operator=(market_board&&) = delete;

    /**
     * @brief The member for a kind and a code, creating it the first time.
     *
     * A feed calls this once, when it is built, and keeps the id. A second call
     * with the same kind and code answers the same member rather than a second
     * copy of it.
     *
     * @throws std::length_error when the board is full.
     */
    [[nodiscard]] member_id member_of(market_member_kind kind, std::string code);

    /**
     * @brief The member for a kind and a code if the board already has one.
     *
     * What a reader uses when it must not create members: a screen asking for a
     * curve the run does not carry gets nothing, not an empty one.
     */
    [[nodiscard]] std::optional<member_id> find(market_member_kind kind,
                                                std::string_view code) const;

    /**
     * @brief Publishes a currency's curve.
     *
     * Replaces whatever the member held, including its timestamp, because a curve
     * is republished whole rather than amended point by point.
     *
     * @throws std::invalid_argument if @p id is not a member, or is a spot member.
     */
    void publish_curve(member_id id,
                       std::chrono::year_month_day spot_date,
                       std::span<const curve_point> points,
                       std::chrono::system_clock::time_point at);

    /**
     * @brief Publishes a pair's spot.
     *
     * @throws std::invalid_argument if @p id is not a member, or is a curve member.
     */
    void publish_spot(member_id id, double level, std::chrono::system_clock::time_point at);

    /**
     * @brief The member's values now.
     *
     * @throws std::invalid_argument if @p id is not a member.
     */
    [[nodiscard]] market_member_view view(member_id id) const;

    /// How many members the board knows, published or not.
    [[nodiscard]] std::size_t size() const;

    /// How many members the run may ever have.
    [[nodiscard]] std::size_t capacity() const noexcept {
        return members_.size();
    }

private:
    struct member final {
        market_member_kind kind = market_member_kind::curve;
        std::string code;
        bool published = false;
        std::chrono::system_clock::time_point published_at{};
        std::chrono::year_month_day spot_date{};
        std::vector<curve_point> points;
        double spot = 0.0;
    };

    void require_member(member_id id) const;

    /// Guards the member list: adding a member, and the scan that finds one.
    mutable std::mutex members_mutex_;
    /// Every slot the run may use, allocated once. A member never moves.
    std::vector<member> members_;
    /// How many slots are taken. The only field the ticking thread reads.
    std::atomic<std::size_t> count_{0};
};

}

#endif
