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
#include "ores.analytics.quant/domain/market_board.hpp"
#include <algorithm>
#include <format>
#include <stdexcept>
#include <utility>

namespace ores::analytics::quant::domain {

market_board::market_board(std::size_t capacity)
    : members_(capacity) {
    if (capacity == 0)
        throw std::invalid_argument("A market board needs room for at least one member");
}

market_board::member_id market_board::member_of(market_member_kind kind, std::string code) {
    const std::lock_guard lock(members_mutex_);

    const auto taken = count_.load(std::memory_order_relaxed);
    for (std::size_t i = 0; i < taken; ++i)
        if (members_[i].kind == kind && members_[i].code == code)
            return static_cast<member_id>(i);

    if (taken == members_.size())
        throw std::length_error(
            std::format("A market board of {} members cannot take {}", members_.size(), code));

    members_[taken].kind = kind;
    members_[taken].code = std::move(code);
    count_.store(taken + 1, std::memory_order_release);
    return static_cast<member_id>(taken);
}

std::optional<market_board::member_id> market_board::find(market_member_kind kind,
                                                          std::string_view code) const {
    const std::lock_guard lock(members_mutex_);

    const auto taken = count_.load(std::memory_order_relaxed);
    for (std::size_t i = 0; i < taken; ++i)
        if (members_[i].kind == kind && members_[i].code == code)
            return static_cast<member_id>(i);
    return std::nullopt;
}

void market_board::require_member(member_id id) const {
    if (static_cast<std::size_t>(id) >= count_.load(std::memory_order_acquire))
        throw std::invalid_argument(std::format("Market board member {} does not exist", id));
}

void market_board::publish_curve(member_id id,
                                 std::chrono::year_month_day spot_date,
                                 std::span<const curve_point> points,
                                 std::chrono::system_clock::time_point at) {
    require_member(id);

    auto& m = members_[id];
    if (m.kind != market_member_kind::curve)
        throw std::invalid_argument(
            std::format("Market board member {} holds a spot, not a curve", id));

    // The member's own storage, reused: this allocates on the first publish of a
    // curve of this size and never again while the size holds.
    m.points.assign(points.begin(), points.end());
    m.spot_date = spot_date;
    m.published_at = at;
    m.published = true;
}

void market_board::publish_spot(member_id id,
                                double level,
                                std::chrono::system_clock::time_point at) {
    require_member(id);

    auto& m = members_[id];
    if (m.kind != market_member_kind::spot)
        throw std::invalid_argument(
            std::format("Market board member {} holds a curve, not a spot", id));

    m.spot = level;
    m.published_at = at;
    m.published = true;
}

market_member_view market_board::view(member_id id) const {
    require_member(id);

    const auto& m = members_[id];
    return {.kind = m.kind,
            .code = m.code,
            .published = m.published,
            .published_at = m.published_at,
            .spot_date = m.spot_date,
            .points = std::span<const curve_point>(m.points),
            .spot = m.spot};
}

std::size_t market_board::size() const {
    return count_.load(std::memory_order_acquire);
}

}
