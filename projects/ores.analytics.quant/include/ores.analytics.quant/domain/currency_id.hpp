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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_CURRENCY_ID_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_CURRENCY_ID_HPP

#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>

namespace ores::analytics::quant::domain {

/**
 * @brief Opaque, compact handle for a currency within a single @c crm_topology.
 *
 * Assigned by @c topology_builder when it first sees a currency code; never
 * meaningful across two different topologies. Deliberately not the ISO
 * currency code itself, so the runtime engine can index dense arrays by
 * @c index() instead of hashing strings on every lookup.
 */
class currency_id {
public:
    constexpr currency_id() noexcept : index_(invalid_index) {}
    constexpr explicit currency_id(std::uint16_t index) noexcept : index_(index) {}

    [[nodiscard]] constexpr std::uint16_t index() const noexcept { return index_; }
    [[nodiscard]] constexpr bool valid() const noexcept { return index_ != invalid_index; }

    friend constexpr bool operator==(currency_id lhs, currency_id rhs) noexcept {
        return lhs.index_ == rhs.index_;
    }
    friend constexpr bool operator!=(currency_id lhs, currency_id rhs) noexcept {
        return !(lhs == rhs);
    }
    friend constexpr bool operator<(currency_id lhs, currency_id rhs) noexcept {
        return lhs.index_ < rhs.index_;
    }

private:
    static constexpr std::uint16_t invalid_index =
        std::numeric_limits<std::uint16_t>::max();
    std::uint16_t index_;
};

} // namespace ores::analytics::quant::domain

template <>
struct std::hash<ores::analytics::quant::domain::currency_id> {
    std::size_t operator()(
        ores::analytics::quant::domain::currency_id id) const noexcept {
        return std::hash<std::uint16_t>{}(id.index());
    }
};

#endif
