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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_FETCHER_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_FETCHER_HPP

#include <chrono>
#include <expected>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::official::domain {

/**
 * @brief One value a publisher published for one of its ids on one date.
 */
struct published_fixing final {
    std::chrono::year_month_day date;
    /// The publisher's own id of the value, such as a currency or a series code.
    std::string external_id;
    /// The value as the shortest text that reads back to the published number.
    std::string value;
};

/**
 * @brief How one adapter reads its publisher: the specialised half of an adapter.
 *
 * Each adapter reads one access route, with its own API, format and terms. The source
 * feed and the adapter loop do the rest, which is the same for every publisher: the
 * announce, the demand, the backfill and the publish.
 *
 * The contract that the adapter keeps:
 * - A fetcher lives longer than the adapter that hosts it, which keeps a reference to it.
 * - One thread calls a fetcher, so a fetcher may keep state without a lock.
 * - The adapter reads the universe of a key before it reads the fixings of that key.
 */
class fetcher {
public:
    virtual ~fetcher() = default;

    /// The code of the adapter module, which the announce names.
    [[nodiscard]] virtual std::string_view adapter_code() const = 0;

    /// The scheme of the publisher's ids, which the announce and each tick name.
    [[nodiscard]] virtual std::string_view id_scheme() const = 0;

    /// The field of the publisher's ids, which the announce and each tick name.
    [[nodiscard]] virtual std::string_view field() const = 0;

    /// The ids the publisher behind @p key publishes.
    [[nodiscard]] virtual std::expected<std::vector<std::string>, std::string>
    universe(std::string_view key) = 0;

    /// What the publisher behind @p key published from @p from to @p to, both included.
    [[nodiscard]] virtual std::expected<std::vector<published_fixing>, std::string> fixings(
        std::string_view key, std::chrono::year_month_day from, std::chrono::year_month_day to) = 0;
};

}

#endif
