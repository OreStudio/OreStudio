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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_SOURCE_FEED_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_SOURCE_FEED_HPP

#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.marketdata.official/domain/frankfurter.hpp"
#include "ores.marketdata.official/export.hpp"
#include <chrono>
#include <cstdint>
#include <expected>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace ores::marketdata::official::domain {

/**
 * @brief One source the adapter hosts: its name, which is its adapter id, the catalogue
 * provider it belongs to, and the Frankfurter key of the publisher it reads.
 */
struct hosted_source final {
    std::string source_name;
    std::string provider_code;
    std::string frankfurter_key;
};

/// The id scheme of the publishers' own notation: the currency a rate is quoted in.
inline constexpr std::string_view id_scheme = "iso4217";
/// The field of a vendor id: the rate of the pivot in that currency.
inline constexpr std::string_view rate_field = "rate";

/**
 * @brief What one source serves, and what it has sent.
 *
 * Its universe is the currencies its publisher quotes against its pivot. The demand set
 * narrows it to what the market data service wants. A refresh sends every published rate
 * of a demanded currency newer than the last it sent, oldest first, and a currency it has
 * never sent starts from the backfill window. The store skips what it already holds, so
 * a restart, which forgets what was sent, sends the window again at no cost.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT source_feed final {
public:
    using publish_fn = std::function<void(const messaging::adapter_tick&)>;

    source_feed(hosted_source source, frankfurter_fetcher& fetcher, int backfill_days);

    [[nodiscard]] const hosted_source& source() const {
        return source_;
    }

    /// Reads the universe from Frankfurter.
    std::expected<void, std::string> load_universe();

    [[nodiscard]] bool has_universe() const {
        return !universe_.empty();
    }

    [[nodiscard]] messaging::adapter_announce_request announcement() const;

    /// The answer to a demand set: the ids of the universe are accepted, the others rejected.
    [[nodiscard]] messaging::adapter_demand_response
    answer(const messaging::adapter_demand_request& request) const;

    /// Takes the demand set, so a refresh sends the accepted ids.
    messaging::adapter_demand_response demand(const messaging::adapter_demand_request& request);

    /**
     * @brief Sends what is new up to @p today through @p publish, and returns how many ticks
     * it sent.
     */
    std::expected<std::size_t, std::string> refresh(std::chrono::year_month_day today,
                                                    const publish_fn& publish);

    /// The rates refused because their base was not the provider's pivot.
    [[nodiscard]] std::uint64_t misbased() const {
        return misbased_;
    }

    /// The fixing of a rate: the tick a rate becomes, stamped at midday UTC of its date.
    [[nodiscard]] static messaging::adapter_tick tick_of(const published_rate& rate,
                                                         std::uint64_t sequence);

private:
    hosted_source source_;
    frankfurter_fetcher& fetcher_;
    int backfill_days_;
    std::string pivot_;
    std::set<std::string> universe_;
    std::set<std::string> demanded_;
    std::map<std::string, std::chrono::year_month_day> last_sent_;
    std::uint64_t sequence_ = 0;
    std::uint64_t misbased_ = 0;
};

}

#endif
