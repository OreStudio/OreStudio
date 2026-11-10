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
#include "ores.marketdata.official/domain/fetcher.hpp"
#include "ores.marketdata.official/export.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <chrono>
#include <cstdint>
#include <expected>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <tuple>
#include <vector>

namespace ores::marketdata::official::domain {

/**
 * @brief One source the process hosts: its name, which is its adapter id, the catalogue
 * provider it belongs to, the adapter that reads it, and the key of the publisher in that
 * adapter's own notation.
 */
struct hosted_source final {
    std::string source_name;
    std::string provider_code;
    std::string adapter_code;
    std::string key;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<hosted_source>) {
    return std::tuple{
        ::ores::utility::codec::field{"source_name", &hosted_source::source_name},
        ::ores::utility::codec::field{"provider_code", &hosted_source::provider_code},
        ::ores::utility::codec::field{"adapter_code", &hosted_source::adapter_code},
        ::ores::utility::codec::field{"key", &hosted_source::key},
    };
}

static_assert(::ores::utility::codec::names_every_member<hosted_source>(),
              "hosted_source has a member its wire shape does not name");

/**
 * @brief What one source serves, and what it has sent.
 *
 * Its universe is the ids its publisher publishes, in the notation of its fetcher. The
 * demand set narrows it to what the market data service wants. A refresh sends every
 * published value of a demanded id newer than the last it sent, oldest first, and an id it
 * has never sent starts from the backfill window. The store skips what it already holds, so
 * a restart, which forgets what was sent, sends the window again at no cost.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT source_feed final {
public:
    using publish_fn = std::function<void(const messaging::adapter_tick&)>;

    source_feed(hosted_source source, fetcher& reader, int backfill_days);

    [[nodiscard]] const hosted_source& source() const {
        return source_;
    }

    /// The fetcher that reads the source's publisher.
    [[nodiscard]] const fetcher& reader() const {
        return reader_;
    }

    /// Reads the universe from the publisher.
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

    /// The tick a fixing becomes, in the fetcher's notation, stamped at midday UTC of its date.
    [[nodiscard]] messaging::adapter_tick tick_of(const published_fixing& fixing,
                                                  std::uint64_t sequence) const;

private:
    hosted_source source_;
    fetcher& reader_;
    int backfill_days_{};
    std::set<std::string> universe_;
    std::set<std::string> demanded_;
    std::map<std::string, std::chrono::year_month_day> last_sent_;
    std::map<std::string, std::chrono::year_month_day> read_through_;
    std::uint64_t sequence_ = 0;
};

}

#endif
