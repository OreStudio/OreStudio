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
#ifndef ORES_HTTP_ROUTES_MARKETDATA_QUOTE_BOARD_HPP
#define ORES_HTTP_ROUTES_MARKETDATA_QUOTE_BOARD_HPP

#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace ores::http::routes::marketdata {

/**
 * @brief The latest quote of one datum on one feed, labelled for the quotes view.
 *
 * The labels are read once from the URI by the codec, when the datum is first heard,
 * so a read of the board labels nothing.
 */
struct quote final {
    std::string provider;
    std::string feed;
    /// The name of the source that published it, as the tick states it.
    std::string source;
    std::string uri;
    std::string value;
    /// The value of the datum's previous tick on this feed, or the value itself for its first.
    std::string previous;
    std::chrono::system_clock::time_point at;
    /// The board's count when the quote arrived, which orders the reads.
    std::uint64_t sequence = 0;
    std::string instrument_type;
    /// The refdata asset class code the URI's authority maps onto, or empty for none.
    std::string asset_class;
    std::string series_uri;
    /// The words of the series: its subject and the values of its identity fields.
    std::string series;
    /// The words of the point within the series: the values of its coordinates.
    std::string point;
};

/**
 * @brief The latest quote of every datum on every feed, as the ingest publishes them.
 *
 * One subscription hears every feed's subject, so the board costs the same whatever the
 * number of sessions that read it. It holds no entitlement: a read names the feeds the
 * caller may watch, and the board returns those alone.
 */
class quote_board final {
public:
    /// What a tick on a feed's subject is handed to, with the subject it came on.
    using tick_handler = std::function<void(const std::string& subject,
                                            const ores::marketdata::messaging::market_tick&)>;
    /**
     * @brief Listens on a subject, and answers the handle whose destruction stops the
     * listening.
     */
    using subscriber =
        std::function<std::shared_ptr<void>(const std::string& subject, tick_handler on_tick)>;
    /// The feed a quote belongs to: the provider and the feed id.
    using feed_key = std::pair<std::string, std::string>;

    /// How many quotes the board keeps; a datum first heard beyond it is not kept.
    static constexpr std::size_t capacity = 200'000;

    explicit quote_board(subscriber subscribe);

    /**
     * @brief The subscriber that listens through a client's transport and reads each
     * tick with the process's wire codec.
     */
    [[nodiscard]] static subscriber nats_subscriber(ores::nats::service::nats_client& session);

    /// Starts listening, the first time it is called.
    void start();

    /**
     * @brief Takes one tick heard on @p subject.
     *
     * A subject that names no provider and feed, or a URI the codec cannot read, is
     * ignored.
     */
    void take(const std::string& subject, const ores::marketdata::messaging::market_tick& tick);

    /// The quotes of @p feeds that arrived after @p sequence, oldest first.
    [[nodiscard]] std::vector<quote> after(std::uint64_t sequence,
                                           const std::set<feed_key>& feeds) const;

    /// The board's count now: the sequence of the newest quote.
    [[nodiscard]] std::uint64_t sequence() const;

    /// The source name and the time of the newest quote of each feed it has heard.
    [[nodiscard]] std::map<feed_key, std::pair<std::string, std::chrono::system_clock::time_point>>
    feeds() const;

private:
    subscriber subscribe_;
    std::once_flag started_;
    std::shared_ptr<void> listening_;
    mutable std::mutex mutex_;
    std::uint64_t sequence_ = 0;
    /// The quotes by feed and URI.
    std::map<std::pair<feed_key, std::string>, quote> quotes_;
};

}

#endif
