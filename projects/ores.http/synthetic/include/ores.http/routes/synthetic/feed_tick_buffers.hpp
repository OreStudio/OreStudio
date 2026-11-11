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
#ifndef ORES_HTTP_ROUTES_SYNTHETIC_FEED_TICK_BUFFERS_HPP
#define ORES_HTTP_ROUTES_SYNTHETIC_FEED_TICK_BUFFERS_HPP

#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <chrono>
#include <cstddef>
#include <deque>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <string>
#include <vector>

namespace ores::http::routes::synthetic {

/**
 * @brief One tick as a screen plots it: when, the price as a number, and the
 * oresmd URI of the datum it is a value of.
 */
struct feed_tick final {
    std::chrono::system_clock::time_point at;
    double value{};
    std::string uri;
};

/**
 * @brief The recent ticks of the feeds a screen asked about, shared by every
 * session.
 *
 * A feed publishes its ticks on a subject a screen can tail, and the buffers
 * keep the newest ones of each source a read has named. A buffer holds ticks,
 * not a run's state, so a read of a feed that stopped finds whatever was heard
 * while it ran, until a raster read stops listening to the feeds that no longer
 * run, as the BFF's did. At most a thousand sources are heard at once, so reads
 * that name feed after feed cannot grow the subscriptions without end.
 *
 * Ticks arrive on the transport's threads and reads on the server's, so each
 * source's ticks are guarded by a lock of their own, which a tick takes and
 * nothing else does while it holds it.
 */
class feed_tick_buffers final {
public:
    /// What a tick on a source's subject is handed to.
    using tick_handler = std::function<void(const ores::marketdata::messaging::market_tick&)>;

    /**
     * @brief Listens on a subject, and answers the handle whose destruction stops
     * the listening.
     */
    using subscriber =
        std::function<std::shared_ptr<void>(const std::string& subject, tick_handler on_tick)>;

    /// How many ticks a source's buffer keeps, the newest last.
    static constexpr std::size_t capacity = 500;

    /// How many sources are heard at once.
    static constexpr std::size_t most_sources = 1000;

    explicit feed_tick_buffers(subscriber subscribe);

    /**
     * @brief The subscriber that listens through a client's transport and reads
     * each tick with the process's wire codec.
     *
     * The client must outlive every buffer this subscriber serves.
     */
    [[nodiscard]] static subscriber nats_subscriber(ores::nats::service::nats_client& session);

    /**
     * @brief Starts listening to one source the first time a read names it.
     *
     * @return whether the source is heard: false when it is new and the buffers
     *         already hear as many sources as they may.
     */
    bool watch(const std::string& source_name);

    /// The newest ticks of a source, at most @p limit, oldest first.
    [[nodiscard]] std::vector<feed_tick> ticks(const std::string& source_name,
                                               std::size_t limit) const;

    /**
     * @brief When a source's ticks arrived from an instant on, at most @p limit of
     * them, in the order they were heard.
     *
     * The time is the one the feed stamps a tick with, its observation time, as
     * the BFF's raster read it; a feed stamps it as it publishes.
     */
    [[nodiscard]] std::vector<std::chrono::system_clock::time_point>
    arrival_times(const std::string& source_name,
                  std::chrono::system_clock::time_point since,
                  std::size_t limit = capacity) const;

    /**
     * @brief Stops listening to every source the caller no longer names, so the
     * subscriptions stay at the size of what runs.
     */
    void keep_only(const std::vector<std::string>& source_names);

    /// How many sources are heard from.
    [[nodiscard]] std::size_t size() const;

private:
    struct run final {
        mutable std::mutex mutex;
        std::deque<feed_tick> ticks;
    };

    struct heard final {
        std::shared_ptr<run> ticks;
        std::shared_ptr<void> stop;
    };

    [[nodiscard]] std::shared_ptr<run> run_of(const std::string& source_name) const;

    subscriber subscribe_;
    mutable std::mutex mutex_;
    std::map<std::string, heard, std::less<>> runs_;
};

}

#endif
