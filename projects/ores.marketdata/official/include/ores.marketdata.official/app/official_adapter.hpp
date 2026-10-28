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
#ifndef ORES_MARKETDATA_OFFICIAL_APP_OFFICIAL_ADAPTER_HPP
#define ORES_MARKETDATA_OFFICIAL_APP_OFFICIAL_ADAPTER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.official/domain/source_feed.hpp"
#include "ores.marketdata.official/export.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include <atomic>
#include <chrono>
#include <memory>
#include <mutex>
#include <optional>
#include <thread>
#include <vector>

namespace ores::marketdata::official::app {

/**
 * @brief The timing of the adapter.
 */
struct adapter_timing final {
    /// The time between two heartbeats. The service expires an adapter after 15 seconds.
    std::chrono::milliseconds heartbeat_interval{5000};
    /// The time between two announces once one succeeded.
    std::chrono::milliseconds reannounce_interval{60000};
    /// The time between two announces, or two reads of the universe, while none succeeded.
    std::chrono::milliseconds retry_interval{5000};
    /// How long the adapter waits for the reply to an announce.
    std::chrono::milliseconds reply_timeout{2000};
    /// The time between two refreshes of a source once its demand is served.
    std::chrono::milliseconds poll_interval{std::chrono::hours(1)};
};

/**
 * @brief Serves each hosted source over the adapter contract.
 *
 * One thread reads each source's universe, announces it, sends its heartbeat and
 * refreshes it: at once after a new demand, and then at the poll interval. A demand set is
 * answered on the bus's own thread from the universe alone, and the loop takes it, so a
 * refresh that waits on Frankfurter never delays a reply.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT official_adapter final {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.marketdata.official.app.official_adapter");
        return instance;
    }

public:
    official_adapter(ores::nats::service::client& nats,
                     std::vector<domain::hosted_source> sources,
                     domain::frankfurter_fetcher& fetcher,
                     int backfill_days,
                     adapter_timing timing);
    ~official_adapter();

    official_adapter(const official_adapter&) = delete;
    official_adapter& operator=(const official_adapter&) = delete;

    /// Subscribes to the demand subject of each source.
    std::vector<ores::nats::service::subscription> subscribe();

    void start();
    void stop();

private:
    struct served final {
        explicit served(std::unique_ptr<domain::source_feed> f)
            : feed(std::move(f)) {}
        std::unique_ptr<domain::source_feed> feed;
        std::mutex mu;
        std::optional<messaging::adapter_demand_request> pending;
        std::atomic<bool> ready{false};
        bool announced = false;
        std::uint64_t beats = 0;
        std::chrono::steady_clock::time_point next_universe{};
        std::chrono::steady_clock::time_point next_announce{};
        std::chrono::steady_clock::time_point next_heartbeat{};
        std::chrono::steady_clock::time_point next_refresh{};
    };

    void on_demand(served& s, const ores::nats::message& msg);
    bool announce(served& s);
    /// Serves one source, and reads Frankfurter only when @p may_refresh; says whether it
    /// read it.
    bool serve(served& s, std::chrono::steady_clock::time_point now, bool may_refresh);
    void loop();

    ores::nats::service::client& nats_;
    adapter_timing timing_;
    std::vector<std::unique_ptr<served>> served_;
    std::atomic<bool> stop_{false};
    std::thread thread_;
};

}

#endif
