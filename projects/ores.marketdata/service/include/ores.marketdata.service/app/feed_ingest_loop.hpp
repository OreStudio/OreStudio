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
#ifndef ORES_MARKETDATA_SERVICE_APP_FEED_INGEST_LOOP_HPP
#define ORES_MARKETDATA_SERVICE_APP_FEED_INGEST_LOOP_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include <atomic>
#include <chrono>
#include <map>
#include <mutex>
#include <string>
#include <thread>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief Subscribes to raw synthetic producer channels and republishes on the
 * official tenant-scoped stream.
 *
 * On start(), all enabled feed_binding rows are loaded; for each one a NATS
 * subscription is opened on "synthetic.v1.tick.<source_name>". Each arriving
 * fx_spot_tick is:
 *   1. Persisted as a market_observation (tenant + no point_id, source in commentary).
 *   2. Re-published verbatim on "marketdata.v1.tick.<ore_key_subject>" so that
 *      existing consumers (fx_spot_subscription, chart) continue to work unchanged.
 *
 * refresh() re-reads the bindings table and reconciles subscriptions: new
 * enabled bindings are subscribed, disabled/deleted bindings are unsubscribed.
 * It is called by the feed_binding NATS notify trigger handler on every change.
 */
class ORES_MARKETDATA_SERVICE_EXPORT feed_ingest_loop {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.marketdata.service.app.feed_ingest_loop");
        return instance;
    }

public:
    feed_ingest_loop(ores::nats::service::client& nats, ores::database::context ctx);
    ~feed_ingest_loop();

    void start();
    void refresh();

private:
    void subscribe_binding(const std::string& ore_key, const std::string& source_name,
                           const std::string& tenant_id_str);
    void unsubscribe_binding(const std::string& source_name);
    void status_loop();
    void log_status() const;

    struct feed_stats {
        std::string ore_key;
        std::string nats_subject;
        std::atomic<std::uint64_t> tick_count{0};
        std::atomic<std::chrono::system_clock::time_point::rep> last_tick_rep{
            std::chrono::system_clock::time_point::min().time_since_epoch().count()};
    };

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    mutable std::mutex mu_;
    std::map<std::string, ores::nats::service::subscription> subs_;
    std::map<std::string, std::shared_ptr<feed_stats>> stats_;

    static constexpr std::chrono::minutes status_interval_{1};
    std::atomic<bool> stop_flag_{false};
    std::thread status_thread_;
};

} // namespace ores::marketdata::service::app

#endif
