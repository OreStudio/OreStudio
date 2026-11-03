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
#ifndef ORES_SYNTHETIC_SERVICE_DERIVED_FX_VOL_ADAPTER_HPP
#define ORES_SYNTHETIC_SERVICE_DERIVED_FX_VOL_ADAPTER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.synthetic.api/feeds/derived_fx_vol_feed.hpp"
#include <atomic>
#include <chrono>
#include <cstdint>
#include <mutex>
#include <optional>
#include <string>
#include <thread>

namespace ores::synthetic::service {

/** The timing of the derived source, as a seam so a test runs in milliseconds. */
struct derived_fx_vol_timing final {
    /** The time between two heartbeats. The service expires an adapter after 15 seconds. */
    std::chrono::milliseconds heartbeat_interval{5000};
    /** The time between two announces once one succeeded. */
    std::chrono::milliseconds reannounce_interval{60000};
    /** The time between two attempts while an announce or the universe fails. */
    std::chrono::milliseconds retry_interval{30000};
    /** The time between two refreshes: the day's fixings arrive once, so an hour is ample. */
    std::chrono::milliseconds refresh_interval{3600000};
    /** How long the adapter waits for the reply to an announce. */
    std::chrono::milliseconds reply_timeout{2000};
};

/**
 * @brief The derived source of FX volatility quotes as an adapter of the market data service.
 *
 * It reads its universe from the catalogue, the ECB fixings of each demanded pair through
 * the market data reader as the system party, and each pair's quoting convention from
 * refdata, and publishes the quotes the feed derives through the adapter's four messages.
 * It fetches nothing from outside: every input is already stored.
 */
class derived_fx_vol_adapter final {
private:
    static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.synthetic.service.derived_fx_vol_adapter");
        return instance;
    }

public:
    derived_fx_vol_adapter(ores::nats::service::client& nats,
                           ores::nats::service::nats_client& svc_nats,
                           ores::database::context db_ctx,
                           std::string source_name,
                           std::string provider_code,
                           derived_fx_vol_timing timing = {});
    ~derived_fx_vol_adapter();

    derived_fx_vol_adapter(const derived_fx_vol_adapter&) = delete;
    derived_fx_vol_adapter& operator=(const derived_fx_vol_adapter&) = delete;

    void start();
    void stop();

private:
    feed::derived_fx_readers make_readers();
    bool load_universe();
    bool announce();
    void demand_message(const ores::nats::message& msg);
    void serve(std::chrono::steady_clock::time_point now);
    void loop();

    ores::nats::service::client& nats_;
    ores::nats::service::nats_client& svc_nats_;
    ores::database::context db_ctx_;
    std::string source_name_;
    std::string provider_code_;
    derived_fx_vol_timing timing_;
    /** Guards the demand the NATS thread hands to the loop, and nothing else. */
    std::mutex mu_;
    /** Touched by the loop alone, but for its universe, which is set once before ready_. */
    feed::derived_fx_vol_feed feed_;
    std::optional<marketdata::messaging::adapter_demand_request> pending_;
    std::optional<std::string> system_party_;
    std::atomic<bool> stop_{false};
    std::atomic<bool> ready_{false};
    bool announced_ = false;
    std::uint64_t beats_ = 0;
    std::chrono::steady_clock::time_point next_universe_{};
    std::chrono::steady_clock::time_point next_announce_{};
    std::chrono::steady_clock::time_point next_heartbeat_{};
    std::chrono::steady_clock::time_point next_refresh_{};
    std::optional<ores::nats::service::subscription> demand_sub_;
    std::thread thread_;
};

}

#endif
