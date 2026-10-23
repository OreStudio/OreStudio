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
#ifndef ORES_SYNTHETIC_SERVICE_SIMULATOR_ADAPTER_HPP
#define ORES_SYNTHETIC_SERVICE_SIMULATOR_ADAPTER_HPP

#include "generator_runner.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include <atomic>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <thread>

namespace ores::synthetic::service {

/** The timing of an adapter, as a seam so a test runs in milliseconds. */
struct simulator_adapter_timing final {
    /** The time between two heartbeats. The service expires an adapter after 15 seconds. */
    std::chrono::milliseconds heartbeat_interval{5000};
    /** The time between two announces once one succeeded. Another service start needs one. */
    std::chrono::milliseconds reannounce_interval{60000};
    /** The time between two announces while none succeeded. */
    std::chrono::milliseconds retry_interval{3000};
    /** How long the adapter waits for the reply to an announce. */
    std::chrono::milliseconds reply_timeout{2000};
};

/**
 * @brief The simulator as an adapter of the market data service.
 *
 * The adapter announces its id and its universe, answers the demand set the service sends, and
 * sends a heartbeat. The runner starts the generators the demand names, and each generator
 * publishes its ticks on the ingress subject itself. The adapter id is the name of the source
 * the catalogue holds for the simulator, and its provider is the provider whose notation the
 * ticks use.
 *
 * The service may start after the adapter or restart while the adapter runs, so the adapter
 * announces again until one announce succeeds and then at a slow pace.
 */
class simulator_adapter final {
private:
    static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.synthetic.service.simulator_adapter");
        return instance;
    }

public:
    simulator_adapter(ores::nats::service::client& nats,
                      generator_runner& runner,
                      std::string adapter_id,
                      std::string provider_code,
                      simulator_adapter_timing timing = {});
    ~simulator_adapter();

    simulator_adapter(const simulator_adapter&) = delete;
    simulator_adapter& operator=(const simulator_adapter&) = delete;

    /** Subscribes to the demand subject and starts the announce and heartbeat thread. */
    void start();

    /** Stops the thread and the subscription. The generators stay with the runner. */
    void stop();

    /** What the adapter announces: its id, its provider and the ids it can serve. */
    [[nodiscard]] ores::marketdata::messaging::adapter_announce_request announcement() const;

    /** Answers a demand set. This is what the subscription does with each message. */
    [[nodiscard]] ores::marketdata::messaging::adapter_demand_response
    on_demand(const ores::marketdata::messaging::adapter_demand_request& request);

    /** Whether the last announce succeeded. */
    [[nodiscard]] bool announced() const {
        return announced_.load(std::memory_order_relaxed);
    }

private:
    bool announce();
    void demand_message(const ores::nats::message& msg);
    void loop();

    ores::nats::service::client& nats_;
    generator_runner& runner_;
    std::string adapter_id_;
    std::string provider_code_;
    simulator_adapter_timing timing_;
    std::atomic<bool> stop_{false};
    std::atomic<bool> announced_{false};
    std::uint64_t counter_ = 0;
    std::optional<ores::nats::service::subscription> demand_sub_;
    std::thread thread_;
};

}

#endif
