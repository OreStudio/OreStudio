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
#ifndef ORES_MARKETDATA_SERVICE_APP_ADAPTER_SUPERVISOR_HPP
#define ORES_MARKETDATA_SERVICE_APP_ADAPTER_SUPERVISOR_HPP

#if defined(_MSC_VER)
#    pragma once
#endif

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.service/app/adapter_registry.hpp"
#include "ores.marketdata.service/app/demand_calculator.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include <atomic>
#include <chrono>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <set>
#include <string>
#include <thread>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief Tracks the adapters and sends each its demand set.
 *
 * The supervisor answers the announce of an adapter, checks its universe against the
 * catalogue, and records its heartbeats in an @c adapter_registry. A monitor thread makes an
 * adapter stale when its heartbeat stops. The supervisor sends the full demand set of an adapter
 * when the adapter becomes ready, when it returns from stale, and when a route, a subscription,
 * a source, a vendor instrument or a mapping changes. The set always replaces the last one.
 *
 * An adapter id is the name of a source in the catalogue. The demand set of an adapter is the
 * set of the source of that name, and an announce of an id that no source carries leaves the
 * adapter with an empty set.
 *
 * The state machine is in the registry and the demand set is computed by @c compute_demand(),
 * both tested without NATS. This class holds the subscriptions, the catalogue snapshot and the
 * timing.
 */
class ORES_MARKETDATA_SERVICE_EXPORT adapter_supervisor {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.marketdata.service.app.adapter_supervisor");
        return instance;
    }

public:
    adapter_supervisor(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::chrono::milliseconds heartbeat_timeout = adapter_registry::default_heartbeat_timeout);
    ~adapter_supervisor();

    adapter_supervisor(const adapter_supervisor&) = delete;
    adapter_supervisor& operator=(const adapter_supervisor&) = delete;

    /// Reads the catalogue, subscribes to the announce and heartbeat subjects and starts the
    /// monitor thread.
    void start();

    /// Reads the catalogue again and sends every adapter its demand set again.
    void refresh();

    /// The state of every adapter, in the order of their ids.
    [[nodiscard]] std::vector<messaging::adapter_status> status() const;

    /// Answers an announce. This is what the subscription does with each message.
    messaging::adapter_announce_response
    on_announce(const messaging::adapter_announce_request& request,
                std::chrono::system_clock::time_point now);

    /// Records a heartbeat and asks for the demand set again when the adapter has recovered.
    void on_heartbeat(const std::string& adapter_id,
                      std::uint64_t counter,
                      std::chrono::system_clock::time_point now);

    /// Computes the demand set of @p adapter_id and sends it, and records the reply.
    /// @return Whether the adapter answered. A failure is logged and the caller sends again.
    bool send_demand(const std::string& adapter_id);

private:
    struct snapshot;

    /// Owes @p adapter_id a demand set from @p from on.
    void owe_demand(const std::string& adapter_id,
                    std::chrono::steady_clock::time_point from = std::chrono::steady_clock::now());

    void announce_message(const ores::nats::message& msg);
    void heartbeat_message(const ores::nats::message& msg);
    void monitor_loop();
    std::shared_ptr<const snapshot> current() const;

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    adapter_registry registry_;
    mutable std::mutex mu_;
    std::shared_ptr<const snapshot> snapshot_;
    /// The adapters that are owed a demand set, with the time from which to send it.
    std::map<std::string, std::chrono::steady_clock::time_point> needs_demand_;
    /// The adapters whose heartbeat arrived before they announced themselves, reported once.
    std::set<std::string> unknown_reported_;
    std::atomic<bool> stop_{false};
    std::optional<ores::nats::service::subscription> announce_sub_;
    std::optional<ores::nats::service::subscription> heartbeat_sub_;
    std::thread monitor_;
};

}

#endif
