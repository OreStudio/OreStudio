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
#include "simulator_adapter.hpp"
#include "ores.marketdata.api/domain/adapter_subjects.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <span>

namespace ores::synthetic::service {

using namespace ores::logging;
namespace messaging = ores::marketdata::messaging;
namespace domain = ores::marketdata::domain;

simulator_adapter::simulator_adapter(ores::nats::service::client& nats,
                                     generator_runner& runner,
                                     std::string adapter_id,
                                     std::string provider_code,
                                     simulator_adapter_timing timing)
    : nats_(nats)
    , runner_(runner)
    , adapter_id_(std::move(adapter_id))
    , provider_code_(std::move(provider_code))
    , timing_(timing) {}

simulator_adapter::~simulator_adapter() {
    stop();
}

void simulator_adapter::start() {
    const auto subject = domain::adapter_demand_subject(adapter_id_);
    BOOST_LOG_SEV(lg(), info) << "Starting adapter '" << adapter_id_ << "' of provider '"
                              << provider_code_ << "': demand on '" << subject << "'";
    demand_sub_ =
        nats_.subscribe(subject, [this](ores::nats::message msg) { demand_message(msg); });
    thread_ = std::thread(&simulator_adapter::loop, this);
}

void simulator_adapter::stop() {
    stop_.store(true, std::memory_order_relaxed);
    demand_sub_.reset();
    if (thread_.joinable())
        thread_.join();
}

messaging::adapter_announce_request simulator_adapter::announcement() const {
    messaging::adapter_announce_request request;
    request.adapter_id = adapter_id_;
    request.provider_code = provider_code_;
    request.universe = runner_.universe();
    return request;
}

messaging::adapter_demand_response
simulator_adapter::on_demand(const messaging::adapter_demand_request& request) {
    const auto outcome = runner_.apply(request.ids);
    messaging::adapter_demand_response response;
    response.accepted = outcome.accepted;
    response.rejected = outcome.rejected;
    return response;
}

void simulator_adapter::demand_message(const ores::nats::message& msg) {
    auto request =
        ores::nats::default_wire_codec().decode<messaging::adapter_demand_request>(msg.data);
    if (!request) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to decode a demand on '" << msg.subject
                                  << "': " << request.error().what();
        return;
    }
    ores::service::messaging::reply(nats_, msg, on_demand(*request));
}

bool simulator_adapter::announce() {
    try {
        const auto request = announcement();
        const auto bytes = ores::nats::default_wire_codec().encode(request);
        const auto reply = nats_.request_sync(domain::adapter_announce_subject(),
                                              std::span<const std::byte>(bytes),
                                              {},
                                              timing_.reply_timeout);
        const auto response =
            ores::nats::default_wire_codec().decode<messaging::adapter_announce_response>(
                reply.data);
        if (!response) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to decode the announce reply: " << response.error().what();
            return false;
        }
        if (!response->success) {
            BOOST_LOG_SEV(lg(), warn) << "The service refused the announce of '" << adapter_id_
                                      << "': " << response->message;
            return false;
        }
        BOOST_LOG_SEV(lg(), info) << "Announced '" << adapter_id_ << "' with "
                                  << request.universe.size() << " id(s): " << response->served_count
                                  << " served, " << response->unknown_count << " unknown";
        return true;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), debug)
            << "The announce of '" << adapter_id_ << "' got no answer: " << e.what();
        return false;
    }
}

void simulator_adapter::loop() {
    using clock = std::chrono::steady_clock;
    constexpr auto slice = std::chrono::milliseconds(50);
    auto next_announce = clock::now();
    auto next_heartbeat = clock::now();
    while (!stop_.load(std::memory_order_relaxed)) {
        const auto now = clock::now();
        if (now >= next_announce) {
            const auto ok = announce();
            announced_.store(ok, std::memory_order_relaxed);
            next_announce =
                clock::now() + (ok ? timing_.reannounce_interval : timing_.retry_interval);
        }
        if (announced() && now >= next_heartbeat) {
            try {
                messaging::adapter_heartbeat beat;
                beat.adapter_id = adapter_id_;
                beat.counter = ++counter_;
                nats_.publish(domain::adapter_heartbeat_subject(adapter_id_),
                              ores::nats::default_wire_codec().encode(beat));
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn) << "Failed to send a heartbeat: " << e.what();
            }
            next_heartbeat = clock::now() + timing_.heartbeat_interval;
        }
        std::this_thread::sleep_for(slice);
    }
}

}
