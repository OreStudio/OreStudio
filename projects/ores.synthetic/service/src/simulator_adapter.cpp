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
namespace simulator_adapter_messaging = ores::marketdata::messaging;
namespace domain = ores::marketdata::domain;

simulator_adapter::simulator_adapter(
    ores::nats::service::client& nats,
    generator_runner& runner,
    std::string adapter_id,
    std::string provider_code,
    simulator_adapter_timing timing,
    std::shared_ptr<const ores::synthetic::feed::fault_state> faults)
    : nats_(nats)
    , runner_(runner)
    , adapter_id_(std::move(adapter_id))
    , provider_code_(std::move(provider_code))
    , timing_(timing)
    , faults_(std::move(faults)) {}

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

simulator_adapter_messaging::adapter_announce_request simulator_adapter::announcement() const {
    simulator_adapter_messaging::adapter_announce_request request;
    request.adapter_id = adapter_id_;
    request.provider_code = provider_code_;
    request.universe = runner_.universe();
    return request;
}

simulator_adapter_messaging::adapter_demand_response
simulator_adapter::on_demand(const simulator_adapter_messaging::adapter_demand_request& request) {
    simulator_adapter_messaging::adapter_demand_response response;
    // A not entitled rejection runs no generator and rejects every id.
    if (faults().not_entitled) {
        runner_.apply({});
        for (const auto& id : request.ids)
            response.rejected.push_back(
                simulator_adapter_messaging::rejected_id{id, "not_entitled"});
        return response;
    }
    const auto outcome = runner_.apply(request.ids);
    response.accepted = outcome.accepted;
    response.rejected = outcome.rejected;
    return response;
}

void simulator_adapter::demand_message(const ores::nats::message& msg) {
    // A lost connection answers nothing, so the service sees a demand that did not arrive.
    if (faults().connection_lost)
        return;
    auto request = ores::nats::default_wire_codec()
                       .decode<simulator_adapter_messaging::adapter_demand_request>(msg.data);
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
            ores::nats::default_wire_codec()
                .decode<simulator_adapter_messaging::adapter_announce_response>(reply.data);
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
    ores::synthetic::feed::fault_plan seen = faults();
    while (!stop_.load(std::memory_order_relaxed)) {
        // A change of the plan announces the adapter again, so the service sends the demand
        // set again. A generator that a fault stopped starts again without the author's help.
        if (const auto plan = faults(); !(plan == seen)) {
            seen = plan;
            next_announce = clock::now();
        }
        // A lost connection sends nothing, so the service makes the adapter stale. The
        // announce and the heartbeat resume when the fault is off.
        if (faults().connection_lost) {
            std::this_thread::sleep_for(slice);
            continue;
        }
        if (clock::now() >= next_announce) {
            const auto ok = announce();
            announced_.store(ok, std::memory_order_relaxed);
            next_announce =
                clock::now() + (ok ? timing_.reannounce_interval : timing_.retry_interval);
        }
        if (announced() && clock::now() >= next_heartbeat) {
            try {
                simulator_adapter_messaging::adapter_heartbeat beat;
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
