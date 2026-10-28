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
#include "ores.marketdata.official/app/official_adapter.hpp"
#include "ores.marketdata.api/domain/adapter_subjects.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <span>
#include <utility>

namespace ores::marketdata::official::app {

using namespace ores::logging;
using clock = std::chrono::steady_clock;
namespace subjects = ores::marketdata::domain;

official_adapter::official_adapter(ores::nats::service::client& nats,
                                   std::vector<domain::hosted_source> sources,
                                   domain::frankfurter_fetcher& fetcher,
                                   int backfill_days,
                                   adapter_timing timing)
    : nats_(nats)
    , timing_(timing) {
    for (auto& source : sources)
        served_.push_back(std::make_unique<served>(
            std::make_unique<domain::source_feed>(std::move(source), fetcher, backfill_days)));
}

official_adapter::~official_adapter() {
    stop();
}

std::vector<ores::nats::service::subscription> official_adapter::subscribe() {
    std::vector<ores::nats::service::subscription> subs;
    for (auto& s : served_) {
        const auto subject = subjects::adapter_demand_subject(s->feed->source().source_name);
        BOOST_LOG_SEV(lg(), info) << "Serving source '" << s->feed->source().source_name
                                  << "' of provider '" << s->feed->source().provider_code
                                  << "' from Frankfurter '" << s->feed->source().frankfurter_key
                                  << "': demand on '" << subject << "'";
        auto* target = s.get();
        subs.push_back(nats_.subscribe(
            subject, [this, target](ores::nats::message msg) { on_demand(*target, msg); }));
    }
    return subs;
}

void official_adapter::start() {
    if (served_.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No source is configured, so the adapter serves none";
        return;
    }
    thread_ = std::thread(&official_adapter::loop, this);
}

void official_adapter::stop() {
    stop_.store(true, std::memory_order_relaxed);
    if (thread_.joinable())
        thread_.join();
}

void official_adapter::on_demand(served& s, const ores::nats::message& msg) {
    auto request =
        ores::nats::default_wire_codec().decode<messaging::adapter_demand_request>(msg.data);
    if (!request) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to decode a demand on '" << msg.subject
                                  << "': " << request.error().what();
        return;
    }
    // Until its universe is read, a source has not announced, and it rejects every id as not
    // ready rather than leave the service to read an empty answer.
    messaging::adapter_demand_response response;
    if (s.ready.load(std::memory_order_acquire)) {
        response = s.feed->answer(*request);
    } else {
        for (const auto& id : request->ids)
            response.rejected.push_back({id, "not_ready"});
    }
    {
        std::lock_guard lock(s.mu);
        s.pending = std::move(*request);
    }
    ores::service::messaging::reply(nats_, msg, response);
}

bool official_adapter::announce(served& s) {
    const auto& name = s.feed->source().source_name;
    try {
        const auto bytes = ores::nats::default_wire_codec().encode(s.feed->announcement());
        const auto reply = nats_.request_sync(subjects::adapter_announce_subject(),
                                              std::span<const std::byte>(bytes),
                                              {},
                                              timing_.reply_timeout);
        const auto response =
            ores::nats::default_wire_codec().decode<messaging::adapter_announce_response>(
                reply.data);
        if (!response || !response->success) {
            BOOST_LOG_SEV(lg(), warn)
                << "The service refused the announce of '" << name
                << "': " << (response ? response->message : response.error().what());
            return false;
        }
        BOOST_LOG_SEV(lg(), info) << "Announced '" << name << "': " << response->served_count
                                  << " served, " << response->unknown_count << " unknown";
        return true;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), debug)
            << "The announce of '" << name << "' got no answer: " << e.what();
        return false;
    }
}

bool official_adapter::serve(served& s, clock::time_point now, bool may_refresh) {
    const auto& name = s.feed->source().source_name;
    if (!s.ready.load(std::memory_order_relaxed)) {
        if (now < s.next_universe)
            return false;
        if (auto loaded = s.feed->load_universe(); !loaded) {
            BOOST_LOG_SEV(lg(), warn)
                << "Cannot read the universe of '" << name << "': " << loaded.error();
            s.next_universe = now + timing_.retry_interval;
            return false;
        }
        s.ready.store(true, std::memory_order_release);
    }

    if (now >= s.next_announce) {
        s.announced = announce(s);
        s.next_announce =
            now + (s.announced ? timing_.reannounce_interval : timing_.retry_interval);
    }
    if (!s.announced)
        return false;

    if (now >= s.next_heartbeat) {
        try {
            messaging::adapter_heartbeat beat;
            beat.adapter_id = name;
            beat.counter = ++s.beats;
            nats_.publish(subjects::adapter_heartbeat_subject(name),
                          ores::nats::default_wire_codec().encode(beat));
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to send the heartbeat of '" << name << "': " << e.what();
        }
        s.next_heartbeat = now + timing_.heartbeat_interval;
    }

    std::optional<messaging::adapter_demand_request> taken;
    {
        std::lock_guard lock(s.mu);
        taken.swap(s.pending);
    }
    if (taken) {
        s.feed->demand(*taken);
        s.next_refresh = now;
    }
    if (!may_refresh || now < s.next_refresh)
        return false;

    const auto today = std::chrono::year_month_day{
        std::chrono::floor<std::chrono::days>(std::chrono::system_clock::now())};
    // A publish that fails, such as on a lost connection, ends this refresh only: what was
    // not sent is sent again on the next.
    std::expected<std::size_t, std::string> sent = 0;
    try {
        sent = s.feed->refresh(today, [&](const messaging::adapter_tick& tick) {
            nats_.publish(subjects::adapter_ingress_subject(name, tick.external_id),
                          ores::nats::default_wire_codec().encode(tick));
        });
    } catch (const std::exception& e) {
        sent = std::unexpected(std::string(e.what()));
    }
    if (!sent) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to refresh '" << name << "': " << sent.error();
        s.next_refresh = now + timing_.retry_interval;
        return true;
    }
    if (*sent > 0)
        BOOST_LOG_SEV(lg(), info) << "Sent " << *sent << " fixing(s) of '" << name << "'";
    s.next_refresh = now + timing_.poll_interval;
    return true;
}

void official_adapter::loop() {
    constexpr auto slice = std::chrono::milliseconds(50);
    // Every source announces and beats on every pass, and one at most reads Frankfurter, so
    // a long backfill of one source never starves the heartbeats of the others.
    std::size_t turn = 0;
    while (!stop_.load(std::memory_order_relaxed)) {
        bool refreshed = false;
        for (std::size_t i = 0; i < served_.size(); ++i) {
            auto& s = *served_[(turn + i) % served_.size()];
            if (serve(s, clock::now(), !refreshed)) {
                refreshed = true;
                turn = (turn + i + 1) % served_.size();
            }
        }
        std::this_thread::sleep_for(slice);
    }
}

}
