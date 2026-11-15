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
#include "ores.http/routes/synthetic/feed_tick_buffers.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include <algorithm>
#include <charconv>
#include <cmath>
#include <set>
#include <utility>

namespace ores::http::routes::synthetic {

using ores::marketdata::messaging::market_tick;

feed_tick_buffers::feed_tick_buffers(subscriber subscribe)
    : subscribe_(std::move(subscribe)) {}

feed_tick_buffers::subscriber
feed_tick_buffers::nats_subscriber(ores::nats::service::nats_client& session) {
    return [&session](const std::string& subject, tick_handler on_tick) -> std::shared_ptr<void> {
        auto listening = session.transport().subscribe(subject, [on_tick](ores::nats::message msg) {
            const auto tick = ores::nats::default_wire_codec().decode<market_tick>(msg.data);
            if (tick)
                on_tick(*tick);
        });
        return std::make_shared<ores::nats::service::subscription>(std::move(listening));
    };
}

bool feed_tick_buffers::watch(const std::string& source_name) {
    std::lock_guard lock(mutex_);
    if (runs_.contains(source_name))
        return true;
    if (runs_.size() >= most_sources)
        return false;
    auto ticks = std::make_shared<run>();
    // The handler holds the run it fills, so a tick that arrives while the run
    // is being dropped writes to a buffer nobody reads rather than a freed one.
    auto stop =
        subscribe_(ores::marketdata::domain::feed_display_subject_for(source_name),
                   [ticks](const market_tick& tick) {
                       double value = 0.0;
                       const auto [end, error] = std::from_chars(
                           tick.value.data(), tick.value.data() + tick.value.size(), value);
                       if (error != std::errc{} || end != tick.value.data() + tick.value.size() ||
                           !std::isfinite(value))
                           return;
                       std::lock_guard held(ticks->mutex);
                       ticks->ticks.push_back({tick.observation_time, value, tick.oresmd_uri});
                       while (ticks->ticks.size() > capacity)
                           ticks->ticks.pop_front();
                   });
    runs_.emplace(source_name, heard{std::move(ticks), std::move(stop)});
    return true;
}

std::shared_ptr<feed_tick_buffers::run>
feed_tick_buffers::run_of(const std::string& source_name) const {
    std::lock_guard lock(mutex_);
    const auto found = runs_.find(source_name);
    return found == runs_.end() ? nullptr : found->second.ticks;
}

std::vector<feed_tick> feed_tick_buffers::ticks(const std::string& source_name,
                                                std::size_t limit) const {
    const auto heard = run_of(source_name);
    if (!heard)
        return {};
    std::lock_guard held(heard->mutex);
    const auto first = heard->ticks.size() > limit ? heard->ticks.size() - limit : 0;
    return {heard->ticks.begin() + static_cast<std::ptrdiff_t>(first), heard->ticks.end()};
}

std::vector<std::chrono::system_clock::time_point>
feed_tick_buffers::arrival_times(const std::string& source_name,
                                 std::chrono::system_clock::time_point since,
                                 std::size_t limit) const {
    const auto heard = run_of(source_name);
    if (!heard)
        return {};
    std::vector<std::chrono::system_clock::time_point> times;
    {
        std::lock_guard held(heard->mutex);
        for (const auto& tick : heard->ticks)
            if (tick.at >= since)
                times.push_back(tick.at);
    }
    if (times.size() > limit)
        times.erase(times.begin(), times.end() - static_cast<std::ptrdiff_t>(limit));
    return times;
}

void feed_tick_buffers::keep_only(const std::vector<std::string>& source_names) {
    const std::set<std::string, std::less<>> keep(source_names.begin(), source_names.end());
    std::vector<heard> dropped;
    {
        std::lock_guard lock(mutex_);
        for (auto it = runs_.begin(); it != runs_.end();) {
            if (keep.contains(it->first)) {
                ++it;
                continue;
            }
            dropped.push_back(std::move(it->second));
            it = runs_.erase(it);
        }
    }
    // A subscription waits for its handlers when it stops, so the dropped ones
    // stop here, outside the lock a read needs.
    dropped.clear();
}

std::size_t feed_tick_buffers::size() const {
    std::lock_guard lock(mutex_);
    return runs_.size();
}

}
