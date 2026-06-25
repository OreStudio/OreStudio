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
#include "fx_spot_feed.hpp"
#include "ores.marketdata.api/domain/fx_spot_tick_json_io.hpp" // IWYU pragma: keep.
#include "ores.utility/rfl/reflectors.hpp"                    // IWYU pragma: keep.
#include <rfl/json.hpp>
#include <algorithm>
#include <chrono>
#include <span>
#include <stdexcept>
#include <string>
#include <thread>

namespace ores::synthetic::service {

namespace {

std::string to_nats_subject(const std::string& ore_key) {
    // "FX/RATE/EUR/USD" → "marketdata.v1.tick.fx.rate.eur.usd"
    std::string s = "marketdata.v1.tick.";
    s.reserve(s.size() + ore_key.size());
    for (char c : ore_key) {
        if (c == '/')
            s += '.';
        else
            s += static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
    }
    return s;
}

} // namespace

fx_spot_feed::fx_spot_feed(ores::nats::service::client& nats,
                           std::string ore_key,
                           std::unique_ptr<ores::marketdata::domain::IStochasticProcess> process,
                           double ticks_per_hour)
    : nats_(nats),
      ore_key_(std::move(ore_key)),
      process_(std::move(process)),
      ticks_per_hour_(ticks_per_hour),
      nats_subject_(to_nats_subject(ore_key_)) {

    if (!process_)
        throw std::invalid_argument("fx_spot_feed: process must not be null");
    if (ticks_per_hour_ <= 0.0)
        throw std::invalid_argument("fx_spot_feed: ticks_per_hour must be positive");
}

std::string fx_spot_feed::ore_key() const {
    return ore_key_;
}

void fx_spot_feed::start(handler on_tick) {
    using namespace std::chrono;

    const auto period_us =
        duration_cast<microseconds>(hours(1)) / static_cast<long long>(ticks_per_hour_);

    stop_flag_.store(false, std::memory_order_relaxed);

    while (!stop_flag_.load(std::memory_order_relaxed)) {
        std::this_thread::sleep_for(period_us);

        if (stop_flag_.load(std::memory_order_relaxed))
            break;

        ores::marketdata::domain::fx_spot_tick tick;
        tick.ore_key = ore_key_;
        tick.datetime = system_clock::now();
        tick.mid = process_->next();

        on_tick(tick);

        const auto json = rfl::json::write(tick);
        const auto data = std::as_bytes(std::span{json.data(), json.size()});
        nats_.publish(nats_subject_, data);
    }
}

void fx_spot_feed::stop() {
    stop_flag_.store(true, std::memory_order_relaxed);
}

std::string fx_spot_feed::derive_nats_subject(const std::string& ore_key) {
    return to_nats_subject(ore_key);
}

}
