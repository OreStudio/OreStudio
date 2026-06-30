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
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/fx_spot_tick_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <chrono>
#include <format>
#include <rfl/json.hpp>
#include <span>
#include <stdexcept>
#include <string>
#include <thread>

namespace ores::synthetic::service {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.service.fx_spot_feed");
    return instance;
}

} // namespace

fx_spot_feed::fx_spot_feed(ores::nats::service::client& nats,
                           ores::nats::service::nats_client& auth_nats,
                           std::string ore_key,
                           std::string nats_subject,
                           std::unique_ptr<ores::marketdata::domain::IStochasticProcess> process,
                           double ticks_per_hour,
                           boost::uuids::uuid series_id,
                           ores::utility::uuid::tenant_id tenant_id)
    : nats_(nats)
    , auth_nats_(auth_nats)
    , md_client_(auth_nats_)
    , ore_key_(std::move(ore_key))
    , process_(std::move(process))
    , ticks_per_hour_(ticks_per_hour)
    , nats_subject_(std::move(nats_subject))
    , series_id_(series_id)
    , tenant_id_(std::move(tenant_id)) {

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

    // Note: stop_flag_ is already false from the member initialiser. We must NOT
    // reset it here — a stop() that arrives between thread spawn and this point
    // would be clobbered, and the loop (and join()) would hang forever.

    // Sleep the tick period in small slices so stop() is observed promptly
    // (the period can be minutes; we must not block stop()/join() that long).
    constexpr auto slice = milliseconds(100);

    while (!stop_flag_.load(std::memory_order_relaxed)) {
        auto remaining = duration_cast<microseconds>(period_us);
        while (remaining.count() > 0 && !stop_flag_.load(std::memory_order_relaxed)) {
            const auto nap = remaining < slice ? remaining : duration_cast<microseconds>(slice);
            std::this_thread::sleep_for(nap);
            remaining -= nap;
        }

        if (stop_flag_.load(std::memory_order_relaxed))
            break;

        ores::marketdata::domain::fx_spot_tick tick;
        tick.ore_key = ore_key_;
        tick.datetime = system_clock::now();
        tick.mid = process_->next();

        on_tick(tick);

        const auto json = rfl::json::write(tick);
        const auto data = std::as_bytes(std::span{json.data(), json.size()});
        nats_.js_publish(nats_subject_, data);
        const auto n = publish_count_.fetch_add(1, std::memory_order_relaxed) + 1;
        if (n == 1 || n % 100 == 0) {
            BOOST_LOG_SEV(lg(), info)
                << "SYNTHETIC PUBLISH: subject='" << nats_subject_
                << "' ore_key='" << ore_key_
                << "' count=" << n << " mid=" << tick.mid;
        }

        // Step 3: persist each tick as a market_observation via marketdata service.
        ores::marketdata::domain::market_observation obs;
        obs.id = uuid_gen_();
        obs.tenant_id = tenant_id_;
        obs.series_id = series_id_;
        obs.observation_datetime = tick.datetime;
        obs.recorded_at = tick.datetime; // intentional: synthetic feed has no separate record lag
        obs.value = std::format("{:.6f}", tick.mid);
        obs.source = "SYNTHETIC";

        const auto saved = md_client_.save_observations({std::move(obs)});
        if (!saved) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to save observation: " << saved.error();
        }
    }
}

void fx_spot_feed::stop() {
    stop_flag_.store(true, std::memory_order_relaxed);
}

}
