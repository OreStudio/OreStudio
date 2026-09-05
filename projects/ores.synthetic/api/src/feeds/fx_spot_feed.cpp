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
#include "ores.synthetic.api/feeds/fx_spot_feed.hpp"
#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/fx_spot_tick_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <random>
#include <stdexcept>
#include <thread>

namespace ores::synthetic::feed {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.api.fx_spot_feed");
    return instance;
}

// ISO date part of an observation_datetime -- the same one-liner duplicated in
// ir_curve_feed.cpp (the two live in the same component but keep their own copy
// rather than share a header for a one-line helper).
std::string date_part(std::chrono::system_clock::time_point tp) {
    const auto days = std::chrono::floor<std::chrono::days>(tp);
    return std::format("{:%F}", days);
}

// Resolves the initial price from a real market_observation when cfg.price_source is "vintage",
// mirroring the deleted feed_controller::vintage_data_available() -- the config's own series
// (from its ore_key), keyed on (source=vintage_source, point_id="SPOT", date=vintage_date).
//
// @throws vintage_data_missing_error if no matching observation is found.
double resolve_vintage_initial_price(ores::nats::service::nats_client& auth_nats,
                                     const ores::synthetic::domain::fx_spot_generation_config& cfg,
                                     const std::string& caller_bearer_token) {
    const auto missing_message = [&] {
        return "No vintage data found for source=" + cfg.vintage_source +
               ", date=" + cfg.vintage_date + ", point_id=SPOT.";
    };

    const auto key =
        ores::marketdata::core::oresmd_projections::split_market_series_key(cfg.ore_key);
    if (!key)
        throw vintage_data_missing_error("Cannot parse ORE key '" + cfg.ore_key + "'.");

    auto delegated_nats = auth_nats.with_delegation(caller_bearer_token);
    ores::marketdata::client::market_data_client md_client(delegated_nats);

    auto series = md_client.find_series(
        key->series_type, key->metric, key->qualifier, boost::uuids::to_string(cfg.party_id));
    if (!series)
        throw vintage_data_missing_error("Failed to look up series for '" + cfg.ore_key +
                                         "': " + series.error());
    if (!series->has_value())
        throw vintage_data_missing_error(missing_message());

    // Paged scan -- see ir_curve_feed.cpp's resolve_vintage_initial_rate() for why an unbounded
    // fetch is unsafe here (NATS max payload).
    constexpr std::uint32_t page_size = 200;
    const auto series_id_str = boost::uuids::to_string((*series)->id);
    std::uint32_t offset = 0;
    for (;;) {
        auto observations = md_client.list_observations_page(series_id_str, offset, page_size);
        if (!observations) {
            throw vintage_data_missing_error("Failed to look up observations for '" + cfg.ore_key +
                                             "': " + observations.error());
        }
        for (const auto& obs : *observations) {
            if (obs.source == cfg.vintage_source && obs.point_id == "SPOT" &&
                date_part(obs.observation_datetime) == cfg.vintage_date) {
                try {
                    return std::stod(obs.value);
                } catch (const std::exception& e) {
                    throw vintage_data_missing_error("Vintage observation value '" + obs.value +
                                                     "' is not a valid number: " + e.what());
                }
            }
        }
        if (observations->size() < page_size)
            break;
        offset += page_size;
    }
    throw vintage_data_missing_error(missing_message());
}

} // namespace

ORES_SYNTHETIC_API_EXPORT std::shared_ptr<fx_spot_feed>
make_fx_spot_feed(ores::nats::service::client& nats,
                  ores::nats::service::nats_client& auth_nats,
                  const ores::synthetic::domain::fx_spot_generation_config& cfg,
                  const std::vector<ores::synthetic::domain::gmm_component>& components,
                  ores::synthetic::domain::binding_mode binding_mode,
                  const std::string& caller_bearer_token) {
    if (components.empty())
        throw std::invalid_argument("make_fx_spot_feed: config '" + cfg.ore_key +
                                    "' has no GMM components.");

    std::vector<double> means, stdevs, weights;
    means.reserve(components.size());
    stdevs.reserve(components.size());
    weights.reserve(components.size());
    for (const auto& c : components) {
        means.push_back(c.mean);
        stdevs.push_back(c.stdev);
        weights.push_back(c.weight);
    }

    // "vintage" resolves the initial price from a real market_observation, overriding the stored
    // gmm_initial_price (0 for vintage configs, per the check constraint); "fixed" uses the
    // stored value as-is. See make_fx_spot_feed's doc comment for the vintage semantics.
    const double initial_price =
        cfg.price_source == "vintage" ?
            resolve_vintage_initial_price(auth_nats, cfg, caller_bearer_token) :
            cfg.gmm_initial_price;

    // Persistent random_device so the OS entropy pool is not re-seeded between rapid
    // successive calls (which can produce equal values on some platforms when called on
    // separate temporaries) -- same note as feed_controller::start().
    static std::random_device rd;
    const std::uint32_t seed = rd();
    BOOST_LOG_SEV(lg(), ores::logging::info)
        << "SYNTHETIC SEED: source='" << cfg.source_name << "' seed=" << seed;

    auto process =
        ores::analytics::quant::service::process_factory::make_process(cfg.process_type,
                                                                       std::move(means),
                                                                       std::move(stdevs),
                                                                       std::move(weights),
                                                                       initial_price,
                                                                       seed);

    return std::make_shared<fx_spot_feed>(nats,
                                          cfg.ore_key,
                                          cfg.source_name,
                                          synthetic_producer_subject(cfg.source_name, binding_mode),
                                          std::move(process),
                                          static_cast<double>(cfg.ticks_per_hour));
}

fx_spot_feed::fx_spot_feed(
    ores::nats::service::client& nats,
    std::string ore_key,
    std::string source_name,
    std::string nats_subject,
    std::unique_ptr<ores::analytics::quant::domain::IStochasticProcess> process,
    double ticks_per_hour)
    : nats_(nats)
    , ore_key_(std::move(ore_key))
    , source_name_(std::move(source_name))
    , process_(std::move(process))
    , ticks_per_hour_(ticks_per_hour)
    , nats_subject_(std::move(nats_subject)) {

    if (!process_)
        throw std::invalid_argument("fx_spot_feed: process must not be null");
    if (ticks_per_hour_ <= 0.0)
        throw std::invalid_argument("fx_spot_feed: ticks_per_hour must be positive");

    if (const auto key =
            ores::marketdata::core::oresmd_projections::split_market_series_key(ore_key_);
        key) {
        qualifier_ = key->qualifier;
    }
}

const std::string& fx_spot_feed::source_name() const {
    return source_name_;
}

const std::string& fx_spot_feed::qualifier() const {
    return qualifier_;
}

const std::string& fx_spot_feed::role() const {
    static const std::string empty;
    return empty;
}

std::string fx_spot_feed::conflict_key() const {
    return ores::marketdata::domain::feed_conflict_key(qualifier_, role());
}

void fx_spot_feed::start() {
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

        // A tick-loop thread has no caller to propagate an exception to -- an uncaught throw
        // here would std::terminate() the whole service process, taking down every other feed
        // and NATS handler with it. Log and skip this tick instead; the next tick tries again.
        try {
            const auto& codec = ores::nats::default_wire_codec();
            BOOST_LOG_SEV(lg(), trace)
                << "Encoding fx_spot_tick for " << nats_subject_ << ": wire_format="
                << (codec.format() == ores::nats::wire_format::msgpack ? "msgpack" : "json");
            nats_.js_publish(nats_subject_, codec.encode(tick));
            const auto n = publish_count_.fetch_add(1, std::memory_order_relaxed) + 1;
            if (n == 1 || n % 100 == 0) {
                BOOST_LOG_SEV(lg(), info)
                    << "SYNTHETIC PUBLISH: subject='" << nats_subject_ << "' ore_key='" << ore_key_
                    << "' count=" << n << " mid=" << tick.mid;
            }
        } catch (const std::exception& ex) {
            BOOST_LOG_SEV(lg(), error) << "SYNTHETIC PUBLISH FAILED: subject='" << nats_subject_
                                       << "' ore_key='" << ore_key_ << "': " << ex.what();
        }
    }
}

void fx_spot_feed::stop() {
    stop_flag_.store(true, std::memory_order_relaxed);
}

}
