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
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/datum/ore_key_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include "ores.synthetic.api/feeds/producer_subject.hpp"
#include "ores.synthetic.api/feeds/vintage_lookup.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <random>
#include <stdexcept>

namespace ores::synthetic::feed {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.api.fx_spot_feed");
    return instance;
}

// Resolves the initial price from a real market_observation when cfg.price_source is "vintage",
// mirroring the deleted feed_controller::vintage_data_available() -- the config's own series
// (from its ore_key), keyed on (source=vintage_source, its own datum URI, date=vintage_date).
//
// @throws vintage_data_missing_error if no matching observation is found.
double resolve_vintage_initial_price(ores::nats::service::nats_client& auth_nats,
                                     const ores::synthetic::domain::fx_spot_generation_config& cfg,
                                     const std::string& caller_bearer_token) {
    const auto missing_message = "No vintage data found for source=" + cfg.vintage_source +
                                 ", date=" + cfg.vintage_date + ", datum=" + cfg.ore_key + ".";

    // The series and the row are found by the URIs the key's datum writes as: the
    // same codecs the marketdata service's ingest loop applies to a tick's key, so
    // the vintage read and the tick that follows it name one series.
    const auto datum = ores::marketdata::datum::ore_key_codec::read(cfg.ore_key);
    if (!datum)
        throw vintage_data_missing_error("ORE key '" + cfg.ore_key + "': " + datum.error());
    const auto datum_uri = ores::marketdata::datum::oresmd_uri_codec::write(*datum).value();
    const auto series_uri =
        ores::marketdata::datum::oresmd_uri_codec::write(ores::marketdata::datum::series_of(*datum))
            .value();

    const auto found = find_vintage_observation(auth_nats,
                                                caller_bearer_token,
                                                series_uri,
                                                datum_uri,
                                                cfg.vintage_source,
                                                cfg.vintage_date,
                                                missing_message,
                                                cfg.ore_key,
                                                boost::uuids::to_string(cfg.party_id));
    if (!found)
        throw vintage_data_missing_error(found.error());
    return *found;
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
    // separate temporaries).
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
                                          producer_subject(cfg.source_name, binding_mode),
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
    , clock_(ticks_per_hour)
    , nats_subject_(std::move(nats_subject)) {

    if (!process_)
        throw std::invalid_argument("fx_spot_feed: process must not be null");
    if (ticks_per_hour <= 0.0)
        throw std::invalid_argument("fx_spot_feed: ticks_per_hour must be positive");

    const auto datum = ores::marketdata::datum::ore_key_codec::read(ore_key_);
    if (!datum)
        throw std::invalid_argument("fx_spot_feed: '" + ore_key_ +
                                    "' names no datum: " + datum.error());
    oresmd_uri_ = ores::marketdata::datum::oresmd_uri_codec::write(*datum).value();

    // The qualifier is what follows the key's type and quote, such as EUR/USD.
    const auto first = ore_key_.find('/');
    const auto second = first == std::string::npos ? first : ore_key_.find('/', first + 1);
    if (second != std::string::npos)
        qualifier_ = ore_key_.substr(second + 1);
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
    clock_.run(
        [this] {
            ores::marketdata::messaging::market_tick tick;
            tick.oresmd_uri = oresmd_uri_;
            tick.value = std::format("{}", process_->next());
            tick.observation_time = std::chrono::system_clock::now();
            tick.source = source_name_;

            const auto& codec = ores::nats::default_wire_codec();
            BOOST_LOG_SEV(lg(), trace)
                << "Encoding market_tick for " << nats_subject_
                << ": wire_format="
                << (codec.format() == ores::nats::wire_format::msgpack ? "msgpack" : "json");
            nats_.js_publish(nats_subject_, codec.encode(tick));
            return tick.value;
        },
        [this](std::uint64_t n, const std::string& value) {
            if (n == 1 || n % 100 == 0) {
                BOOST_LOG_SEV(lg(), info)
                    << "SYNTHETIC PUBLISH: subject='" << nats_subject_ << "' ore_key='" << ore_key_
                    << "' count=" << n << " value=" << value;
            }
        },
        [this](const std::exception& ex) {
            BOOST_LOG_SEV(lg(), error) << "SYNTHETIC PUBLISH FAILED: subject='" << nats_subject_
                                       << "' ore_key='" << ore_key_ << "': " << ex.what();
        });
}

void fx_spot_feed::stop() {
    clock_.stop();
}

}
