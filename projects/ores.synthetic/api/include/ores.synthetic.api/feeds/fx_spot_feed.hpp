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
#ifndef ORES_SYNTHETIC_API_FEEDS_FX_SPOT_FEED_HPP
#define ORES_SYNTHETIC_API_FEEDS_FX_SPOT_FEED_HPP

#include "ores.analytics.quant/domain/i_stochastic_process.hpp"
#include "ores.marketdata.api/domain/i_feed.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.synthetic.api/domain/binding_mode.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.api/export.hpp"
#include <atomic>
#include <cctype>
#include <memory>
#include <string>
#include <vector>

namespace ores::synthetic::feed {

/**
 * @brief Build the producer subject from source_name and binding_mode. '.'
 * is kept (it is the NATS hierarchy separator and source names are dotted),
 * but any character that is not a safe subject token — whitespace, wildcards
 * ('*', '>'), or non-alphanumerics other than '.', '_', '-' — is replaced
 * with '_' so a stray value cannot produce surprise routing or a publish
 * error.
 *
 * sandboxed feeds publish under a distinct "synthetic.v1.sandbox.tick."
 * prefix rather than "synthetic.v1.tick." -- the marketdata ingest loop's
 * subscription subject is always derived from a feed_binding's source_name
 * as "synthetic.v1.tick." + source_name (see feed_ingest_loop.cpp), so a
 * sandboxed feed's ticks are structurally unreachable from the bound-feed
 * resolution path regardless of whether a feed_binding for this source_name
 * exists.
 */
inline std::string synthetic_producer_subject(const std::string& source_name,
                                              ores::synthetic::domain::binding_mode binding_mode) {
    std::string token;
    token.reserve(source_name.size());
    for (unsigned char c : source_name) {
        const bool safe = std::isalnum(c) || c == '.' || c == '_' || c == '-';
        token += safe ? static_cast<char>(c) : '_';
    }
    const bool sandboxed = binding_mode == ores::synthetic::domain::binding_mode::sandboxed;
    return sandboxed ? "synthetic.v1.sandbox.tick." + token : "synthetic.v1.tick." + token;
}

/**
 * @brief Concrete FX spot feed: fixed-mode tick clock + stochastic process.
 *
 * Implements IFeed. On start(), runs a tick loop on the calling thread
 * (caller must run it on a dedicated std::thread). Each tick:
 *   1. Advances the stochastic process to get a new price.
 *   2. Builds an fx_spot_tick (ore_key, utc now, price).
 *   3. Publishes the tick JSON to the NATS JetStream subject.
 *
 * Persistence is handled by the marketdata ingest loop, which subscribes to
 * the JetStream subject, lazily creates the market series on first tick, and
 * writes market observations. The synthetic service has no marketdata writes.
 *
 * The NATS publish subject is supplied by the caller (derived per-producer
 * from its source name) so that multiple producers for the same ORE key
 * publish on distinct subjects.
 *
 * The qualifier is derived from the ore_key at construction via
 * split_market_series_key (e.g. "FX/RATE/EUR/USD" -> "EUR/USD"); a key that
 * does not parse yields an empty qualifier. The feed has no role -- its
 * conflict key is its qualifier alone.
 */
class ORES_SYNTHETIC_API_EXPORT fx_spot_feed final : public ores::marketdata::domain::IFeed {
public:
    fx_spot_feed(ores::nats::service::client& nats,
                 std::string ore_key,
                 std::string source_name,
                 std::string nats_subject,
                 std::unique_ptr<ores::analytics::quant::domain::IStochasticProcess> process,
                 double ticks_per_hour);

    /**
     * @brief ORE canonical key this feed produces, e.g. "FX/RATE/EUR/USD".
     * FX-specific; the common interface exposes qualifier() instead.
     */
    std::string ore_key() const {
        return ore_key_;
    }

    const std::string& source_name() const override;
    const std::string& qualifier() const override;
    const std::string& role() const override;
    std::string conflict_key() const override;
    void start() override;
    void stop() override;
    std::uint64_t publish_count() const override {
        return publish_count_.load(std::memory_order_relaxed);
    }

private:
    ores::nats::service::client& nats_;
    std::string ore_key_;
    std::string source_name_;
    std::string qualifier_;
    std::unique_ptr<ores::analytics::quant::domain::IStochasticProcess> process_;
    double ticks_per_hour_;
    std::string nats_subject_;
    std::atomic<bool> stop_flag_{false};
    std::atomic<std::uint64_t> publish_count_{0};
};

/**
 * @brief Constructs an FX spot feed from a persisted fx_spot_generation_config and its
 * gmm_component rows, ready to start() on its own thread -- the FX half of the factory seam,
 * mirroring make_ir_curve_feed for curves. The process is built from the components' means/
 * stdevs/weights via process_factory, seeded from random_device; the publish subject is derived
 * from the config's source_name under @p binding_mode via synthetic_producer_subject().
 *
 * The config's "vintage" price_source is not resolved here: the FX vintage lookup (vintage_data_
 * available) lives in feed_controller::start()'s client-supplied-params path, which predates
 * this builder and is deleted by the per-config control-plane task. Until then the factory path
 * behaves like the auto-start path it replaces (gmm_initial_price as-is).
 *
 * @throws std::invalid_argument if @p components is empty or cfg.ticks_per_hour is non-positive.
 * cfg.process_type is not validated here: it is forwarded to process_factory::make_process(),
 * which falls back to the geometric engine for unrecognised values.
 */
ORES_SYNTHETIC_API_EXPORT std::shared_ptr<fx_spot_feed> make_fx_spot_feed(
    ores::nats::service::client& nats,
    const ores::synthetic::domain::fx_spot_generation_config& cfg,
    const std::vector<ores::synthetic::domain::gmm_component>& components,
    ores::synthetic::domain::binding_mode binding_mode =
        ores::synthetic::domain::binding_mode::bound);

}
#endif
