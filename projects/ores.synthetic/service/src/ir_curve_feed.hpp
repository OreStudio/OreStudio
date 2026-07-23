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
#ifndef ORES_SYNTHETIC_SERVICE_IR_CURVE_FEED_HPP
#define ORES_SYNTHETIC_SERVICE_IR_CURVE_FEED_HPP

#include "ir_curve_template_resolver.hpp"
#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <atomic>
#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Concrete IR curve family feed: one short-rate process step fans out to N tenor ticks.
 *
 * Mirrors fx_spot_feed's fixed-mode tick clock, but where fx_spot_feed publishes one scalar per
 * step, ir_curve_feed publishes N (one per ir_curve_template_entry) — the "one process step -> N
 * tenor ticks" shape a curve needs (see the tick-batch-publishing task). Each generation step:
 *   1. Advances the short-rate process to get a new state.
 *   2. Derives every template entry's rate from that one state's discount_factor()s, via
 *      curve_instrument_pricer (deposit/FRA/par-rate solve, dispatched by curve_role) — so the
 *      whole batch is, by construction, a slice of one internally consistent latent curve.
 *   3. Publishes each entry as its own ir_curve_tick, all sharing one datetime, on the family
 *      subject (synthetic.v1.curve_family.<source>) — N individual NATS messages, not one
 *      aggregate payload (see the task's "what the wire format is not" analysis).
 *
 * Persistence is handled by ores.marketdata.service's curve_feed_ingest_loop, which subscribes to
 * the family wildcard and writes one market_observation row per tick. The synthetic service has
 * no marketdata writes.
 */
class ir_curve_feed final {
public:
    ir_curve_feed(ores::nats::service::client& nats,
                  ores::utility::uuid::tenant_id tenant_id,
                  boost::uuids::uuid party_id,
                  std::string source_name,
                  std::string nats_subject,
                  std::string series_type,
                  std::string metric,
                  std::string qualifier,
                  std::unique_ptr<ores::analytics::quant::domain::IYieldCurveProcess> process,
                  double ticks_per_hour,
                  std::vector<ir_curve_resolved_entry> entries);

    void start();
    void stop();
    std::uint64_t publish_count() const {
        return publish_count_.load(std::memory_order_relaxed);
    }
    const std::string& source_name() const {
        return source_name_;
    }
    /**
     * @brief The published market-data key (series_type/metric implied, currency_code +
     * strip_currency_prefix(currency_code, index_name)) -- the value curve_feed_controller checks
     * for cross-config collisions, since it is what every consumer actually looks up by, unlike
     * source_name (unique per config, not per market-data identity).
     */
    const std::string& qualifier() const {
        return qualifier_;
    }

private:
    ores::nats::service::client& nats_;
    ores::utility::uuid::tenant_id tenant_id_;
    boost::uuids::uuid party_id_;
    std::string source_name_;
    std::string nats_subject_;
    std::string series_type_;
    std::string metric_;
    std::string qualifier_;
    std::unique_ptr<ores::analytics::quant::domain::IYieldCurveProcess> process_;
    double ticks_per_hour_;
    std::vector<ir_curve_resolved_entry> entries_;
    std::atomic<bool> stop_flag_{false};
    std::atomic<std::uint64_t> publish_count_{0};
};

/**
 * @brief Resolves a config's Curve Template entries and constructs its ir_curve_feed, ready to
 * start() on its own thread. Shared by auto-start and the on-demand start control-plane so the
 * two paths can never drift (e.g. one lowercasing process_type and the other not).
 *
 * @throws std::invalid_argument if process_type/curve_role/tenor data is invalid (see resolve()
 * and process_factory::make_yield_curve_process()).
 */
std::shared_ptr<ir_curve_feed>
make_ir_curve_feed(ores::nats::service::client& nats,
                   const ores::synthetic::domain::ir_curve_generation_config& cfg,
                   const std::vector<ores::synthetic::domain::ir_curve_template_entry>& entries,
                   const ir_curve_refdata_context& refctx);

}

#endif
