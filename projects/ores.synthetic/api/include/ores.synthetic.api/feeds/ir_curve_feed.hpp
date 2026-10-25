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
#ifndef ORES_SYNTHETIC_API_FEEDS_IR_CURVE_FEED_HPP
#define ORES_SYNTHETIC_API_FEEDS_IR_CURVE_FEED_HPP

#include "ir_curve_template_resolver.hpp"
#include "ores.analytics.quant/domain/arrival_spec.hpp"
#include "ores.analytics.quant/domain/i_arrival_process.hpp"
#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.analytics.quant/domain/market_link.hpp"
#include "ores.analytics.quant/service/tick_clock.hpp"
#include "ores.analytics.quant/service/tick_scheduler.hpp"
#include "ores.marketdata.api/domain/i_feed.hpp"
#include "ores.marketdata.api/domain/market_clock.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_process_parameter_value.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition.hpp"
#include "ores.synthetic.api/export.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <functional>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::feed {

/**
 * @brief The kind string this producer registers under in
 * make_default_feed_factory() — the asset-class discriminator of the factory
 * seam, exposed as i_feed::kind(). The control-plane passes it to
 * factory::make() to select this producer's builder and scopes running-feed
 * listings by it.
 */
inline constexpr std::string_view ir_curve_feed_kind = "ir_curve";

/**
 * @brief Thrown by make_ir_curve_feed() and make_fx_spot_feed() when cfg.price_source is
 * "vintage" and no matching market_observation is found for (vintage_source, vintage_date) --
 * on the config's anchor entry for curves, on its SPOT point for FX -- as an exception rather
 * than a result enum since the make functions already throw std::invalid_argument for other
 * construction failures (process_type/curve_role/tenor data).
 */
class ORES_SYNTHETIC_API_EXPORT vintage_data_missing_error final : public std::runtime_error {
public:
    explicit vintage_data_missing_error(const std::string& detail)
        : std::runtime_error(detail) {}
};

/**
 * @brief Concrete IR curve family feed: one short-rate process step fans out to N tenor ticks.
 *
 * Implements i_feed. Mirrors fx_spot_feed's fixed-mode tick clock, but where fx_spot_feed
 * publishes one scalar per step, ir_curve_feed publishes N (one per ir_curve_template_entry) —
 * the "one process step -> N tenor ticks" shape a curve needs (see the tick-batch-publishing
 * task). Each generation step:
 *   1. Advances the short-rate process to get a new state.
 *   2. Derives every template entry's rate from that one state's discount_factor()s, via
 *      curve_instrument_pricer (deposit/FRA/par-rate solve, dispatched by curve_role) — so the
 *      whole batch is, by construction, a slice of one internally consistent latent curve.
 *   3. Publishes each entry as its own market_tick, named by its pillar's oresmd quote URI and
 *      all sharing one observation time, on the producer subject for its source_name
 *      (synthetic.v1.ops.tick.<source>).
 *
 * Persistence is handled by ores.marketdata.service's feed_ingest_loop, which stores one
 * row of the tick table per tick for each consumer the source's feed bindings name. The synthetic
 * service has no marketdata writes.
 */
class ORES_SYNTHETIC_API_EXPORT ir_curve_feed final : public ores::marketdata::domain::i_feed {
public:
    ir_curve_feed(ores::nats::service::client& nats,
                  std::string source_name,
                  std::string nats_subject,
                  std::string currency_code,
                  std::string index_tenor,
                  std::string role,
                  std::unique_ptr<ores::analytics::quant::domain::i_yield_curve_process> process,
                  const ores::analytics::quant::domain::arrival_spec& arrival,
                  std::vector<ir_curve_resolved_entry> entries,
                  ores::analytics::quant::domain::market_link link = {},
                  ores::analytics::quant::service::tick_scheduler* scheduler = nullptr);

    void start() override;
    /// One tick, as the scheduler or the feed's own clock drives it.
    std::string tick(double elapsed_seconds);
    void stop() override;
    std::uint64_t publish_count() const override {
        return clock_.publish_count();
    }
    std::string_view kind() const override {
        return ir_curve_feed_kind;
    }
    const std::string& source_name() const override {
        return source_name_;
    }
    const std::string& nats_subject() const override {
        return nats_subject_;
    }
    /**
     * @brief The oresmd series the curve's pillars land in: one for the pillars that start at
     * spot and one for each later start date. Each is a term structure whose `tenor` is the
     * index tenor. Controllers refuse to run two feeds whose lists share a series, since both
     * would write points into it, whatever their roles.
     */
    const std::vector<std::string>& series_uris() const override {
        return series_uris_;
    }

    /**
     * @brief The use this curve is built for (discount, projection or both). The oresmd grammar
     * holds no role, so it is the config's own data and plays no part in which series the feed
     * owns.
     */
    const std::string& role() const override {
        return role_;
    }

private:
    ores::nats::service::client& nats_;
    std::string source_name_;
    std::string nats_subject_;
    std::string currency_code_;
    std::string role_;
    std::vector<std::string> series_uris_;
    std::vector<std::string> datum_uris_;
    std::unique_ptr<ores::analytics::quant::domain::i_yield_curve_process> process_;
    std::unique_ptr<ores::analytics::quant::domain::i_arrival_process> arrivals_;
    ores::analytics::quant::service::tick_clock clock_;
    /// The arrival rate in ticks per second, the unit a step is counted in and
    /// the one the arrival process itself divides by.
    double ticks_per_second_;
    /// The heat this feed adds to when it ticks, or null for a run with no
    /// endogenous factor.
    ores::analytics::quant::domain::shared_intensity* intensity_;
    /// The row's rate, which a scheduler needs to re-arm this series.
    ores::analytics::quant::domain::arrival_spec arrival_;
    /// The factors this feed follows.
    ores::analytics::quant::domain::market_link link_;
    /// The run's scheduler, or null when the feed runs its own clock.
    ores::analytics::quant::service::tick_scheduler* scheduler_;
    /// The series' registration in the scheduler, while it is registered.
    std::optional<std::size_t> handle_;
    std::vector<ir_curve_resolved_entry> entries_;
};

/**
 * @brief Resolves a config's Curve Template entries and constructs its ir_curve_feed, ready to
 * start() on its own thread. Shared by auto-start and the on-demand start control-plane so the
 * two paths can never drift (e.g. one lowercasing process_type and the other not) -- including
 * vintage resolution below, which both paths now go through identically.
 *
 * The process is built from the config's row-based parameters: @p values carries the config's
 * {parameter_definition_id, value} rows (all rows for this config, grouped by the caller) and
 * @p definitions the system-tenant parameter-definitions catalogue; the two are joined and
 * validated by map_parameters_to_yield_curve_process(), which throws a clear message on
 * missing/unexpected/out-of-bounds parameters.
 *
 * When cfg.price_source is "vintage", the process's initial_rate is resolved from a real
 * market_observation instead of the initial_rate parameter value: the resolved entries'
 * shortest-tenor DEPOSIT entry is looked up in market_observation by (cfg.vintage_source,
 * cfg.vintage_date, that entry's point_id), through @p auth_nats as the service itself. The
 * service's own identity belongs to the system tenant, so the lookup sees the system tenant's
 * observations and no tenant's. When cfg.price_source is "fixed" (the default), @p auth_nats is
 * unused and the initial_rate parameter value is used as-is.
 *
 * The publish subject is derived from cfg.source_name via producer_subject() and is
 * "synthetic.v1.ops.tick.<source>".
 *
 * @throws std::invalid_argument if process_type/curve_role/tenor data is invalid (see resolve()
 * and map_parameters_to_yield_curve_process()).
 * @throws vintage_data_missing_error if cfg.price_source is "vintage" and no matching observation
 * is found (or the config has no DEPOSIT entry to anchor on).
 */
ORES_SYNTHETIC_API_EXPORT std::shared_ptr<ir_curve_feed> make_ir_curve_feed(
    ores::nats::service::client& nats,
    ores::nats::service::nats_client& auth_nats,
    const ores::synthetic::domain::ir_curve_generation_config& cfg,
    const std::vector<ores::synthetic::domain::ir_curve_template_entry>& entries,
    const std::vector<ores::synthetic::domain::ir_curve_generation_config_process_parameter_value>&
        values,
    const std::vector<ores::synthetic::domain::yield_curve_process_parameter_definition>&
        definitions,
    const ir_curve_refdata_context& refctx,
    const ores::marketdata::domain::market_clock* clock = nullptr,
    ores::analytics::quant::domain::market_link link = {},
    ores::analytics::quant::service::tick_scheduler* scheduler = nullptr);

/**
 * @brief Picks the vintage-lookup anchor entry from a resolved Curve Template: the DEPOSIT entry
 * with the smallest ticks_ahead_end (the point instrument closest to an overnight/short rate —
 * the natural real-world analog to a short-rate model's initial_rate). Returns nullptr if @p
 * resolved has no DEPOSIT entry. Exposed separately from make_ir_curve_feed's vintage resolution
 * so the pure selection logic is unit-testable without a live market_data_client.
 */
ORES_SYNTHETIC_API_EXPORT const ir_curve_resolved_entry*
select_vintage_anchor_entry(const std::vector<ir_curve_resolved_entry>& resolved);

}
#endif
