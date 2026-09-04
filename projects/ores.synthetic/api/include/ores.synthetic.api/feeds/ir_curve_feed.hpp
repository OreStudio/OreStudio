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
#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.marketdata.api/domain/i_feed.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_process_parameter_value.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition.hpp"
#include "ores.synthetic.api/export.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <atomic>
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
 * seam, exposed as IFeed::kind(). The control-plane passes it to
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
 * Implements IFeed. Mirrors fx_spot_feed's fixed-mode tick clock, but where fx_spot_feed
 * publishes one scalar per step, ir_curve_feed publishes N (one per ir_curve_template_entry) —
 * the "one process step -> N tenor ticks" shape a curve needs (see the tick-batch-publishing
 * task). Each generation step:
 *   1. Advances the short-rate process to get a new state.
 *   2. Derives every template entry's rate from that one state's discount_factor()s, via
 *      curve_instrument_pricer (deposit/FRA/par-rate solve, dispatched by curve_role) — so the
 *      whole batch is, by construction, a slice of one internally consistent latent curve.
 *   3. Publishes each entry as its own ir_curve_tick, all sharing one datetime, on the unified
 *      tick subject (synthetic.v1.tick.ir_curve.<source>) — N individual NATS messages, not one
 *      aggregate payload (see the task's "what the wire format is not" analysis).
 *
 * Persistence is handled by ores.marketdata.service's feed_ingest_loop, which subscribes to the
 * unified tick wildcard, dispatches on the kind token and writes one market_observation row per
 * tick. The synthetic service has no marketdata writes.
 */
class ORES_SYNTHETIC_API_EXPORT ir_curve_feed final : public ores::marketdata::domain::IFeed {
public:
    ir_curve_feed(ores::nats::service::client& nats,
                  ores::utility::uuid::tenant_id tenant_id,
                  boost::uuids::uuid party_id,
                  std::string source_name,
                  std::string nats_subject,
                  std::string series_type,
                  std::string metric,
                  std::string qualifier,
                  std::string role,
                  std::unique_ptr<ores::analytics::quant::domain::IYieldCurveProcess> process,
                  double ticks_per_hour,
                  std::vector<ir_curve_resolved_entry> entries);

    void start() override;
    void stop() override;
    std::uint64_t publish_count() const override {
        return publish_count_.load(std::memory_order_relaxed);
    }
    std::string_view kind() const override {
        return ir_curve_feed_kind;
    }
    const std::string& source_name() const override {
        return source_name_;
    }
    /**
     * @brief The published market-data key (series_type/metric implied, ir_curve_qualifier(cfg))
     * -- the value controllers check for cross-config collisions (together with role()), since
     * it is what every consumer actually looks up by, unlike source_name (unique per config, not
     * per market-data identity).
     */
    const std::string& qualifier() const override {
        return qualifier_;
    }

    /**
     * @brief Whether this curve discounts, projects, or both (oresmd's curve_role) --
     * controllers only treat two feeds as conflicting when both qualifier() AND role() match, so
     * a discount curve and a projection curve for the same (currency_code, index_family, tenor)
     * can run side by side.
     */
    const std::string& role() const override {
        return role_;
    }

    std::string conflict_key() const override {
        return ores::marketdata::domain::feed_conflict_key(qualifier_, role_);
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
    std::string role_;
    std::unique_ptr<ores::analytics::quant::domain::IYieldCurveProcess> process_;
    double ticks_per_hour_;
    std::vector<ir_curve_resolved_entry> entries_;
    std::atomic<bool> stop_flag_{false};
    std::atomic<std::uint64_t> publish_count_{0};
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
 * cfg.vintage_date, that entry's point_id), via a market_data_client delegated with @p
 * caller_bearer_token so the lookup runs in the caller's own tenant/party context
 * (market_observation is tenant-scoped under RLS; this service's own service-account token is
 * bound to the system tenant). An empty caller_bearer_token falls back to @p auth_nats's own
 * (undelegated) identity -- e.g. auto-start, which has no end-user session. When cfg.price_source
 * is "fixed" (the default), @p auth_nats and @p caller_bearer_token are unused and the
 * initial_rate parameter value is used as-is.
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
    const std::string& caller_bearer_token = {});

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
