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
#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.analytics.quant/service/tick_scheduler.hpp"
#include "ores.marketdata.api/domain/i_feed.hpp"
#include "ores.marketdata.api/domain/market_clock.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_process_parameter_value.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition.hpp"
#include "ores.synthetic.api/export.hpp"
#include "ores.synthetic.api/feeds/adapter_target.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <functional>
#include <map>
#include <memory>
#include <optional>
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
                  ores::analytics::quant::service::tick_scheduler& scheduler,
                  double step_speed,
                  double sensitivity = 1.0,
                  std::vector<ir_pillar_quote_target> adapter_targets = {});

    void start() override;
    /// One tick, as the scheduler drives it.
    std::string tick(const ores::analytics::quant::service::tick_context& context);
    void stop() override;
    std::uint64_t publish_count() const override {
        return published_.load(std::memory_order_relaxed);
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
    /// The subject the screen tails this feed's ticks on, beside the ingest subject.
    std::string display_subject_;
    std::string currency_code_;
    std::string role_;
    std::vector<std::string> series_uris_;
    std::vector<std::string> datum_uris_;
    std::unique_ptr<ores::analytics::quant::domain::i_yield_curve_process> process_;
    /// The simulated seconds one step of the process covers: the market speed the
    /// process was built with over the arrival rate. A later speed changes how many
    /// steps a tick covers, and not how long a step is.
    double step_seconds_;
    /// The row's arrival law, which the scheduler draws this series' gaps from.
    ores::analytics::quant::domain::arrival_spec arrival_;
    ores::analytics::quant::service::tick_scheduler& scheduler_;
    /// How strongly this feed follows the market.
    double sensitivity_;
    /// The series' registration in the scheduler, while it is registered.
    std::optional<std::size_t> handle_;
    /// Ticks this feed has published.
    std::atomic<std::uint64_t> published_{0};
    std::vector<ir_curve_resolved_entry> entries_;
    /// The adapter id of each entry, in the same order, or nothing for an entry that publishes a
    /// market tick. Empty for every entry when the feed serves no adapter.
    std::vector<std::optional<ir_pillar_quote_target>> adapter_targets_;
    /// The last adapter tick of each pillar, by its external id, so a repeated tick is the one the
    /// pillar made before.
    std::map<std::string, ores::marketdata::messaging::adapter_tick> previous_ticks_;
};

/**
 * @brief The simulated years one tick of a curve covers: the market speed over the
 * curve's rate.
 *
 * The rate decides how often a curve publishes and not how fast its calendar runs, so
 * a curve and the spot feeds beside it live in one simulated time. At the default speed
 * of 1440, a curve at one tick a minute steps a calendar day, as it always has.
 */
constexpr double curve_step_years(double market_speed, double ticks_per_hour) {
    constexpr double seconds_per_year = 365.0 * 24.0 * 60.0 * 60.0;
    return ticks_per_hour > 0.0 ? market_speed / (ticks_per_hour / 3600.0) / seconds_per_year :
                                  ir_curve_feed_dt;
}

/**
 * @brief Resolves a config's Curve Template entries and constructs its ir_curve_feed, ready to
 * start() on its own thread. Shared by auto-start and the on-demand start control-plane so the
 * two paths can never drift (e.g. one lowercasing process_type and the other not).
 *
 * The process is built from the config's row-based parameters: @p values carries the config's
 * {parameter_definition_id, value} rows (all rows for this config, grouped by the caller) and
 * @p definitions the system-tenant parameter-definitions catalogue; the two are joined and
 * validated by map_parameters_to_yield_curve_process(), which throws a clear message on
 * missing/unexpected/out-of-bounds parameters.
 *
 * The curve starts from the initial_rate parameter value as entered: "fixed" is its one price
 * source.
 *
 * The publish subject is derived from cfg.source_name via producer_subject() and is
 * "synthetic.v1.ops.tick.<source>".
 *
 * @throws std::invalid_argument if process_type/curve_role/tenor data is invalid (see resolve()
 * and map_parameters_to_yield_curve_process()).
 */
ORES_SYNTHETIC_API_EXPORT std::shared_ptr<ir_curve_feed> make_ir_curve_feed(
    ores::nats::service::client& nats,
    const ores::synthetic::domain::ir_curve_generation_config& cfg,
    const std::vector<ores::synthetic::domain::ir_curve_template_entry>& entries,
    const std::vector<ores::synthetic::domain::ir_curve_generation_config_process_parameter_value>&
        values,
    const std::vector<ores::synthetic::domain::yield_curve_process_parameter_definition>&
        definitions,
    const ir_curve_refdata_context& refctx,
    ores::analytics::quant::service::tick_scheduler& scheduler,
    std::vector<ir_pillar_quote_target> adapter_targets = {});

}
#endif
