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

#include "ores.analytics.quant/domain/arrival_spec.hpp"
#include "ores.analytics.quant/domain/i_stochastic_process.hpp"
#include "ores.analytics.quant/service/tick_scheduler.hpp"
#include "ores.marketdata.api/domain/feed_start.hpp"
#include "ores.marketdata.api/domain/i_feed.hpp"
#include "ores.marketdata.api/domain/market_clock.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.synthetic.api/domain/fx_spot_fixing_item.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.api/export.hpp"
#include "ores.synthetic.api/feeds/adapter_faults.hpp"
#include "ores.synthetic.api/feeds/adapter_target.hpp"
#include "ores.synthetic.api/feeds/fixing_calibration.hpp"
#include <atomic>
#include <memory>
#include <optional>
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
inline constexpr std::string_view fx_spot_feed_kind = "fx_spot";

/**
 * @brief Concrete FX spot feed: fixed-mode tick clock + stochastic process.
 *
 * Implements i_feed. On start(), runs a tick loop on the calling thread
 * (caller must run it on a dedicated std::thread). Each tick:
 *   1. Advances the stochastic process to get a new price.
 *   2. Builds a market_tick: the datum's oresmd quote URI, the price as decimal
 *      text, utc now and the source.
 *   3. Publishes it to the NATS JetStream subject.
 *
 * Persistence is handled by the marketdata ingest loop, which subscribes to
 * the JetStream subject, lazily creates the market series on first tick, and
 * writes market observations. The synthetic service has no marketdata writes.
 *
 * The NATS publish subject is supplied by the caller (derived per-producer
 * from its source name) so that multiple producers for the same datum
 * publish on distinct subjects.
 *
 * The feed is built from the oresmd quote URI of the datum it produces. The
 * strict codec reads it at construction, and a URI that names no datum, or a
 * datum that is not an fx_spot quote, is refused. The feed's series_uris() is
 * the one series URI of that quote. The feed has no role -- its conflict key is
 * its series URI alone.
 */
class ORES_SYNTHETIC_API_EXPORT fx_spot_feed final : public ores::marketdata::domain::i_feed {
public:
    fx_spot_feed(ores::nats::service::client& nats,
                 std::string quote_uri,
                 std::string source_name,
                 std::string nats_subject,
                 std::unique_ptr<ores::analytics::quant::domain::i_stochastic_process> process,
                 const ores::analytics::quant::domain::arrival_spec& arrival,
                 ores::analytics::quant::service::tick_scheduler& scheduler,
                 double sensitivity = 1.0,
                 std::optional<adapter_target> adapter = std::nullopt);

    /**
     * @brief The oresmd quote URI this feed publishes ticks under, e.g.
     * "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD".
     * FX-specific; the common interface exposes series_uris().
     */
    const std::string& quote_uri() const {
        return quote_uri_;
    }

    const std::string& source_name() const override;
    const std::vector<std::string>& series_uris() const override;
    const std::string& role() const override;
    const std::string& nats_subject() const override {
        return nats_subject_;
    }
    std::string_view kind() const override {
        return fx_spot_feed_kind;
    }
    void start() override;
    /// One tick, as the scheduler drives it.
    std::string tick(const ores::analytics::quant::service::tick_context& context);
    void stop() override;
    std::uint64_t publish_count() const override {
        return published_.load(std::memory_order_relaxed);
    }
    /// Where the feed's starting value came from, or that it kept its fixed level.
    std::optional<ores::marketdata::domain::feed_start> start_source() const override {
        return start_source_;
    }
    void record_start_source(ores::marketdata::domain::feed_start s) {
        start_source_ = std::move(s);
    }

private:
    const std::string& published_subject() const {
        return adapter_ ? adapter_->subject : nats_subject_;
    }

    ores::nats::service::client& nats_;
    std::string quote_uri_;
    std::vector<std::string> series_uris_;
    std::string source_name_;
    std::unique_ptr<ores::analytics::quant::domain::i_stochastic_process> process_;
    /// The row's arrival law, which the scheduler draws this series' gaps from.
    ores::analytics::quant::domain::arrival_spec arrival_;
    ores::analytics::quant::service::tick_scheduler& scheduler_;
    /// How strongly this feed follows the market.
    double sensitivity_;
    /// The series' registration in the scheduler, while it is registered.
    std::optional<std::size_t> handle_;
    /// Ticks this feed has published.
    std::atomic<std::uint64_t> published_{0};
    /// The arrival rate in ticks per second, the unit a step is counted in.
    double ticks_per_second_;
    std::string nats_subject_;
    /// The subject the screen tails this feed's ticks on, beside the adapter ingress.
    std::string display_subject_;
    std::optional<adapter_target> adapter_;
    std::optional<ores::marketdata::messaging::adapter_tick> previous_tick_;
    ores::marketdata::domain::feed_start start_source_;
};

/**
 * @brief Constructs an FX spot feed from a persisted fx_spot_generation_config and its
 * gmm_component rows, ready to start() on its own thread -- the FX half of the factory seam,
 * mirroring make_ir_curve_feed for curves. The process is built from the components' means/
 * stdevs/weights via process_factory, drawing from a stream keyed by the run seed and the source
 * name, so a run's prices reproduce; the publish subject is derived
 * from the config's source_name via producer_subject().
 *
 * A config with price_source "fixed" starts from gmm_initial_price. One with "fixing" reads @p
 * fixing_items in rank order through @p auth_nats and starts from the first that has a fixing: the
 * latest on or before the date of @p clock, or on the date its policy names. An item that quotes
 * the pair the other way round is read as one over its value. The spread of that item's fixings
 * over its window, in hourly terms, sets the size of the mixture: every component's deviation is
 * scaled by one factor, so its shape is kept. When no item has a fixing, the feed keeps
 * gmm_initial_price and its configured mixture. The feed records where it started, which
 * start_source() reports. The fixings are read once, when the feed is built, so a feed that starts
 * again takes the newest.
 *
 * @throws std::invalid_argument if @p components is empty or cfg.ticks_per_hour is
 * non-positive. cfg.process_type is not validated here: it is forwarded to
 * process_factory::make_process(), which falls back to the geometric engine for unrecognised
 * values.
 */
ORES_SYNTHETIC_API_EXPORT std::shared_ptr<fx_spot_feed>
make_fx_spot_feed(ores::nats::service::client& nats,
                  ores::nats::service::nats_client& auth_nats,
                  const ores::synthetic::domain::fx_spot_generation_config& cfg,
                  const std::vector<ores::synthetic::domain::gmm_component>& components,
                  const std::vector<ores::synthetic::domain::fx_spot_fixing_item>& fixing_items,
                  ores::analytics::quant::service::tick_scheduler& scheduler,
                  std::optional<adapter_target> adapter = std::nullopt,
                  const ores::marketdata::domain::market_clock* clock = nullptr);

}
#endif
