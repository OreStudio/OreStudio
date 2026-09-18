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
#ifndef ORES_MARKETDATA_SERVICE_APP_FEED_INGEST_LOOP_HPP
#define ORES_MARKETDATA_SERVICE_APP_FEED_INGEST_LOOP_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/feed_binding.hpp"
#include "ores.marketdata.service/app/crm_ingest_bridge.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.ore.core/market/series_key_registry.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <atomic>
#include <chrono>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <thread>
#include <tuple>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief The single ingest loop: one wildcard subscription over the unified tick
 * subject scheme, dispatching on the subject's kind token only.
 *
 * Every tick arrives on "synthetic.v1.tick.<kind>.<source_name>" (see
 * ores.marketdata.api/domain/tick_subjects.hpp), so the kind is a property of the
 * subject and not of the payload. Nothing here branches on asset class: the loop
 * ingests whatever the producer published, and the asset class is data it reads
 * or is handed.
 *
 * fx_spot: the tick carries no series identity, so a feed_binding supplies it.
 * refresh() rebuilds a cache of enabled bindings keyed by source_name -- one
 * producer channel fans out to every (tenant, party, workspace) that consumes
 * it, and each consumer materializes its own observations and republish stream
 * from the shared tick. For every cached binding of the tick's source:
 *   1. The tick is persisted as a market_observation under that binding's party.
 *   2. It is re-published verbatim on the per-party subject
 *      "marketdata.v1.tick.<tenant>.<workspace>.<party>.<ore_key_subject>",
 *      which is the stream fx_spot_subscription and the chart consume.
 * A tick whose source has no enabled binding is dropped with a one-time warn.
 *
 * ir_curve: ir_curve_tick is fully self-describing (tenant, party, series
 * identity and point_id all travel on the wire); no binding is involved. One
 * observation is persisted per point_id and republished per party like FX.
 *
 * Republish is gated on a successful persist, so the republished stream cannot
 * diverge from the observations table.
 *
 * refresh() re-reads the bindings table and swaps the cache in. It is called by
 * the feed_binding NATS notify trigger handler on every change, and once at
 * start().
 */
class ORES_MARKETDATA_SERVICE_EXPORT feed_ingest_loop {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.marketdata.service.app.feed_ingest_loop");
        return instance;
    }

public:
    /// @param crm_bridge Optional; if set, every persisted fx_spot tick is also
    /// offered to the bridge as a candidate driver update (a no-op if the
    /// tick's (tenant, party) has no CRM configured, or the pair isn't
    /// one of its driver edges) -- see crm_ingest_bridge's own class doc.
    feed_ingest_loop(ores::nats::service::client& nats,
                     ores::database::context ctx,
                     std::shared_ptr<crm_ingest_bridge> crm_bridge = nullptr);
    ~feed_ingest_loop();

    void start();
    void refresh();

private:
    void on_tick(const ores::nats::message& msg);
    void ingest_ir_curve(const ores::nats::message& msg);
    /// Persists and republishes one fx_spot tick for every enabled binding of
    /// its source. Holds mu_ for the cache lookup only.
    void ingest_bound_tick(ores::nats::message msg, const std::string& source_name);
    /// Shared persistence for both tick kinds: resolve the market_series by its
    /// series identity, auto-creating it when missing, then write the
    /// observation row. Returns true when the observation was persisted, so
    /// callers can gate side effects (republish) on a durable write.
    /// asset_class and series_subclass are supplied by the caller -- the binding
    /// for a bound tick, the wire payload for a self-describing one; neither is
    /// inferred from the series_type. point_id is the caller's coordinate, or
    /// empty to take the series type's default, which is SPOT for an FX rate.
    bool persist_tick_observation(const ores::database::context& ctx,
                                  ores::utility::uuid::tenant_id tenant_id,
                                  const boost::uuids::uuid& party_id,
                                  const std::string& series_type,
                                  const std::string& metric,
                                  const std::string& qualifier,
                                  const std::string& asset_class,
                                  const std::string& series_subclass,
                                  std::chrono::system_clock::time_point datetime,
                                  const std::string& value,
                                  const std::string& source,
                                  const std::string& point_id);

    // Identity of one bound consumer: one per (source_name, tenant, party,
    // workspace). A single producer channel feeds many parties; each gets its
    // own observations and republish stream from the shared tick.
    struct binding_key {
        std::string source_name;
        std::string tenant_id;
        std::string party_id;
        std::string workspace_id;

        bool operator<(const binding_key& other) const {
            return std::tie(source_name, tenant_id, party_id, workspace_id) <
                   std::tie(other.source_name, other.tenant_id, other.party_id, other.workspace_id);
        }
    };

    void status_loop();
    void log_status() const;

    struct feed_stats {
        std::string series_identity;
        std::string nats_subject;
        std::string publish_subject;
        std::atomic<std::uint64_t> tick_count{0};
        std::atomic<std::chrono::system_clock::time_point::rep> last_tick_rep{
            std::chrono::system_clock::time_point::min().time_since_epoch().count()};
    };

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::shared_ptr<crm_ingest_bridge> crm_bridge_;
    mutable std::mutex mu_;
    /// The only subscription this loop makes; every kind arrives through it.
    std::optional<ores::nats::service::subscription> tick_sub_;
    /// The series key grammar, read once by start() before the subscription
    /// exists and never written again. A tick that names no point of its own
    /// takes its point from here, so no tick costs a database read.
    std::optional<ores::ore::market::series_key_registry> series_key_registry_;
    /// Enabled bindings by source_name, rebuilt by refresh(). A source with no
    /// entry is not consumed by anyone.
    std::map<std::string, std::vector<ores::marketdata::domain::feed_binding>> bindings_by_source_;
    std::map<binding_key, std::shared_ptr<feed_stats>> fx_stats_;
    /// Per-(kind token, source_name) stats for IR: the source comes from the
    /// wire, not from any binding.
    std::map<std::pair<std::string, std::string>, std::shared_ptr<feed_stats>> ir_stats_;
    std::set<std::string> unbound_warned_;

    static constexpr std::chrono::minutes status_interval_{1};
    std::atomic<bool> stop_flag_{false};
    std::thread status_thread_;
};

} // namespace ores::marketdata::service::app

#endif
