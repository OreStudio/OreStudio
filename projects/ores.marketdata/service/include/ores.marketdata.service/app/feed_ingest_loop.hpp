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
#include "ores.marketdata.api/domain/series_subclass.hpp"
#include "ores.marketdata.service/app/crm_ingest_bridge.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
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

namespace ores::marketdata::service::app {

/**
 * @brief The single ingest loop: per-party subscriptions for bound FX feeds, one
 * wildcard subscription for self-describing IR curves.
 *
 * FX: on refresh() every enabled feed_binding with asset_class = fx gets its own
 * subscription on "synthetic.v1.tick.fx_spot.<source_name>" (the kind-token
 * scheme of ores.marketdata.api/domain/tick_subjects.hpp), keyed by the full
 * (source_name, tenant, party, workspace) identity of the binding. One producer
 * channel fans out to every party that consumes it (JetStream delivers a copy
 * per subscription), and each party materializes its own observations and
 * republish stream from the shared tick. Each arriving fx_spot_tick is:
 *   1. Persisted as a market_observation under the subscription's party.
 *   2. Re-published verbatim on the per-party subject
 *      "marketdata.v1.tick.<tenant>.<workspace>.<party>.<ore_key_subject>",
 *      which is the stream fx_spot_subscription and the chart consume.
 *
 * IR: ir_curve_tick is fully self-describing (tenant, party, series identity and
 * point_id all travel on the wire); no binding is involved. A wildcard
 * subscription (synthetic.v1.tick.>) feeds the ir_curve branch, which persists
 * one observation per point_id and republishes per party like FX. fx_spot ticks
 * also arrive on the wildcard but are ignored there; unbound fx_spot sources
 * get a one-time warn.
 *
 * Republish is gated on a successful persist, so the republished stream cannot
 * diverge from the observations table.
 *
 * refresh() re-reads the bindings table and rebuilds the subscription set. It is
 * called by the feed_binding NATS notify trigger handler on every change.
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
    /// Shared persistence for both tick kinds: resolve the market_series by its
    /// series identity, auto-creating it when missing, then write the
    /// observation row. Returns true when the observation was persisted, so
    /// callers can gate side effects (republish) on a durable write.
    /// asset_class is derived from the lowercased series_type; series_subclass
    /// falls back to that same derivation when no wire value exists.
    bool persist_tick_observation(
        const ores::database::context& ctx,
        ores::utility::uuid::tenant_id tenant_id,
        const boost::uuids::uuid& party_id,
        const std::string& series_type,
        const std::string& metric,
        const std::string& qualifier,
        std::optional<ores::marketdata::domain::series_subclass> series_subclass,
        bool is_scalar,
        std::chrono::system_clock::time_point datetime,
        const std::string& value,
        const std::string& source,
        const std::string& point_id);

    // Identity of one FX ingest subscription: one per (source_name, tenant,
    // party, workspace). A single producer channel feeds many parties; each
    // gets its own subscription so it materializes its own observations and
    // republish stream from the shared tick.
    struct subscription_key {
        std::string source_name;
        std::string tenant_id;
        std::string party_id;
        std::string workspace_id;

        bool operator<(const subscription_key& other) const {
            return std::tie(source_name, tenant_id, party_id, workspace_id) <
                   std::tie(other.source_name, other.tenant_id, other.party_id,
                            other.workspace_id);
        }
    };

    // Both called only from refresh(), which holds mu_.
    void subscribe_binding_locked(const subscription_key& key, const std::string& ore_key);
    void unsubscribe_binding_locked(const subscription_key& key);

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
    /// Wildcard subscription feeding the ir_curve branch; fx_spot ticks it also
    /// receives are ignored by on_tick.
    std::optional<ores::nats::service::subscription> ir_sub_;
    std::map<subscription_key, ores::nats::service::subscription> subs_;
    std::map<subscription_key, std::shared_ptr<feed_stats>> fx_stats_;
    /// Per-(kind token, source_name) stats for IR: the source comes from the
    /// wire, not from any binding.
    std::map<std::pair<std::string, std::string>, std::shared_ptr<feed_stats>> ir_stats_;
    /// Source names with at least one enabled FX binding; used to warn about
    /// fx_spot ticks on the wildcard for unbound sources.
    std::set<std::string> bound_sources_;
    std::set<std::string> unbound_warned_;

    static constexpr std::chrono::minutes status_interval_{1};
    std::atomic<bool> stop_flag_{false};
    std::thread status_thread_;
};

} // namespace ores::marketdata::service::app

#endif
