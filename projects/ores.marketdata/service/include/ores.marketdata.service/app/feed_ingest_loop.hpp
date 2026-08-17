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
#include "ores.marketdata.service/app/crm_ingest_bridge.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
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

namespace ores::marketdata::service::app {

/**
 * @brief The single ingest loop: one wildcard subscription on the unified tick
 * scheme (synthetic.v1.tick.>) persists every arriving tick as a market_observation.
 *
 * The kind token in the subject (the factory kind string — see
 * ores.marketdata.api/domain/tick_subjects.hpp) selects the per-kind path:
 *
 * - fx_spot ticks are bound-feed ticks: identity (tenant, party, oresmd URI)
 *   comes from the feed_binding cache, not from the wire (fx_spot_tick is not
 *   self-describing). A tick whose source has no enabled binding is dropped
 *   with a one-time warn.
 * - ir_curve ticks are fully self-describing (tenant, party, series identity and
 *   point_id all travel on the wire); no binding is involved.
 *
 * Every persisted tick is republished verbatim on
 * "marketdata.v1.tick.<tenant>.<series>", the scheme existing consumers
 * (fx_spot_subscription, chart) subscribe to. The subject is the ORE-shaped
 * forward projection of the series' oresmd URI (quote key or index name) — the
 * wire goes oresmd-native in a later story step. Republish is gated on a
 * successful persist, so the republished stream cannot diverge from the
 * observations table.
 *
 * refresh() re-reads the bindings table and rebuilds the cache. It is called by
 * the feed_binding NATS notify trigger handler on every change.
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
    void ingest_fx_spot(const ores::nats::message& msg, std::string_view source_name);
    void ingest_ir_curve(const ores::nats::message& msg);
    /// Shared persistence for both tick kinds: resolve the market_series by its
    /// oresmd URI, auto-creating it when missing, then write the observation
    /// row. Returns true when the observation was persisted, so callers can
    /// gate side effects (republish) on a durable write.
    bool persist_tick_observation(
        const ores::database::context& ctx,
        ores::utility::uuid::tenant_id tenant_id,
        const boost::uuids::uuid& party_id,
        const std::string& oresmd_uri,
        std::chrono::system_clock::time_point datetime,
        const std::string& value,
        const std::string& source,
        const std::string& point_id);
    void status_loop();
    void log_status() const;

    /// Identity a bound fx_spot tick needs; nothing on the wire supplies it.
    struct binding_record {
        std::string oresmd_uri;
        std::string tenant_id_str;
        boost::uuids::uuid party_id;
        std::string publish_subject;
    };

    struct feed_stats {
        std::string series_identity;
        std::string nats_subject;
        std::atomic<std::uint64_t> tick_count{0};
        std::atomic<std::chrono::system_clock::time_point::rep> last_tick_rep{
            std::chrono::system_clock::time_point::min().time_since_epoch().count()};
    };

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::shared_ptr<crm_ingest_bridge> crm_bridge_;
    mutable std::mutex mu_;
    std::optional<ores::nats::service::subscription> sub_;
    /// Enabled feed_binding rows keyed by source_name; built by refresh().
    std::map<std::string, binding_record> bindings_;
    /// Sources already warned about as unbound, so a running feed doesn't spam.
    std::set<std::string> unbound_warned_;
    /// Per-(kind token, source_name) stats: the same source_name can exist in
    /// more than one feed kind, so the kind token is part of the key.
    std::map<std::pair<std::string, std::string>, std::shared_ptr<feed_stats>> stats_;
    /// Reused across ticks. The loop's single subscription dispatches callbacks
    /// serially, so one generator is safe.
    boost::uuids::random_generator uuid_gen_;

    static constexpr std::chrono::minutes status_interval_{1};
    std::atomic<bool> stop_flag_{false};
    std::thread status_thread_;
};

} // namespace ores::marketdata::service::app

#endif
