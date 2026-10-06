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
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/domain/feed_binding.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/classification/series_classifier.hpp"
#include "ores.marketdata.service/app/crm_ingest_bridge.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include <atomic>
#include <chrono>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <set>
#include <string>
#include <thread>
#include <tuple>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief The ingest loop: one subscription over every producer's ticks, and one
 * path for every tick, whatever its asset class.
 *
 * A producer publishes a market_tick on "synthetic.v1.tick.<source>". The tick
 * names its datum by its oresmd quote URI and its producer by its source; it
 * carries no owner. refresh() caches the enabled feed bindings by source, and
 * each binding of the tick's source is one consumer: the tick is stored as a
 * market_observation under that binding's tenant and party, and republished
 * on market_tick_subject() for that consumer and the datum's canonical ORE
 * key. A tick from a source with no enabled binding is dropped, with one
 * warning per source, and so is a tick whose URI names no datum or whose
 * value is not a number.
 *
 * A series a tick lands in for the first time is created and classified the
 * way the file import classifies it, from the datum. An FX spot tick is also
 * offered to the CRM bridge. Republish waits for the stored observation, so
 * the republished stream cannot diverge from the table.
 */
class ORES_MARKETDATA_SERVICE_EXPORT feed_ingest_loop {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.marketdata.service.app.feed_ingest_loop");
        return instance;
    }

public:
    /// @param crm_bridge Optional; when set, every stored FX spot tick is offered
    /// to it as a candidate driver update.
    feed_ingest_loop(ores::nats::service::client& nats,
                     ores::database::context ctx,
                     std::shared_ptr<crm_ingest_bridge> crm_bridge = nullptr);
    ~feed_ingest_loop();

    void start();
    void refresh();

private:
    void on_tick(const ores::nats::message& msg);

    /// Stores @p tick for the consumer @p binding names. Returns true when the
    /// observation was written, so the caller can gate the republish on it.
    bool persist(const domain::feed_binding& binding,
                 const datum::market_datum& tick_datum,
                 const messaging::market_tick& tick);

    /// The classifier for @p tenant_ctx's tenant, read once per refresh(). It is
    /// shared, so a refresh() on another thread cannot destroy it while in use.
    std::shared_ptr<const core::series_classifier>
    classifier_for(const ores::database::context& tenant_ctx);

    // One consumer: a source feeds many parties, each of which gets its own
    // observations and republish stream from the shared tick.
    struct binding_key {
        std::string source_name;
        std::string tenant_id;
        std::string party_id;

        bool operator<(const binding_key& other) const {
            return std::tie(source_name, tenant_id, party_id) <
                   std::tie(other.source_name, other.tenant_id, other.party_id);
        }
    };

    struct feed_stats {
        std::atomic<std::uint64_t> tick_count{0};
        std::atomic<std::chrono::system_clock::time_point::rep> last_tick_rep{
            std::chrono::system_clock::time_point::min().time_since_epoch().count()};
    };

    void status_loop();
    void log_status() const;

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::shared_ptr<crm_ingest_bridge> crm_bridge_;
    mutable std::mutex mu_;
    std::optional<ores::nats::service::subscription> tick_sub_;
    /// Enabled bindings by source, rebuilt by refresh().
    std::map<std::string, std::vector<domain::feed_binding>> bindings_by_source_;
    std::map<binding_key, std::shared_ptr<feed_stats>> stats_;
    /// Classifiers by tenant id, cleared by refresh().
    std::map<std::string, std::shared_ptr<const core::series_classifier>> classifiers_;
    /// Sources and URIs already reported as dropped, so each is reported once
    /// per refresh().
    std::set<std::string> unbound_warned_;
    std::set<std::string> unnameable_warned_;
    std::set<std::string> bad_value_warned_;

    static constexpr std::chrono::minutes status_interval_{1};
    std::atomic<bool> stop_flag_{false};
    std::thread status_thread_;
};

}

#endif
