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
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/classification/series_classifier.hpp"
#include "ores.marketdata.service/app/crm_ingest_bridge.hpp"
#include "ores.marketdata.service/app/drop_oldest_queue.hpp"
#include "ores.marketdata.service/app/store_backoff.hpp"
#include "ores.marketdata.service/app/tick_age.hpp"
#include "ores.marketdata.service/app/tick_plan.hpp"
#include "ores.marketdata.service/app/tick_writer.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <atomic>
#include <chrono>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <set>
#include <span>
#include <string>
#include <thread>
#include <tuple>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief The series a tick creates when it lands in one for the first time.
 *
 * Identity and classification come from the datum; the producer kind comes
 * from the binding the tick arrived under, so a series a generated producer
 * creates says so while a real feed's keeps the default. A series already
 * present keeps the kind it was created with, which is why the caller builds
 * one of these only on the create path.
 */
ORES_MARKETDATA_SERVICE_EXPORT domain::market_series
make_feed_series(const domain::feed_binding& binding,
                 const boost::uuids::uuid& id,
                 const std::string& series_uri,
                 const std::string& series_subclass,
                 const std::string& service_account);

/**
 * @brief The dates one tick is recorded under: the day it was published, and the close
 * of the last completed business day.
 *
 * A report that values at the previous business day's close asks ORE for a market data
 * date, and ORE reads a time series by the date each observation carries. A live
 * producer holds only the value it has now, so a deployment whose feeds started today
 * would have no close for yesterday and every end-of-day report would fail on a
 * valuation date with no quotes. Recording each observation under the close as well
 * gives the series a valuation date's worth of history from the producer's first tick,
 * so a fresh environment can value a report at the close it values at without anyone
 * backfilling dates by hand.
 *
 * One business day is as far back as this reaches: one is what an end-of-day report
 * asks for, and a longer lookback would be history the producer has no state for. The
 * time of day is kept, so one date's rows stay in the order they arrived and a reader
 * that takes the last one takes the newest close. A producer that publishes at the
 * weekend records no second date, because the close already carries the last business
 * day's ticks.
 */
ORES_MARKETDATA_SERVICE_EXPORT std::vector<std::chrono::system_clock::time_point>
observation_dates(std::chrono::system_clock::time_point published_at);

/**
 * @brief The ingest loop: one subscription over every producer's ticks, and one
 * path for every tick, whatever its asset class.
 *
 * A producer publishes a market_tick on "synthetic.v1.ops.tick.<source>". The tick
 * names its datum by its oresmd quote URI and its producer by its source; it
 * carries no owner. refresh() caches the enabled feed bindings by source, and
 * each binding of the tick's source is one consumer: the tick is stored as a
 * market_observation under that binding's tenant and party, and republished
 * on market_tick_subject() for that consumer and the datum's canonical oresmd
 * URI. A tick from a source with no enabled binding is dropped, with one
 * warning per source, and so is a tick whose URI names no datum or whose
 * value is not a number.
 *
 * A series a tick lands in for the first time is created and classified the
 * way the file import classifies it, from the datum. An FX spot tick is also
 * offered to the CRM bridge.
 *
 * The subscription only decodes a tick and queues it. A worker thread takes
 * every tick that has arrived and stores them as one batch: one transaction for
 * each tenant, one series read for each series, and no commit for each tick.
 * Republish follows the commit of the batch that stored the tick, on core NATS
 * with no wait for an acknowledgement. A tick is perishable, because an old tick
 * that waits while a newer one arrives is worse than a tick that is lost. So the
 * queue is bounded and a full queue drops its oldest tick, and the worker drops a
 * tick that has aged past max_tick_age when it takes the tick. A batch takes about one
 * second, so a tick does not wait long inside a slow batch, and a tick that has aged past
 * the limit by the time its batch commits is stored and not republished.
 *
 * A store that does not answer does not stop the live path. The loop leaves the
 * store alone for a growing wait, and it still republishes the ticks it could not
 * store. A shutdown stops the subscription first, then works through the queued
 * ticks for a short time and drops the rest.
 *
 * @see The tick ingest pipeline (doc/knowledge/architecture/tick_ingest_pipeline.org)
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
    /// @param tick_writer Optional; when set, every stored tick is also copied to the tick
    /// table. The loop is its only caller.
    feed_ingest_loop(ores::nats::service::client& nats,
                     ores::database::context ctx,
                     std::shared_ptr<crm_ingest_bridge> crm_bridge = nullptr,
                     std::shared_ptr<tick_writer> tick_writer = nullptr);
    ~feed_ingest_loop();

    void start();
    void refresh();

    /**
     * @brief Stores @p tick for every cached binding of its source, and drops it
     * when the cache holds none.
     *
     * This is the whole of what the loop does with a tick once it has arrived,
     * so it is the entry point a test drives to reach the store without a live
     * subscription. The bindings cache it consults is what refresh() built.
     *
     * @return The reason the tick was dropped, or nothing when it was stored.
     */
    std::optional<tick_drop> handle_tick(const messaging::market_tick& tick);

    /**
     * @brief Stores @p ticks as one batch, the way the worker does.
     *
     * @return One entry for each tick: the reason it was dropped, or nothing when
     * it was accepted.
     */
    std::vector<std::optional<tick_drop>>
    handle_batch(const std::vector<messaging::market_tick>& ticks);

private:
    struct item;

    /// Whether a fresh transaction opens. It tells a store that is down from a row that
    /// the store refuses.
    [[nodiscard]] static bool store_reachable(const ores::database::context& tenant_ctx);

    void on_tick(const ores::nats::message& msg);
    void worker_loop();

    /// Plans, stores and republishes @p ticks. Fills @p outcomes when it is set.
    void process(const std::vector<messaging::market_tick>& ticks,
                 std::vector<std::optional<tick_drop>>* outcomes);

    /// Writes the rows of the items at @p indexes in one transaction of @p tenant_ctx.
    /// A batch that fails is split in two until the failing item is alone.
    void write_items(const ores::database::context& tenant_ctx,
                     std::vector<item>& items,
                     std::span<const std::size_t> indexes);

    /// Copies the ticks of the stored @p items to the tick table. A failure is logged and
    /// counted, and it does not change what the loop does with the tick. The context of a
    /// tenant in @p tenant_ctxs assigns the coordinate numbers of its series.
    void write_ticks(const std::vector<messaging::market_tick>& ticks,
                     const std::vector<item>& items,
                     const std::map<std::string, ores::database::context>& tenant_ctxs);

    /// The id of the series that @p tick_datum belongs to for @p binding's party,
    /// created and classified on the first tick that lands in it. Read once for
    /// each series and then cached until refresh().
    boost::uuids::uuid series_id_for(const ores::database::context& tenant_ctx,
                                     const domain::feed_binding& binding,
                                     const datum::market_datum& tick_datum,
                                     boost::uuids::random_generator& uuid_gen);

    void warn_dropped(const messaging::market_tick& tick, tick_drop reason);

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
        /// The age of the newest stored tick when its batch committed.
        std::atomic<std::int64_t> last_lag_ms{0};
    };

    void status_loop();
    void log_status() const;

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::shared_ptr<crm_ingest_bridge> crm_bridge_;
    std::shared_ptr<tick_writer> tick_writer_;
    /// Whether the last copy to the tick table failed.
    std::atomic<bool> tick_write_failing_{false};
    mutable std::mutex mu_;
    std::optional<ores::nats::service::subscription> tick_sub_;
    using bindings_map = std::map<std::string, std::vector<domain::feed_binding>>;
    /// Enabled bindings by source, rebuilt by refresh(). A batch reads one snapshot.
    std::shared_ptr<const bindings_map> bindings_by_source_ = std::make_shared<bindings_map>();
    /// Series ids by tenant, party and series URI, cleared by refresh().
    std::map<std::string, boost::uuids::uuid> series_ids_;
    std::map<binding_key, std::shared_ptr<feed_stats>> stats_;
    /// Classifiers by tenant id, cleared by refresh().
    std::map<std::string, std::shared_ptr<const core::series_classifier>> classifiers_;
    /// Sources and URIs already reported as dropped, so each is reported once
    /// per refresh().
    std::set<std::string> unbound_warned_;
    std::set<std::string> unnameable_warned_;
    std::set<std::string> bad_value_warned_;
    /// Coordinate numbers by series id and datum URI. A number never changes while its
    /// series exists, so refresh() keeps them.
    std::map<std::string, std::int16_t> coords_;

    static constexpr std::size_t queue_capacity = 65536;
    /// The most ticks one batch holds. A batch takes about target_batch_ms: the worker sizes
    /// the next batch from the speed of the last one, between min_batch_ticks and this limit.
    static constexpr std::size_t batch_limit = 20000;
    static constexpr std::size_t initial_batch_ticks = 500;
    static constexpr std::size_t min_batch_ticks = 50;
    static constexpr std::int64_t target_batch_ms = 1000;
    /// A live tick older than this is dropped when the worker takes it.
    static constexpr std::chrono::milliseconds max_tick_age{5000};
    /// How long a shutdown works through the queued ticks.
    static constexpr std::chrono::milliseconds shutdown_drain{2000};
    static constexpr std::chrono::minutes status_interval_{1};

    /// The ticks that arrived and wait for the worker.
    drop_oldest_queue<messaging::market_tick> queue_{queue_capacity};
    store_backoff store_backoff_;
    std::atomic<std::uint64_t> stale_dropped_{0};
    std::atomic<std::uint64_t> late_unpublished_{0};
    std::atomic<std::uint64_t> store_unavailable_{0};
    std::atomic<std::uint64_t> shutdown_dropped_{0};
    std::atomic<std::uint64_t> ticks_copied_{0};
    std::atomic<std::uint64_t> tick_copy_failures_{0};
    std::atomic<std::chrono::steady_clock::rep> drain_deadline_rep_{
        std::chrono::steady_clock::time_point::max().time_since_epoch().count()};
    std::atomic<std::uint64_t> batches_{0};
    std::atomic<std::uint64_t> last_batch_ticks_{0};
    std::atomic<std::int64_t> last_batch_ms_{0};
    std::atomic<bool> stop_flag_{false};
    std::thread worker_;
    std::thread status_thread_;
};

}

#endif
