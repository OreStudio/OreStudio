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
#include "ores.marketdata.service/app/batch_handoff.hpp"
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
#include <cstdint>
#include <expected>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <set>
#include <string>
#include <thread>
#include <tuple>
#include <unordered_map>
#include <utility>
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
 * @brief The ingest loop: one subscription over every producer's ticks, and one
 * path for every tick, whatever its asset class.
 *
 * A producer publishes a market_tick on "synthetic.v1.ops.tick.<source>". The tick
 * names its datum by its oresmd quote URI and its producer by its source; it
 * carries no owner. refresh() caches the enabled feed bindings by source, and
 * each binding of the tick's source is one consumer: the tick is stored as a row of
 * the tick table under that binding's tenant and party, and republished
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
 * every tick that has arrived and prepares them as one batch: it plans each tick, finds
 * the series of each consumer once, numbers the datums and builds the rows. A second
 * thread copies the batch to the tick table with one copy. A third thread then records the
 * activity and republishes. The three stages work on three batches at once. One batch waits
 * at most between two stages, so five batches are in the loop at most, and a slow stage stops
 * the stage before it and the batches do not pile up. A small batch that arrives while no
 * batch is in the stages skips them: the worker prepares, copies and republishes it itself, so a
 * slow stream does not pay for two thread wakeups. Republish follows the copy of the batch that
 * stored the tick, on core NATS with no wait for an acknowledgement. A tick is perishable, because
 * an old tick that waits while a newer one arrives is worse than a tick that is lost. So the queue
 * is bounded and a full queue drops its oldest tick, and the worker drops a tick that has aged past
 * max_tick_age when it takes the tick. A batch takes about one second, so a tick does not wait long
 * inside a slow batch, and a tick that has aged past the limit by the time its batch commits is
 * stored and not republished.
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
    /// A batch of this many ticks or fewer, taken while no batch is in the stages, is worked
    /// through by the worker itself. A handoff wakes a thread, which waits for a free CPU, so a
    /// slow stream is faster without the stages. This is a tuning value. It is above
    /// min_batch_ticks, and the work of a batch this size takes a few milliseconds, so a feed
    /// that sends more than about ten thousand ticks a second makes larger batches and uses the
    /// stages. No run has tried another value.
    static constexpr std::size_t inline_batch_ticks = 64;

    /// @param crm_bridge Optional; when set, every stored FX spot tick is offered
    /// to it as a candidate driver update.
    /// @param tick_writer The writer of the tick table, which is the store. The loop is its
    /// only caller. Without one, no tick is stored, and each is republished.
    /// @param pipelined Whether the copy of one batch runs while the next is prepared. When it
    /// is false, the worker prepares and copies each batch itself, one after the other. The burst
    /// tests use that to compare the two.
    feed_ingest_loop(ores::nats::service::client& nats,
                     ores::database::context ctx,
                     std::shared_ptr<crm_ingest_bridge> crm_bridge = nullptr,
                     std::shared_ptr<tick_writer> tick_writer = nullptr,
                     bool pipelined = true);
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

    void on_tick(const ores::nats::message& msg);
    void worker_loop();

    /// A batch between its two stages: its ticks, what the loop decided for each, and the rows
    /// that the copy writes.
    struct prepared_batch;

    /// Plans, stores and republishes @p ticks in the calling thread, one stage after the other.
    /// Fills @p outcomes when it is set.
    void process(std::vector<messaging::market_tick> ticks,
                 std::vector<std::optional<tick_drop>>* outcomes);

    /// The first stage: plans @p ticks, finds the series of every consumer and numbers the datums,
    /// and builds the rows. It writes nothing to the tick table.
    std::unique_ptr<prepared_batch> prepare(std::vector<messaging::market_tick> ticks,
                                            std::vector<std::optional<tick_drop>>* outcomes);

    /// The second stage: copies the rows of @p batch, and marks each item stored, or unavailable
    /// when the store does not answer or is left alone for a wait.
    void commit(prepared_batch& batch);

    /// The third stage: records the activity of the bindings, counts, and republishes the ticks
    /// of @p batch, stored or not.
    void announce(prepared_batch& batch);

    /// Runs the second and the third stage in the calling thread, one after the other.
    void complete(prepared_batch& batch);

    /// Numbers the datums of @p batch and builds its rows. A number that cannot be assigned sets
    /// the failure of the batch.
    void prepare_rows(prepared_batch& batch);

    /// Copies the rows of @p batch, and marks each item stored or unavailable.
    void write_rows(prepared_batch& batch);

    /// Records the newest stored tick of each binding of @p batch, at most once in
    /// activity_interval for each binding. A failure is logged and changes nothing else.
    void record_activity(const prepared_batch& batch);

    /// The thread of the second stage. It ends when its handoff is closed and empty.
    void commit_loop();

    /// The thread of the third stage. It ends when its handoff is closed and empty.
    void announce_loop();

    /// The id of the series that @p tick_datum belongs to for @p binding's party,
    /// created and classified on the first tick that lands in it. Read once for
    /// each series and then cached until refresh().
    boost::uuids::uuid series_id_for(const ores::database::context& tenant_ctx,
                                     const domain::feed_binding& binding,
                                     const named_datum& named,
                                     boost::uuids::random_generator& uuid_gen);

    /// What the URI of a tick names, read once for each distinct URI. A URI that names no
    /// datum is not kept, and a feed that sends endless distinct URIs cannot grow the cache
    /// past named_cache_limit.
    std::expected<std::shared_ptr<const named_datum>, tick_drop> name_of(const std::string& uri);

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
    const bool pipelined_;
    mutable std::mutex mu_;
    std::optional<ores::nats::service::subscription> tick_sub_;
    using bindings_map = std::map<std::string, std::vector<domain::feed_binding>>;

    /// The enabled bindings by source, and the targets built from them for each source and
    /// datum. The targets are a function of the bindings and the datum, so they live and die
    /// with the snapshot, and a refresh() leaves none that is stale.
    struct bindings_snapshot {
        bindings_map by_source;

        /// The targets of a tick of @p named from @p source, which must be a bound source.
        [[nodiscard]] std::shared_ptr<const std::vector<tick_target>>
        targets_for(const std::string& source, const named_datum& named) const;

    private:
        mutable std::mutex mu_;
        mutable std::size_t built_ = 0;
        mutable std::unordered_map<
            std::string,
            std::unordered_map<std::string, std::shared_ptr<const std::vector<tick_target>>>>
            targets_;
    };

    /// Enabled bindings by source, rebuilt by refresh(). A batch reads one snapshot.
    std::shared_ptr<const bindings_snapshot> bindings_by_source_ =
        std::make_shared<bindings_snapshot>();

    /// One series of a tenant and a party.
    struct series_entry {
        boost::uuids::uuid tenant_id;
        boost::uuids::uuid party_id;
        std::string provider_code;
        boost::uuids::uuid id;
    };
    /// Series ids by series URI, then by tenant, party and provider, cleared by refresh().
    std::unordered_map<std::string, std::vector<series_entry>> series_ids_;
    std::map<binding_key, std::shared_ptr<feed_stats>> stats_;
    /// When the activity of each binding was last recorded.
    std::map<binding_key, std::chrono::steady_clock::time_point> activity_recorded_;
    /// Classifiers by tenant id, cleared by refresh().
    std::map<std::string, std::shared_ptr<const core::series_classifier>> classifiers_;
    /// Sources and URIs already reported as dropped, so each is reported once
    /// per refresh().
    std::set<std::string> unbound_warned_;
    std::set<std::string> unnameable_warned_;
    std::set<std::string> bad_value_warned_;
    /// The datums that the URIs of ticks name. A URI names the same datum for as long as the
    /// loop runs, so refresh() keeps them.
    std::unordered_map<std::string, std::shared_ptr<const named_datum>> named_;
    /// Coordinate numbers by datum URI, then by series. A number never changes while its series
    /// exists, so refresh() keeps them.
    std::unordered_map<std::string, std::vector<std::pair<boost::uuids::uuid, std::int16_t>>>
        coords_;

    static constexpr std::size_t queue_capacity = 65536;
    static constexpr std::size_t named_cache_limit = 200000;
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
    /// How long the activity row of a binding waits between two writes.
    static constexpr std::chrono::seconds activity_interval{5};

    /// The ticks that arrived and wait for the worker.
    drop_oldest_queue<messaging::market_tick> queue_{queue_capacity};
    store_backoff store_backoff_;
    std::atomic<std::uint64_t> stale_dropped_{0};
    std::atomic<std::uint64_t> late_unpublished_{0};
    std::atomic<std::uint64_t> store_unavailable_{0};
    std::atomic<std::uint64_t> shutdown_dropped_{0};
    std::atomic<std::uint64_t> ticks_stored_{0};
    std::atomic<std::chrono::steady_clock::rep> drain_deadline_rep_{
        std::chrono::steady_clock::time_point::max().time_since_epoch().count()};
    std::atomic<std::uint64_t> batches_{0};
    std::atomic<std::uint64_t> last_batch_ticks_{0};
    std::atomic<std::int64_t> last_batch_ms_{0};
    std::atomic<bool> stop_flag_{false};
    /// The prepared batches that wait for the copy. One waits at most, so the first stage is at
    /// most one batch ahead of the second.
    batch_handoff<std::unique_ptr<prepared_batch>> handoff_{1};
    /// The copied batches that wait for their republish, one at most.
    batch_handoff<std::unique_ptr<prepared_batch>> announce_handoff_{1};
    /// The batches that the worker handed to the stages and the third stage has not finished.
    /// Only the worker adds to it, so a zero that the worker reads stays zero until it adds.
    std::atomic<std::size_t> in_flight_{0};
    std::thread worker_;
    std::thread commit_thread_;
    std::thread announce_thread_;
    std::thread status_thread_;
};

}

#endif
