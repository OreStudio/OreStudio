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
#include "ores.marketdata.service/app/feed_ingest_loop.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/market_series_asset_class.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/feed_binding_repository.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_asset_class_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/series_classification_rule_repository.hpp"
#include "ores.marketdata.core/repository/tick_coordinate_repository.hpp"
#include "ores.marketdata.service/app/observation_batch.hpp"
#include "ores.marketdata.service/app/tick_plan.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <format>
#include <optional>
#include <span>
#include <string_view>
#include <vector>

namespace ores::marketdata::service::app {

using namespace ores::logging;

/// One consumer of one accepted tick: the binding it is stored under, and the rows
/// that go with it. A tick that has several bindings makes several items.
struct feed_ingest_loop::item {
    std::size_t tick_index = 0;
    const tick_plan* plan = nullptr;
    const tick_target* target = nullptr;
    std::string series_key;
    std::vector<domain::market_observation> rows;
    bool stored = false;
    /// The store did not answer, so the tick is republished and not stored.
    bool unavailable = false;
};

feed_ingest_loop::feed_ingest_loop(ores::nats::service::client& nats,
                                   ores::database::context ctx,
                                   std::shared_ptr<crm_ingest_bridge> crm_bridge,
                                   std::shared_ptr<tick_writer> tick_writer)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , crm_bridge_(std::move(crm_bridge))
    , tick_writer_(std::move(tick_writer)) {}

feed_ingest_loop::~feed_ingest_loop() {
    // The callback of the subscription queues into queue_, which is destroyed before
    // tick_sub_. Stopping the subscription first leaves no callback to run after that.
    tick_sub_.reset();
    drain_deadline_rep_.store(
        (std::chrono::steady_clock::now() + shutdown_drain).time_since_epoch().count(),
        std::memory_order_relaxed);
    stop_flag_.store(true, std::memory_order_relaxed);
    queue_.close();
    if (worker_.joinable())
        worker_.join();
    if (status_thread_.joinable())
        status_thread_.join();
}

void feed_ingest_loop::start() {
    // subscribe() prefixes its subject for this environment, so the wildcard
    // stays relative here. subjects_below() returns the prefixed form a stream
    // filter needs, and passed to subscribe() it is prefixed twice and matches
    // no tick.
    const auto wildcard = std::string(domain::synthetic_tick_subject) + ".>";
    BOOST_LOG_SEV(lg(), info) << "Starting feed ingest loop: subscribing to '" << wildcard << "'";
    refresh();
    worker_ = std::thread(&feed_ingest_loop::worker_loop, this);
    tick_sub_ = nats_.subscribe(wildcard, [this](ores::nats::message msg) { on_tick(msg); });
    status_thread_ = std::thread(&feed_ingest_loop::status_loop, this);
}

void feed_ingest_loop::refresh() {
    repository::feed_binding_repository repo;
    const auto bindings = repo.read_latest_all_tenants(ctx_);

    auto wanted = std::make_shared<bindings_map>();
    for (const auto& b : bindings) {
        if (b.enabled)
            (*wanted)[b.source_name].push_back(b);
    }

    std::lock_guard lock(mu_);
    bindings_by_source_ = wanted;
    series_ids_.clear();
    unbound_warned_.clear();
    unnameable_warned_.clear();
    bad_value_warned_.clear();
    classifiers_.clear();

    // A stats entry per bound consumer, so a binding that never ticks still
    // shows in INGEST STATUS.
    std::map<binding_key, std::shared_ptr<feed_stats>> kept;
    for (const auto& [source_name, source_bindings] : *bindings_by_source_) {
        for (const auto& b : source_bindings) {
            const binding_key key{
                b.source_name, b.tenant_id.to_string(), boost::uuids::to_string(b.party_id)};
            const auto it = stats_.find(key);
            kept[key] = it != stats_.end() ? it->second : std::make_shared<feed_stats>();
        }
    }
    stats_ = std::move(kept);

    BOOST_LOG_SEV(lg(), info) << "Feed ingest loop: " << bindings_by_source_->size()
                              << " bound source(s)";
}

void feed_ingest_loop::on_tick(const ores::nats::message& msg) {
    auto tick = ores::nats::default_wire_codec().decode<messaging::market_tick>(msg.data);
    if (!tick) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to decode market_tick on '" << msg.subject
                                  << "': " << tick.error().what();
        return;
    }

    queue_.push(std::move(*tick));
}

void feed_ingest_loop::worker_loop() {
    std::vector<messaging::market_tick> batch;
    auto last_stale_log = std::chrono::steady_clock::time_point::min();
    std::size_t limit = initial_batch_ticks;
    for (;;) {
        batch.clear();
        const auto taken = queue_.take_all(batch, limit, std::chrono::milliseconds(200));
        const bool stopping = stop_flag_.load(std::memory_order_relaxed);
        if (taken == 0) {
            if (stopping)
                break;
            continue;
        }
        if (stopping && std::chrono::steady_clock::now().time_since_epoch().count() >
                            drain_deadline_rep_.load(std::memory_order_relaxed)) {
            const auto lost = taken + queue_.size();
            shutdown_dropped_.fetch_add(lost, std::memory_order_relaxed);
            BOOST_LOG_SEV(lg(), warn)
                << "The ingest stopped with " << lost << " queued tick(s) that it did not store";
            break;
        }

        // A tick that has aged out is dropped and not worked through, so the worker
        // that has fallen behind reaches the newest ticks at once.
        const auto stale = erase_stale(batch, std::chrono::system_clock::now(), max_tick_age);
        if (stale > 0) {
            stale_dropped_.fetch_add(stale, std::memory_order_relaxed);
            const auto now = std::chrono::steady_clock::now();
            if (now - last_stale_log > std::chrono::seconds(10)) {
                last_stale_log = now;
                BOOST_LOG_SEV(lg(), warn)
                    << "Dropped " << stale << " tick(s) older than " << max_tick_age.count()
                    << " ms: the ingest is behind the feeds";
            }
        }
        if (batch.empty())
            continue;

        try {
            const auto started = std::chrono::steady_clock::now();
            process(batch, nullptr);
            // Size the next batch from the speed of this one, so that it takes about
            // target_batch_ms. A slow store gets smaller batches and a tick waits less.
            limit = next_batch_ticks(batch.size(),
                                     std::chrono::duration_cast<std::chrono::milliseconds>(
                                         std::chrono::steady_clock::now() - started)
                                         .count(),
                                     target_batch_ms,
                                     min_batch_ticks,
                                     batch_limit);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to process a batch of " << batch.size() << " tick(s): " << e.what();
        }
    }
}

bool feed_ingest_loop::store_reachable(const ores::database::context& tenant_ctx) {
    try {
        ores::database::repository::unit_of_work uow(tenant_ctx);
        uow.commit();
        return true;
    } catch (const std::exception&) {
        return false;
    }
}

std::optional<tick_drop> feed_ingest_loop::handle_tick(const messaging::market_tick& tick) {
    return handle_batch({tick}).front();
}

std::vector<std::optional<tick_drop>>
feed_ingest_loop::handle_batch(const std::vector<messaging::market_tick>& ticks) {
    std::vector<std::optional<tick_drop>> outcomes(ticks.size());
    process(ticks, &outcomes);
    return outcomes;
}

void feed_ingest_loop::warn_dropped(const messaging::market_tick& tick, tick_drop reason) {
    std::lock_guard lock(mu_);
    switch (reason) {
        case tick_drop::unbound_source:
            if (unbound_warned_.insert(tick.source).second)
                BOOST_LOG_SEV(lg(), warn) << "Dropping ticks for unbound source '" << tick.source
                                          << "': no enabled feed_binding";
            break;
        case tick_drop::unnameable_datum:
            if (unnameable_warned_.insert(tick.oresmd_uri).second)
                BOOST_LOG_SEV(lg(), warn)
                    << "Dropping ticks for '" << tick.oresmd_uri << "': it names no ORE datum";
            break;
        case tick_drop::not_a_number:
            if (bad_value_warned_.insert(tick.source).second)
                BOOST_LOG_SEV(lg(), warn) << "Dropping ticks from source '" << tick.source
                                          << "': value '" << tick.value << "' is not a number";
            break;
    }
}

void feed_ingest_loop::process(const std::vector<messaging::market_tick>& ticks,
                               std::vector<std::optional<tick_drop>>* outcomes) {
    const auto started = std::chrono::steady_clock::now();

    std::shared_ptr<const bindings_map> bindings;
    {
        std::lock_guard lock(mu_);
        bindings = bindings_by_source_;
    }
    static const std::vector<domain::feed_binding> no_bindings;

    // Plan every tick against one snapshot of the bindings.
    std::vector<std::optional<tick_plan>> plans(ticks.size());
    for (std::size_t i = 0; i < ticks.size(); ++i) {
        const auto found = bindings->find(ticks[i].source);
        auto plan = plan_tick(ticks[i], found != bindings->end() ? found->second : no_bindings);
        if (!plan) {
            warn_dropped(ticks[i], plan.error());
            if (outcomes)
                (*outcomes)[i] = plan.error();
            continue;
        }
        plans[i] = std::move(*plan);
    }

    // Build the rows. The tenant context is made once for each tenant, and the
    // series is read once for each series, so a batch pays neither for each tick.
    boost::uuids::random_generator uuid_gen;
    std::map<std::string, ores::database::context> tenant_ctxs;
    std::map<std::string, std::vector<std::size_t>> by_tenant;
    std::vector<item> items;
    for (std::size_t i = 0; i < ticks.size(); ++i) {
        if (!plans[i])
            continue;
        for (const auto& target : plans[i]->targets) {
            const auto& b = target.binding;
            const auto tenant = b.tenant_id.to_string();
            auto ctx_it = tenant_ctxs.find(tenant);
            if (ctx_it == tenant_ctxs.end())
                ctx_it =
                    tenant_ctxs
                        .emplace(tenant, ctx_.with_tenant(b.tenant_id, "ores.marketdata.service"))
                        .first;
            item it;
            it.tick_index = i;
            it.plan = &*plans[i];
            it.target = &target;
            try {
                const auto series_id = series_id_for(ctx_it->second, b, plans[i]->datum, uuid_gen);
                it.series_key =
                    tenant + '|' + boost::uuids::to_string(b.party_id) + '|' +
                    datum::oresmd_uri_codec::write(datum::series_of(plans[i]->datum)).value();
                for (const auto& recorded_at : observation_dates(ticks[i].observation_time)) {
                    domain::market_observation obs;
                    obs.id = uuid_gen();
                    obs.tenant_id = b.tenant_id;
                    obs.party_id = b.party_id;
                    obs.series_id = series_id;
                    obs.observation_datetime = recorded_at;
                    obs.value = ticks[i].value;
                    obs.source = ticks[i].source;
                    obs.oresmd_uri = ticks[i].oresmd_uri;
                    it.rows.push_back(std::move(obs));
                }
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error)
                    << "Failed to prepare the tick for " << ticks[i].oresmd_uri << " from source '"
                    << ticks[i].source << "': " << e.what();
                continue;
            }
            by_tenant[tenant].push_back(items.size());
            items.push_back(std::move(it));
        }
    }

    // One transaction for each tenant. A store that failed lately is left alone for a
    // wait, and its ticks are republished without a store.
    bool store_open = false;
    {
        std::lock_guard lock(mu_);
        store_open = store_backoff_.may_try(std::chrono::steady_clock::now());
    }
    for (const auto& [tenant, indexes] : by_tenant) {
        if (store_open) {
            write_items(tenant_ctxs.at(tenant), items, indexes);
        } else {
            for (const auto i : indexes)
                items[i].unavailable = true;
        }
    }
    for (const auto& it : items)
        if (it.unavailable)
            store_unavailable_.fetch_add(1, std::memory_order_relaxed);
    write_ticks(ticks, items, tenant_ctxs);

    // After the commit, or without it when the store did not answer: stats, the CRM
    // bridge, and the republish.
    const auto now = std::chrono::system_clock::now();
    std::vector<std::vector<std::byte>> payloads(ticks.size());
    for (const auto& it : items) {
        if (!it.stored && !it.unavailable)
            continue;
        const auto& tick = ticks[it.tick_index];
        const auto& b = it.target->binding;

        const binding_key key{
            b.source_name, b.tenant_id.to_string(), boost::uuids::to_string(b.party_id)};
        std::uint64_t prev_count = 0;
        {
            std::lock_guard lock(mu_);
            auto& st = stats_[key];
            if (!st)
                st = std::make_shared<feed_stats>();
            prev_count = st->tick_count.fetch_add(1, std::memory_order_relaxed);
            st->last_tick_rep.store(now.time_since_epoch().count(), std::memory_order_relaxed);
            st->last_lag_ms.store(
                std::chrono::duration_cast<std::chrono::milliseconds>(now - tick.observation_time)
                    .count(),
                std::memory_order_relaxed);
        }

        if (prev_count == 0)
            BOOST_LOG_SEV(lg(), info)
                << "INGEST FIRST TICK: source='" << b.source_name << "' datum='" << tick.oresmd_uri
                << "' subject='" << it.target->subject << "' value=" << tick.value;

        // The live path never carries a tick that has aged out while its batch was
        // written. The tick stays in the store as history.
        if (now - tick.observation_time > max_tick_age) {
            late_unpublished_.fetch_add(1, std::memory_order_relaxed);
            continue;
        }

        // Currency driver pairs are FX-shaped, so only an FX spot rate is a
        // candidate driver update. The bridge offers it tenant-wide.
        if (crm_bridge_ && it.plan->datum.type() == datum::instrument_type::fx_spot)
            crm_bridge_->update(b.tenant_id.to_string(),
                                *it.plan->datum.get<datum::field::unit_ccy>(),
                                *it.plan->datum.get<datum::field::ccy>(),
                                it.plan->value,
                                tick.observation_time);

        auto& payload = payloads[it.tick_index];
        if (payload.empty())
            payload = ores::nats::default_wire_codec().encode(tick);
        try {
            nats_.publish(it.target->subject, payload);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to republish a tick on '" << it.target->subject << "': " << e.what();
        }
    }

    batches_.fetch_add(1, std::memory_order_relaxed);
    last_batch_ticks_.store(ticks.size(), std::memory_order_relaxed);
    last_batch_ms_.store(std::chrono::duration_cast<std::chrono::milliseconds>(
                             std::chrono::steady_clock::now() - started)
                             .count(),
                         std::memory_order_relaxed);
}

void feed_ingest_loop::write_ticks(
    const std::vector<messaging::market_tick>& ticks,
    const std::vector<item>& items,
    const std::map<std::string, ores::database::context>& tenant_ctxs) {
    if (!tick_writer_)
        return;

    std::vector<tick_row> rows;
    rows.reserve(items.size());
    std::size_t unnumbered = 0;
    std::string unnumbered_reason;
    for (const auto& it : items) {
        if (!it.stored)
            continue;
        const auto& b = it.target->binding;
        const auto& series_id = it.rows.front().series_id;

        std::int16_t coord = 0;
        if (it.plan->datum.fields().size() != datum::series_of(it.plan->datum).fields().size()) {
            const auto& uri = ticks[it.tick_index].oresmd_uri;
            const auto key = boost::uuids::to_string(series_id) + '|' + uri;
            bool known = false;
            {
                std::lock_guard lock(mu_);
                const auto found = coords_.find(key);
                if (found != coords_.end()) {
                    coord = found->second;
                    known = true;
                }
            }
            if (!known) {
                try {
                    coord = repository::tick_coordinate_repository{}.resolve(
                        tenant_ctxs.at(b.tenant_id.to_string()), b.party_id, series_id, uri);
                } catch (const std::exception& e) {
                    ++unnumbered;
                    unnumbered_reason = e.what();
                    continue;
                }
                std::lock_guard lock(mu_);
                coords_[key] = coord;
            }
        }

        tick_row row;
        row.tenant_id = b.tenant_id.to_string();
        row.party_id = boost::uuids::to_string(b.party_id);
        row.series_id = boost::uuids::to_string(series_id);
        row.ts = ticks[it.tick_index].observation_time;
        row.coord = coord;
        row.value = it.plan->value;
        rows.push_back(std::move(row));
    }
    if (unnumbered > 0) {
        tick_copy_failures_.fetch_add(unnumbered, std::memory_order_relaxed);
        if (!tick_write_failing_.exchange(true))
            BOOST_LOG_SEV(lg(), error)
                << "The tick table cannot take " << unnumbered
                << " tick(s) of datums with a coordinate: " << unnumbered_reason;
    }
    if (rows.empty())
        return;

    try {
        tick_writer_->write(rows);
        ticks_copied_.fetch_add(rows.size(), std::memory_order_relaxed);
        if (unnumbered == 0 && tick_write_failing_) {
            tick_write_failing_ = false;
            BOOST_LOG_SEV(lg(), info) << "The tick table accepts copies again";
        }
    } catch (const std::exception& e) {
        tick_copy_failures_.fetch_add(rows.size(), std::memory_order_relaxed);
        if (!tick_write_failing_) {
            tick_write_failing_ = true;
            BOOST_LOG_SEV(lg(), error) << e.what();
        }
    }
}

void feed_ingest_loop::write_items(const ores::database::context& tenant_ctx,
                                   std::vector<item>& items,
                                   std::span<const std::size_t> indexes) {
    if (indexes.empty())
        return;

    std::vector<domain::market_observation> rows;
    for (const auto i : indexes)
        rows.insert(rows.end(), items[i].rows.begin(), items[i].rows.end());
    rows = keep_last_per_key(std::move(rows));

    try {
        ores::database::repository::unit_of_work uow(tenant_ctx);
        repository::market_observation_repository{}.insert(uow.ctx(), rows);
        uow.commit();
        for (const auto i : indexes)
            items[i].stored = true;
        std::lock_guard lock(mu_);
        if (store_backoff_.note_success())
            BOOST_LOG_SEV(lg(), info) << "The store answers again";
        return;
    } catch (const std::exception& e) {
        // A store that is down fails every write. Splitting the batch would try it again
        // for each half and drop every tick, so the loop makes one probe first.
        if (!store_reachable(tenant_ctx)) {
            bool starts = false;
            {
                std::lock_guard lock(mu_);
                starts = store_backoff_.note_failure(std::chrono::steady_clock::now());
            }
            if (starts)
                BOOST_LOG_SEV(lg(), error)
                    << "The store does not answer, so ticks are republished and not stored: "
                    << e.what();
            for (const auto i : indexes)
                items[i].unavailable = true;
            return;
        }
        if (indexes.size() == 1) {
            // The cached series id may be the fault, such as a series that was
            // removed. Forget it, so the next tick reads the series again.
            {
                std::lock_guard lock(mu_);
                series_ids_.erase(items[indexes.front()].series_key);
            }
            const auto& tick_uri = items[indexes.front()].rows.front().oresmd_uri;
            BOOST_LOG_SEV(lg(), error)
                << "Failed to store the tick for " << tick_uri << ": " << e.what();
            return;
        }
        BOOST_LOG_SEV(lg(), warn) << "A batch of " << indexes.size()
                                  << " tick(s) failed, so it is split in two: " << e.what();
    }

    const auto middle = indexes.size() / 2;
    write_items(tenant_ctx, items, indexes.first(middle));
    write_items(tenant_ctx, items, indexes.subspan(middle));
}

std::shared_ptr<const core::series_classifier>
feed_ingest_loop::classifier_for(const ores::database::context& tenant_ctx) {
    const auto tenant = tenant_ctx.tenant_id().to_string();
    {
        std::lock_guard lock(mu_);
        const auto it = classifiers_.find(tenant);
        if (it != classifiers_.end())
            return it->second;
    }
    auto classifier = std::make_shared<const core::series_classifier>(
        repository::series_classification_rule_repository{}.read_latest(tenant_ctx));
    std::lock_guard lock(mu_);
    return classifiers_.try_emplace(tenant, std::move(classifier)).first->second;
}

domain::market_series make_feed_series(const domain::feed_binding& binding,
                                       const boost::uuids::uuid& id,
                                       const std::string& series_uri,
                                       const std::string& series_subclass,
                                       const std::string& service_account) {
    domain::market_series series;
    series.id = id;
    series.tenant_id = binding.tenant_id;
    series.party_id = binding.party_id;
    series.oresmd_uri = series_uri;
    series.series_subclass = series_subclass;
    series.producer_kind = binding.producer_kind;
    series.modified_by = service_account;
    series.performed_by = service_account;
    series.change_reason_code = "system.initial_load";
    series.change_commentary = "Created by the feed ingest loop for source " + binding.source_name;
    return series;
}

/**
 * @brief The dates one tick is recorded under: the day it was published, and the close
 * of the last completed business day.
 *
 * ORE reads a market time series by the date it carries -- a market.txt line's first
 * field, which the export takes from the observation's own datetime -- and a report
 * that values at the previous business day's close asks for that date. A live producer
 * can only publish a value it holds now, so a deployment whose feeds started today has
 * no close for yesterday and every end-of-day report fails on a valuation date with no
 * quotes. Recording each observation under the close as well gives the series a
 * valuation date's worth of history from the producer's first tick, so a fresh
 * environment can value a report at the close it values at, with nothing run by hand
 * and no backfill of dates the producer never saw.
 *
 * One business day is as far back as this reaches, because one is what an end-of-day
 * report asks for; a longer lookback would be history the producer has no state for.
 * The time of day is kept, so the rows of one date stay in the order they arrived and
 * a reader that takes the last one takes the newest close.
 */
std::vector<std::chrono::system_clock::time_point>
observation_dates(std::chrono::system_clock::time_point published_at) {
    using namespace std::chrono;
    std::vector<system_clock::time_point> dates{published_at};

    const auto published_day = floor<days>(published_at);
    auto close_day = published_day - days{1};
    while (weekday{close_day} == Saturday || weekday{close_day} == Sunday)
        close_day -= days{1};

    // A producer that publishes on a weekend records no second date: the close a report
    // asks for is already the one the last business day of ticks recorded.
    if (weekday{published_day} != Saturday && weekday{published_day} != Sunday)
        dates.push_back(close_day + (published_at - published_day));
    return dates;
}
boost::uuids::uuid feed_ingest_loop::series_id_for(const ores::database::context& tenant_ctx,
                                                   const domain::feed_binding& binding,
                                                   const datum::market_datum& tick_datum,
                                                   boost::uuids::random_generator& uuid_gen) {
    const auto party = boost::uuids::to_string(binding.party_id);
    const auto series_uri = datum::oresmd_uri_codec::write(datum::series_of(tick_datum)).value();
    const auto cache_key = binding.tenant_id.to_string() + '|' + party + '|' + series_uri;
    {
        std::lock_guard lock(mu_);
        const auto cached = series_ids_.find(cache_key);
        if (cached != series_ids_.end())
            return cached->second;
    }

    repository::market_series_repository series_repo;
    auto existing = series_repo.read_latest_by_uri(tenant_ctx, series_uri, party);
    if (existing.empty()) {
        const auto ck = core::classification_key_of(tick_datum);
        const auto cl =
            classifier_for(tenant_ctx)->classify(ck.series_type, ck.metric, ck.qualifier);
        BOOST_LOG_SEV(lg(), info) << "Creating market series " << series_uri;

        auto series = make_feed_series(
            binding, uuid_gen(), series_uri, cl.series_subclass, ctx_.service_account());
        series_repo.write(tenant_ctx, series);

        std::vector<domain::market_series_asset_class> classes;
        for (const auto& code : cl.asset_classes) {
            domain::market_series_asset_class row;
            row.tenant_id = series.tenant_id.to_string();
            row.market_series_id = series.id;
            row.asset_class_code = code;
            row.modified_by = series.modified_by;
            row.performed_by = series.performed_by;
            row.change_reason_code = series.change_reason_code;
            row.change_commentary = series.change_commentary;
            classes.push_back(std::move(row));
        }
        if (!classes.empty())
            repository::market_series_asset_class_repository(tenant_ctx).write(classes);
        existing.push_back(std::move(series));
    }

    const auto id = existing.front().id;
    std::lock_guard lock(mu_);
    series_ids_[cache_key] = id;
    return id;
}

void feed_ingest_loop::status_loop() {
    using namespace std::chrono;
    constexpr auto slice = milliseconds(200);
    auto next = steady_clock::now() + status_interval_;
    while (!stop_flag_.load(std::memory_order_relaxed)) {
        std::this_thread::sleep_for(slice);
        if (steady_clock::now() >= next) {
            log_status();
            next = steady_clock::now() + status_interval_;
        }
    }
}

void feed_ingest_loop::log_status() const {
    using namespace std::chrono;
    std::lock_guard lock(mu_);
    BOOST_LOG_SEV(lg(), info)
        << "INGEST STATUS: queue=" << queue_.size() << " dropped=" << queue_.dropped()
        << " stale=" << stale_dropped_.load(std::memory_order_relaxed)
        << " late_unpublished=" << late_unpublished_.load(std::memory_order_relaxed)
        << " store_unavailable=" << store_unavailable_.load(std::memory_order_relaxed)
        << " ticks_copied=" << ticks_copied_.load(std::memory_order_relaxed)
        << " tick_copy_failures=" << tick_copy_failures_.load(std::memory_order_relaxed)
        << " batches=" << batches_.load(std::memory_order_relaxed)
        << " last_batch_ticks=" << last_batch_ticks_.load(std::memory_order_relaxed)
        << " last_batch_ms=" << last_batch_ms_.load(std::memory_order_relaxed);
    if (stats_.empty()) {
        BOOST_LOG_SEV(lg(), info) << "INGEST STATUS: no active subscriptions";
        return;
    }
    const auto now = system_clock::now();
    for (const auto& [key, st] : stats_) {
        const auto count = st->tick_count.load(std::memory_order_relaxed);
        const auto last_tp = system_clock::time_point{
            system_clock::duration{st->last_tick_rep.load(std::memory_order_relaxed)}};
        const bool ever = last_tp != system_clock::time_point::min();
        BOOST_LOG_SEV(lg(), info) << "INGEST STATUS: source='" << key.source_name << "' tenant='"
                                  << key.tenant_id << "' party='" << key.party_id
                                  << "' ticks=" << count
                                  << (ever ? std::format(
                                                 " last_tick={}s ago lag={}ms",
                                                 duration_cast<seconds>(now - last_tp).count(),
                                                 st->last_lag_ms.load(std::memory_order_relaxed)) :
                                             std::string(" last_tick=never"));
    }
}

}
