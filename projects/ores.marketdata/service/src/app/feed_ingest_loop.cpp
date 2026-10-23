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
#include "ores.database/service/tenant_context.hpp"
#include "ores.marketdata.api/domain/adapter_subjects.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/market_series_asset_class.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/feed_binding_activity_repository.hpp"
#include "ores.marketdata.core/repository/feed_binding_repository.hpp"
#include "ores.marketdata.core/repository/market_data_mapping_repository.hpp"
#include "ores.marketdata.core/repository/market_data_source_repository.hpp"
#include "ores.marketdata.core/repository/market_data_vendor_instrument_repository.hpp"
#include "ores.marketdata.core/repository/market_series_asset_class_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/route_repository.hpp"
#include "ores.marketdata.core/repository/series_classification_rule_repository.hpp"
#include "ores.marketdata.core/repository/tick_coords_repository.hpp"
#include "ores.marketdata.service/app/batch_size.hpp"
#include "ores.marketdata.service/app/tick_plan.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/functional/hash.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <format>
#include <optional>
#include <set>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ores::marketdata::service::app {

using namespace ores::logging;

/// One consumer of one accepted tick: the binding it is stored under and the series it lands
/// in. A tick that has several bindings makes several items.
struct feed_ingest_loop::item {
    std::size_t tick_index = 0;
    const tick_plan* plan = nullptr;
    const tick_target* target = nullptr;
    boost::uuids::uuid series_id{};
    bool stored = false;
    /// The store did not answer, so the tick is republished and not stored.
    bool unavailable = false;
};

/// A batch between its two stages. The first stage fills it and the second reads it, so the
/// second can copy this batch while the first prepares the next. The items point into @c plans,
/// whose elements stay where they are when the batch is handed on, and the rows view the text of
/// the targets and of @c series_text.
struct feed_ingest_loop::prepared_batch {
    std::vector<messaging::market_tick> ticks;
    std::vector<std::optional<tick_plan>> plans;
    std::vector<item> items;
    std::map<std::string, ores::database::context> tenant_ctxs;
    std::unordered_map<boost::uuids::uuid, std::string, boost::hash<boost::uuids::uuid>>
        series_text;
    std::vector<tick_row> rows;
    /// Why the rows could not be prepared, or empty when they could.
    std::string failure;
    std::chrono::steady_clock::time_point started;
};

feed_ingest_loop::feed_ingest_loop(ores::nats::service::client& nats,
                                   ores::database::context ctx,
                                   std::shared_ptr<crm_ingest_bridge> crm_bridge,
                                   std::shared_ptr<tick_writer> tick_writer,
                                   bool pipelined)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , crm_bridge_(std::move(crm_bridge))
    , tick_writer_(std::move(tick_writer))
    , pipelined_(pipelined) {}

feed_ingest_loop::~feed_ingest_loop() {
    // The callback of the subscription queues into queue_, which is destroyed before
    // tick_sub_. Stopping the subscription first leaves no callback to run after that.
    tick_sub_.reset();
    adapter_sub_.reset();
    drain_deadline_rep_.store(
        (std::chrono::steady_clock::now() + shutdown_drain).time_since_epoch().count(),
        std::memory_order_relaxed);
    stop_flag_.store(true, std::memory_order_relaxed);
    queue_.close();
    if (worker_.joinable())
        worker_.join();
    // The second stage finishes the batch it holds and the one that waits, and then stops.
    handoff_.close();
    if (commit_thread_.joinable())
        commit_thread_.join();
    announce_handoff_.close();
    if (announce_thread_.joinable())
        announce_thread_.join();
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
    if (pipelined_) {
        announce_thread_ = std::thread(&feed_ingest_loop::announce_loop, this);
        commit_thread_ = std::thread(&feed_ingest_loop::commit_loop, this);
    }
    worker_ = std::thread(&feed_ingest_loop::worker_loop, this);
    tick_sub_ = nats_.subscribe(wildcard, [this](ores::nats::message msg) { on_tick(msg); });
    const auto adapter_wildcard = domain::adapter_ingress_subject_all();
    BOOST_LOG_SEV(lg(), info) << "Subscribing to the ticks of adapters on '" << adapter_wildcard
                              << "'";
    adapter_sub_ = nats_.subscribe(adapter_wildcard,
                                   [this](ores::nats::message msg) { on_adapter_tick(msg); });
    status_thread_ = std::thread(&feed_ingest_loop::status_loop, this);
}

std::shared_ptr<const std::vector<tick_target>>
feed_ingest_loop::bindings_snapshot::targets_for(const std::string& source,
                                                 const named_datum& named) const {
    std::lock_guard lock(mu_);
    auto& by_uri = targets_[source];
    const auto found = by_uri.find(named.uri);
    if (found != by_uri.end())
        return found->second;
    auto targets = make_targets(by_source.at(source), named);
    if (built_ < named_cache_limit) {
        by_uri.emplace(named.uri, targets);
        ++built_;
    }
    return targets;
}

namespace {

/// The consumer a route makes of its party: the tick of the source that the route names is
/// stored for the party under the provider of the route.
domain::feed_binding consumer_of(const domain::route& route, const std::string& source_name) {
    domain::feed_binding b;
    b.id = route.id;
    b.tenant_id = route.tenant_id;
    b.party_id = route.party_id;
    b.source_name = source_name;
    b.provider_code = route.provider_code;
    b.source_id = route.source_id;
    b.enabled = true;
    return b;
}

}

void feed_ingest_loop::refresh() {
    repository::feed_binding_repository repo;
    const auto bindings = repo.read_latest_all_tenants(ctx_);

    auto wanted = std::make_shared<bindings_snapshot>();
    for (const auto& b : bindings) {
        if (b.enabled)
            wanted->by_source[b.source_name].push_back(b);
    }

    // The catalogue belongs to the system tenant, and the routes to each party. A route makes
    // its party a consumer of the source it names, in the same table as a binding.
    const auto system_ctx = ores::database::service::tenant_context::with_system_tenant(ctx_);
    auto sources = repository::market_data_source_repository{}.read_latest(system_ctx);
    auto instruments =
        repository::market_data_vendor_instrument_repository{}.read_latest(system_ctx);
    auto mappings = repository::market_data_mapping_repository{}.read_latest(system_ctx);
    const auto routes = repository::route_repository{}.read_latest_all_tenants(ctx_);
    std::size_t routed = 0;
    for (const auto& route : routes) {
        const auto source =
            std::ranges::find(sources, route.source_id, &domain::market_data_source::id);
        if (source == sources.end())
            continue;
        wanted->by_source[source->name].push_back(consumer_of(route, source->name));
        ++routed;
    }
    auto translator = std::make_shared<adapter_tick_translator>(
        std::move(sources), std::move(instruments), std::move(mappings));

    std::lock_guard lock(mu_);
    bindings_by_source_ = wanted;
    translator_ = std::move(translator);
    adapter_drop_warned_.clear();
    series_ids_.clear();
    unbound_warned_.clear();
    unnameable_warned_.clear();
    bad_value_warned_.clear();
    classifiers_.clear();

    // A stats entry per bound consumer, so a binding that never ticks still
    // shows in INGEST STATUS.
    std::map<binding_key, std::shared_ptr<feed_stats>> kept;
    for (const auto& [source_name, source_bindings] : bindings_by_source_->by_source) {
        for (const auto& b : source_bindings) {
            const binding_key key{
                b.source_name, b.tenant_id.to_string(), boost::uuids::to_string(b.party_id)};
            const auto it = stats_.find(key);
            kept[key] = it != stats_.end() ? it->second : std::make_shared<feed_stats>();
        }
    }
    stats_ = std::move(kept);

    BOOST_LOG_SEV(lg(), info) << "Feed ingest loop: " << bindings_by_source_->by_source.size()
                              << " bound source(s), " << routed << " route(s), "
                              << translator_->source_count() << " catalogue source(s)";
}

std::shared_ptr<adapter_tick_translator> feed_ingest_loop::translator() const {
    std::lock_guard lock(mu_);
    return translator_;
}

std::optional<adapter_tick_drop>
feed_ingest_loop::accept_adapter_tick(std::string_view adapter_id,
                                      const messaging::adapter_tick& tick) {
    const auto current = translator();
    auto translated = current->translate(adapter_id, tick, std::chrono::system_clock::now());
    if (!translated) {
        const auto reason = translated.error();
        const auto key = std::string(adapter_id) + ":" + std::string(to_string(reason));
        bool first = false;
        {
            std::lock_guard lock(mu_);
            first = adapter_drop_warned_.insert(key).second;
        }
        if (first)
            BOOST_LOG_SEV(lg(), warn)
                << "Dropping ticks of adapter '" << adapter_id << "': " << to_string(reason)
                << " (first: " << tick.external_id << " " << tick.field << ")";
        return reason;
    }
    queue_.push(std::move(translated->tick));
    return std::nullopt;
}

void feed_ingest_loop::on_adapter_tick(const ores::nats::message& msg) {
    // The subject carries the environment prefix in front of the ingress base.
    const auto base = std::string(domain::adapter_ingress_subject_base) + ".";
    const auto at = msg.subject.find(base);
    const auto parts =
        at == std::string::npos ?
            std::nullopt :
            domain::adapter_ingress_subject_parts(std::string_view(msg.subject).substr(at));
    if (!parts) {
        BOOST_LOG_SEV(lg(), warn) << "Dropping a tick on '" << msg.subject
                                  << "': not an ingress subject of an adapter";
        return;
    }
    auto tick = ores::nats::default_wire_codec().decode<messaging::adapter_tick>(msg.data);
    if (!tick) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to decode adapter_tick on '" << msg.subject
                                  << "': " << tick.error().what();
        return;
    }
    accept_adapter_tick(parts->adapter_id, *tick);
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

        const auto count = batch.size();
        try {
            const auto started = std::chrono::steady_clock::now();
            // With nothing in the stages, a small batch keeps its order and skips the handoffs.
            const bool quiet =
                count <= inline_batch_ticks && in_flight_.load(std::memory_order_acquire) == 0;
            if (pipelined_ && !quiet) {
                auto prepared = prepare(std::move(batch), nullptr);
                in_flight_.fetch_add(1, std::memory_order_relaxed);
                if (!handoff_.push(std::move(prepared))) {
                    in_flight_.fetch_sub(1, std::memory_order_release);
                    shutdown_dropped_.fetch_add(count, std::memory_order_relaxed);
                }
            } else
                process(std::move(batch), nullptr);
            // Size the next batch from the time of one turn, so that it takes about
            // target_batch_ms. A batch that goes through the stages waits in the push while the
            // stage ahead runs, so its turn is as long as the slower stage. A batch that the
            // worker works through itself takes the time of all three stages. A slow store gets
            // smaller batches and a tick waits less.
            limit = next_batch_ticks(count,
                                     std::chrono::duration_cast<std::chrono::milliseconds>(
                                         std::chrono::steady_clock::now() - started)
                                         .count(),
                                     target_batch_ms,
                                     min_batch_ticks,
                                     batch_limit);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to prepare a batch of " << count << " tick(s): " << e.what();
        }
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

std::unique_ptr<feed_ingest_loop::prepared_batch>
feed_ingest_loop::prepare(std::vector<messaging::market_tick> received,
                          std::vector<std::optional<tick_drop>>* outcomes) {
    auto batch = std::make_unique<prepared_batch>();
    batch->started = std::chrono::steady_clock::now();
    batch->ticks = std::move(received);
    const auto& ticks = batch->ticks;

    std::shared_ptr<const bindings_snapshot> bindings;
    {
        std::lock_guard lock(mu_);
        bindings = bindings_by_source_;
    }
    static const std::vector<domain::feed_binding> no_bindings;

    // Plan every tick against one snapshot of the bindings.
    batch->plans.resize(ticks.size());
    for (std::size_t i = 0; i < ticks.size(); ++i) {
        const auto found = bindings->by_source.find(ticks[i].source);
        const auto& source_bindings =
            found != bindings->by_source.end() ? found->second : no_bindings;
        // The URI is read only for a bound source, as plan_tick does, and once for each
        // distinct URI.
        std::shared_ptr<const named_datum> named;
        std::shared_ptr<const std::vector<tick_target>> targets;
        if (!source_bindings.empty()) {
            auto read = name_of(ticks[i].oresmd_uri);
            if (!read) {
                warn_dropped(ticks[i], read.error());
                if (outcomes)
                    (*outcomes)[i] = read.error();
                continue;
            }
            named = std::move(*read);
            targets = bindings->targets_for(ticks[i].source, *named);
        }
        auto plan = plan_tick(ticks[i], source_bindings, std::move(named), std::move(targets));
        if (!plan) {
            warn_dropped(ticks[i], plan.error());
            if (outcomes)
                (*outcomes)[i] = plan.error();
            continue;
        }
        batch->plans[i] = std::move(*plan);
    }

    // Resolve the series of every consumer. The tenant context is made once for each tenant,
    // and the series is read once for each series, so a batch pays neither for each tick.
    boost::uuids::random_generator uuid_gen;
    auto& tenant_ctxs = batch->tenant_ctxs;
    auto& items = batch->items;
    for (std::size_t i = 0; i < ticks.size(); ++i) {
        if (!batch->plans[i])
            continue;
        for (const auto& target : *batch->plans[i]->targets) {
            const auto& b = target.binding;
            auto ctx_it = tenant_ctxs.find(target.tenant_text);
            if (ctx_it == tenant_ctxs.end())
                ctx_it = tenant_ctxs
                             .emplace(target.tenant_text,
                                      ctx_.with_tenant(b.tenant_id, "ores.marketdata.service"))
                             .first;
            item it;
            it.tick_index = i;
            it.plan = &*batch->plans[i];
            it.target = &target;
            try {
                it.series_id = series_id_for(ctx_it->second, b, *batch->plans[i]->named, uuid_gen);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error)
                    << "Failed to prepare the tick for " << ticks[i].oresmd_uri << " from source '"
                    << ticks[i].source << "': " << e.what();
                continue;
            }
            items.push_back(std::move(it));
        }
    }

    prepare_rows(*batch);
    return batch;
}

void feed_ingest_loop::process(std::vector<messaging::market_tick> ticks,
                               std::vector<std::optional<tick_drop>>* outcomes) {
    const auto batch = prepare(std::move(ticks), outcomes);
    complete(*batch);
}

void feed_ingest_loop::commit_loop() {
    while (auto batch = handoff_.pop()) {
        try {
            commit(**batch);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to copy a batch of " << (*batch)->ticks.size()
                                       << " tick(s): " << e.what();
            // The ticks are still republished, as when the store does not answer.
            for (auto& it : (*batch)->items) {
                it.stored = false;
                if (!it.unavailable) {
                    it.unavailable = true;
                    store_unavailable_.fetch_add(1, std::memory_order_relaxed);
                }
            }
        }
        const auto count = (*batch)->ticks.size();
        if (!announce_handoff_.push(std::move(*batch))) {
            in_flight_.fetch_sub(1, std::memory_order_release);
            shutdown_dropped_.fetch_add(count, std::memory_order_relaxed);
        }
    }
}

void feed_ingest_loop::announce_loop() {
    while (auto batch = announce_handoff_.pop()) {
        try {
            announce(**batch);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to announce a batch of " << (*batch)->ticks.size()
                                       << " tick(s): " << e.what();
        }
        in_flight_.fetch_sub(1, std::memory_order_release);
    }
}

void feed_ingest_loop::complete(prepared_batch& batch) {
    commit(batch);
    announce(batch);
}

void feed_ingest_loop::commit(prepared_batch& batch) {
    // A store that failed lately is left alone for a wait, and its ticks are republished
    // without a store.
    write_rows(batch);
    for (const auto& it : batch.items)
        if (it.unavailable)
            store_unavailable_.fetch_add(1, std::memory_order_relaxed);
}

void feed_ingest_loop::announce(prepared_batch& batch) {
    const auto& ticks = batch.ticks;
    const auto& items = batch.items;
    record_activity(batch);

    // After the commit, or without it when the store did not answer: stats, the CRM
    // bridge, and the republish.
    const auto now = std::chrono::system_clock::now();

    // The counters of each consumer, found once for each target and not for each tick.
    std::unordered_map<const tick_target*, std::shared_ptr<feed_stats>> stats_of;
    {
        std::lock_guard lock(mu_);
        for (const auto& it : items) {
            if ((!it.stored && !it.unavailable) || stats_of.contains(it.target))
                continue;
            const auto& b = it.target->binding;
            auto& st =
                stats_[binding_key{b.source_name, it.target->tenant_text, it.target->party_text}];
            if (!st)
                st = std::make_shared<feed_stats>();
            stats_of.emplace(it.target, st);
        }
    }

    std::vector<std::vector<std::byte>> payloads(ticks.size());
    // The deliveries of this batch, by consumer and item. The views point into the ticks of the
    // batch, which outlive this function.
    std::map<std::pair<const tick_target*, std::string_view>, std::int64_t> delivered;
    for (const auto& it : items) {
        if (!it.stored && !it.unavailable)
            continue;
        const auto& tick = ticks[it.tick_index];
        const auto& b = it.target->binding;

        auto& st = stats_of.at(it.target);
        const auto prev_count = st->tick_count.fetch_add(1, std::memory_order_relaxed);
        st->last_tick_rep.store(now.time_since_epoch().count(), std::memory_order_relaxed);
        st->last_lag_ms.store(
            std::chrono::duration_cast<std::chrono::milliseconds>(now - tick.observation_time)
                .count(),
            std::memory_order_relaxed);

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
        if (crm_bridge_ && it.plan->named->datum.type() == datum::instrument_type::fx_spot)
            crm_bridge_->update(it.target->tenant_text,
                                *it.plan->named->datum.get<datum::field::unit_ccy>(),
                                *it.plan->named->datum.get<datum::field::ccy>(),
                                it.plan->value,
                                tick.observation_time);

        auto& payload = payloads[it.tick_index];
        if (payload.empty())
            payload = ores::nats::default_wire_codec().encode(tick);
        try {
            nats_.publish(it.target->subject, payload);
            if (meter_)
                ++delivered[{it.target, tick.oresmd_uri}];
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to republish a tick on '" << it.target->subject << "': " << e.what();
        }
    }

    if (meter_ && !delivered.empty()) {
        const auto period = delivery_meter::hour_of(now);
        std::vector<delivery_entry> counts;
        counts.reserve(delivered.size());
        for (const auto& [at, count] : delivered) {
            const auto& b = at.first->binding;
            counts.push_back({delivery_key{b.tenant_id,
                                           b.party_id,
                                           b.provider_code,
                                           b.source_id,
                                           std::string(at.second),
                                           period},
                              count});
        }
        meter_->add_many(counts);
    }

    batches_.fetch_add(1, std::memory_order_relaxed);
    last_batch_ticks_.store(ticks.size(), std::memory_order_relaxed);
    last_batch_ms_.store(std::chrono::duration_cast<std::chrono::milliseconds>(
                             std::chrono::steady_clock::now() - batch.started)
                             .count(),
                         std::memory_order_relaxed);
}

namespace {

/// The key of a row of the tick table: two ticks of one datum and time are one row.
struct row_key {
    boost::uuids::uuid series;
    std::int16_t coord;
    std::int64_t ts;
    bool operator==(const row_key&) const = default;
};

struct row_key_hash {
    std::size_t operator()(const row_key& k) const {
        std::size_t h = boost::hash<boost::uuids::uuid>{}(k.series);
        boost::hash_combine(h, k.coord);
        boost::hash_combine(h, k.ts);
        return h;
    }
};

}

void feed_ingest_loop::prepare_rows(prepared_batch& batch) {
    const auto& ticks = batch.ticks;
    auto& items = batch.items;
    if (items.empty())
        return;

    // The datum of a tick has a number within its series, which the table stores in place of
    // the URI. The loop assigns the numbers it does not know yet for the whole batch, with a
    // few statements for each tenant, so a start with many new datums does not open a
    // connection for each. A number that cannot be assigned means the database does not
    // answer, so the whole batch is left unstored.
    std::vector<std::int16_t> coord_of(items.size(), 0);
    std::vector<bool> numbered(items.size(), false);
    std::map<std::string, std::vector<repository::tick_datum>> unknown;
    const auto cached_number = [&](const boost::uuids::uuid& series,
                                   const std::string& uri) -> std::optional<std::int16_t> {
        const auto found = coords_.find(uri);
        if (found == coords_.end())
            return std::nullopt;
        for (const auto& [id, number] : found->second)
            if (id == series)
                return number;
        return std::nullopt;
    };
    {
        std::lock_guard lock(mu_);
        std::set<std::pair<boost::uuids::uuid, std::string>> queued;
        for (std::size_t i = 0; i < items.size(); ++i) {
            const auto& it = items[i];
            const auto& uri = ticks[it.tick_index].oresmd_uri;
            if (const auto number = cached_number(it.series_id, uri)) {
                coord_of[i] = *number;
                numbered[i] = true;
            } else if (queued.insert({it.series_id, uri}).second) {
                unknown[it.target->tenant_text].push_back(
                    {it.target->binding.party_id, it.series_id, uri});
            }
        }
    }
    for (const auto& [tenant, wanted] : unknown) {
        try {
            const auto numbers = repository::tick_coords_repository{}.resolve_all(
                batch.tenant_ctxs.at(tenant), wanted);
            std::lock_guard lock(mu_);
            for (std::size_t j = 0; j < numbers.size(); ++j)
                coords_[wanted[j].oresmd_uri].emplace_back(wanted[j].series_id, numbers[j]);
        } catch (const std::exception& e) {
            batch.failure = e.what();
            return;
        }
    }

    // A tick of the same datum and time as an earlier one in the batch replaces it, so a
    // reader finds one row for each. The rows view the text of the targets, and of the series
    // ids written here once for each series.
    {
        std::lock_guard lock(mu_);
        for (std::size_t i = 0; i < items.size(); ++i)
            if (!numbered[i])
                coord_of[i] =
                    *cached_number(items[i].series_id, ticks[items[i].tick_index].oresmd_uri);
    }
    batch.rows.reserve(items.size());
    std::unordered_map<row_key, std::size_t, row_key_hash> row_at;
    row_at.reserve(items.size());
    for (std::size_t i = 0; i < items.size(); ++i) {
        const auto& it = items[i];
        const auto& tick = ticks[it.tick_index];
        auto& text = batch.series_text[it.series_id];
        if (text.empty())
            text = boost::uuids::to_string(it.series_id);
        tick_row row;
        row.tenant_id = it.target->tenant_text;
        row.party_id = it.target->party_text;
        row.series_id = text;
        row.ts = tick.observation_time;
        row.coord = coord_of[i];
        row.value = it.plan->value;
        row.source_id = it.target->source_text;
        const auto at = row_at.try_emplace(
            row_key{it.series_id, coord_of[i], row.ts.time_since_epoch().count()},
            batch.rows.size());
        if (at.second)
            batch.rows.push_back(row);
        else
            batch.rows[at.first->second] = row;
    }
}

void feed_ingest_loop::write_rows(prepared_batch& batch) {
    auto& items = batch.items;
    if (items.empty())
        return;

    bool store_open = false;
    {
        std::lock_guard lock(mu_);
        store_open = tick_writer_ && store_backoff_.may_try(std::chrono::steady_clock::now());
    }
    if (!store_open) {
        for (auto& it : items)
            it.unavailable = true;
        return;
    }

    std::string failure = batch.failure;
    if (failure.empty()) {
        try {
            tick_writer_->write(batch.rows);
        } catch (const std::exception& e) {
            failure = e.what();
        }
    }

    std::lock_guard lock(mu_);
    if (failure.empty()) {
        for (auto& it : items)
            it.stored = true;
        ticks_stored_.fetch_add(batch.rows.size(), std::memory_order_relaxed);
        if (store_backoff_.note_success())
            BOOST_LOG_SEV(lg(), info) << "The store answers again";
        return;
    }
    for (auto& it : items)
        it.unavailable = true;
    if (store_backoff_.note_failure(std::chrono::steady_clock::now()))
        BOOST_LOG_SEV(lg(), error) << "The store does not answer, so ticks are republished and "
                                      "not stored: "
                                   << failure;
}

void feed_ingest_loop::record_activity(const prepared_batch& batch) {
    const auto& ticks = batch.ticks;
    const auto& items = batch.items;
    // The newest stored tick of each target of the batch.
    struct newest {
        const item* it;
        std::chrono::system_clock::time_point at;
    };
    std::unordered_map<const tick_target*, newest> latest;
    for (const auto& it : items) {
        if (!it.stored)
            continue;
        const auto at = ticks[it.tick_index].observation_time;
        const auto found = latest.find(it.target);
        if (found == latest.end())
            latest.emplace(it.target, newest{&it, at});
        else if (found->second.at < at)
            found->second = newest{&it, at};
    }

    // The bindings whose row is due, in one statement for each tenant.
    std::map<std::string, std::vector<repository::feed_binding_activity_entry>> due;
    {
        const auto now = std::chrono::steady_clock::now();
        std::lock_guard lock(mu_);
        for (const auto& [target, n] : latest) {
            const auto& b = target->binding;
            auto& last = activity_recorded_[binding_key{
                b.source_name, target->tenant_text, target->party_text}];
            if (now - last < activity_interval)
                continue;
            last = now;
            due[target->tenant_text].push_back({b.party_id, b.source_name, n.it->series_id, n.at});
        }
    }
    for (const auto& [tenant, entries] : due) {
        try {
            repository::feed_binding_activity_repository{}.record(batch.tenant_ctxs.at(tenant),
                                                                  entries);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to record the activity of " << entries.size()
                                      << " feed binding(s): " << e.what();
        }
    }
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
    series.provider_code = binding.provider_code;
    series.modified_by = service_account;
    series.performed_by = service_account;
    series.change_reason_code = "system.initial_load";
    series.change_commentary = "Created by the feed ingest loop for source " + binding.source_name;
    return series;
}

std::expected<std::shared_ptr<const named_datum>, tick_drop>
feed_ingest_loop::name_of(const std::string& uri) {
    {
        std::lock_guard lock(mu_);
        const auto found = named_.find(uri);
        if (found != named_.end())
            return found->second;
    }
    auto read = name_datum(uri);
    if (!read)
        return read;
    std::lock_guard lock(mu_);
    if (named_.size() < named_cache_limit)
        named_.emplace(uri, *read);
    return read;
}

boost::uuids::uuid feed_ingest_loop::series_id_for(const ores::database::context& tenant_ctx,
                                                   const domain::feed_binding& binding,
                                                   const named_datum& named,
                                                   boost::uuids::random_generator& uuid_gen) {
    const auto& series_uri = named.series_uri;
    const auto find_cached = [&]() -> std::optional<boost::uuids::uuid> {
        const auto found = series_ids_.find(series_uri);
        if (found == series_ids_.end())
            return std::nullopt;
        for (const auto& entry : found->second)
            if (entry.tenant_id == binding.tenant_id.to_uuid() &&
                entry.party_id == binding.party_id && entry.provider_code == binding.provider_code)
                return entry.id;
        return std::nullopt;
    };
    {
        std::lock_guard lock(mu_);
        if (const auto cached = find_cached())
            return *cached;
    }
    const auto party = boost::uuids::to_string(binding.party_id);

    repository::market_series_repository series_repo;
    auto existing =
        series_repo.read_latest_by_key(tenant_ctx, series_uri, binding.provider_code, party);
    if (existing.empty()) {
        const auto ck = core::classification_key_of(named.datum);
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
    series_ids_[series_uri].push_back(
        {binding.tenant_id.to_uuid(), binding.party_id, binding.provider_code, id});
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
        << " ticks_stored=" << ticks_stored_.load(std::memory_order_relaxed)
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
