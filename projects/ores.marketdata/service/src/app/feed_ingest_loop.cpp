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
#include "ores.marketdata.service/app/tick_plan.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <optional>
#include <string_view>

namespace ores::marketdata::service::app {

using namespace ores::logging;

feed_ingest_loop::feed_ingest_loop(ores::nats::service::client& nats,
                                   ores::database::context ctx,
                                   std::shared_ptr<crm_ingest_bridge> crm_bridge)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , crm_bridge_(std::move(crm_bridge)) {}

feed_ingest_loop::~feed_ingest_loop() {
    stop_flag_.store(true, std::memory_order_relaxed);
    if (status_thread_.joinable())
        status_thread_.join();
}

void feed_ingest_loop::start() {
    const auto wildcard = domain::synthetic_tick_wildcard();
    BOOST_LOG_SEV(lg(), info) << "Starting feed ingest loop: subscribing to '" << wildcard << "'";
    refresh();
    tick_sub_ = nats_.subscribe(wildcard, [this](ores::nats::message msg) { on_tick(msg); });
    status_thread_ = std::thread(&feed_ingest_loop::status_loop, this);
}

void feed_ingest_loop::refresh() {
    repository::feed_binding_repository repo;
    const auto bindings = repo.read_latest_all_tenants(ctx_);

    std::map<std::string, std::vector<domain::feed_binding>> wanted;
    for (const auto& b : bindings) {
        if (b.enabled)
            wanted[b.source_name].push_back(b);
    }

    std::lock_guard lock(mu_);
    bindings_by_source_ = std::move(wanted);
    unbound_warned_.clear();
    unnameable_warned_.clear();
    bad_value_warned_.clear();
    classifiers_.clear();

    // A stats entry per bound consumer, so a binding that never ticks still
    // shows in INGEST STATUS.
    std::map<binding_key, std::shared_ptr<feed_stats>> kept;
    for (const auto& [source_name, source_bindings] : bindings_by_source_) {
        for (const auto& b : source_bindings) {
            const binding_key key{b.source_name,
                                  b.tenant_id.to_string(),
                                  boost::uuids::to_string(b.party_id)};
            const auto it = stats_.find(key);
            kept[key] = it != stats_.end() ? it->second : std::make_shared<feed_stats>();
        }
    }
    stats_ = std::move(kept);

    BOOST_LOG_SEV(lg(), info) << "Feed ingest loop: " << bindings_by_source_.size()
                              << " bound source(s)";
}

void feed_ingest_loop::on_tick(const ores::nats::message& msg) {
    const auto tick = ores::nats::default_wire_codec().decode<messaging::market_tick>(msg.data);
    if (!tick) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to decode market_tick on '" << msg.subject
                                  << "': " << tick.error().what();
        return;
    }

    std::vector<domain::feed_binding> bindings;
    {
        std::lock_guard lock(mu_);
        const auto it = bindings_by_source_.find(tick->source);
        if (it != bindings_by_source_.end())
            bindings = it->second;
    }

    const auto plan = plan_tick(*tick, bindings);
    if (!plan) {
        std::lock_guard lock(mu_);
        switch (plan.error()) {
            case tick_drop::unbound_source:
                if (unbound_warned_.insert(tick->source).second)
                    BOOST_LOG_SEV(lg(), warn) << "Dropping ticks for unbound source '"
                                              << tick->source << "': no enabled feed_binding";
                break;
            case tick_drop::unnameable_datum:
                if (unnameable_warned_.insert(tick->oresmd_uri).second)
                    BOOST_LOG_SEV(lg(), warn)
                        << "Dropping ticks for '" << tick->oresmd_uri << "': it names no ORE datum";
                break;
            case tick_drop::not_a_number:
                if (bad_value_warned_.insert(tick->source).second)
                    BOOST_LOG_SEV(lg(), warn) << "Dropping ticks from source '" << tick->source
                                              << "': value '" << tick->value << "' is not a number";
                break;
        }
        return;
    }

    for (const auto& target : plan->targets) {
        const auto& b = target.binding;
        if (!persist(b, plan->datum, *tick))
            continue;

        const binding_key key{b.source_name,
                              b.tenant_id.to_string(),
                              boost::uuids::to_string(b.party_id)};
        std::uint64_t prev_count = 0;
        {
            std::lock_guard lock(mu_);
            auto& st = stats_[key];
            if (!st)
                st = std::make_shared<feed_stats>();
            prev_count = st->tick_count.fetch_add(1, std::memory_order_relaxed);
            st->last_tick_rep.store(std::chrono::system_clock::now().time_since_epoch().count(),
                                    std::memory_order_relaxed);
        }

        if (prev_count == 0)
            BOOST_LOG_SEV(lg(), info)
                << "INGEST FIRST TICK: source='" << b.source_name << "' datum='" << tick->oresmd_uri
                << "' subject='" << target.subject << "' value=" << tick->value;

        // Currency driver pairs are FX-shaped, so only an FX spot rate is a
        // candidate driver update. The bridge offers it tenant-wide.
        if (crm_bridge_ && plan->datum.type() == datum::instrument_type::fx_spot)
            crm_bridge_->update(b.tenant_id.to_string(),
                                *plan->datum.get<datum::field::unit_ccy>(),
                                *plan->datum.get<datum::field::ccy>(),
                                plan->value,
                                tick->observation_time);
        nats_.js_publish(target.subject, msg.data);
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

bool feed_ingest_loop::persist(const domain::feed_binding& binding,
                               const datum::market_datum& tick_datum,
                               const messaging::market_tick& tick) {
    // Local generator per call: the subscription dispatches concurrently.
    boost::uuids::random_generator uuid_gen;
    try {
        auto tenant_ctx = ctx_.with_tenant(binding.tenant_id, "ores.marketdata.service");
        const auto party = boost::uuids::to_string(binding.party_id);
        const auto series_uri =
            datum::oresmd_uri_codec::write(datum::series_of(tick_datum)).value();

        repository::market_series_repository series_repo;
        auto existing = series_repo.read_latest_by_uri(tenant_ctx, series_uri, party);
        if (existing.empty()) {
            const auto ck = core::classification_key_of(tick_datum);
            const auto cl =
                classifier_for(tenant_ctx)->classify(ck.series_type, ck.metric, ck.qualifier);
            BOOST_LOG_SEV(lg(), info) << "Creating market series " << series_uri;

            domain::market_series series;
            series.id = uuid_gen();
            series.tenant_id = tenant_ctx.tenant_id();
            series.party_id = binding.party_id;
            series.oresmd_uri = series_uri;
            series.series_subclass = cl.series_subclass;
            series.modified_by = ctx_.service_account();
            series.performed_by = ctx_.service_account();
            series.change_reason_code = "system.initial_load";
            series.change_commentary = "Created by the feed ingest loop for source " + tick.source;
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

        domain::market_observation obs;
        obs.id = uuid_gen();
        obs.tenant_id = tenant_ctx.tenant_id();
        obs.party_id = binding.party_id;
        obs.series_id = existing.front().id;
        obs.observation_datetime = tick.observation_time;
        obs.value = tick.value;
        obs.source = tick.source;
        obs.oresmd_uri = tick.oresmd_uri;
        repository::market_observation_repository{}.insert(tenant_ctx, obs);
        return true;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to store the tick for " << tick.oresmd_uri
                                   << " from source '" << tick.source << "': " << e.what();
        return false;
    }
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
                                                 " last_tick={}s ago",
                                                 duration_cast<seconds>(now - last_tp).count()) :
                                             std::string(" last_tick=never"));
    }
}

}
