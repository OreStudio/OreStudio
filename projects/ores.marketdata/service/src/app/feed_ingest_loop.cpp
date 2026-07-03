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
#include "ores.marketdata.api/domain/fx_spot_tick.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.core/repository/feed_binding_repository.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <format>
#include <rfl/enums.hpp>
#include <rfl/json.hpp>
#include <set>
#include <stdexcept>

namespace ores::marketdata::service::app {

using namespace ores::logging;

namespace {

// "FX/RATE/EUR/USD" → "marketdata.v1.tick.fx.rate.eur.usd"
std::string ore_key_to_publish_subject(const std::string& tenant_id_str, std::string ore_key) {
    std::transform(ore_key.begin(), ore_key.end(), ore_key.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    std::replace(ore_key.begin(), ore_key.end(), '/', '.');
    return "marketdata.v1.tick." + tenant_id_str + "." + ore_key;
}

// "FX/RATE/EUR/USD" → {series_type="FX", metric="RATE", qualifier="EUR/USD"}
struct ore_key_parts {
    std::string series_type, metric, qualifier;
};
ore_key_parts parse_ore_key(const std::string& ore_key) {
    ore_key_parts p;
    std::istringstream ss(ore_key);
    std::string tok;
    std::vector<std::string> parts;
    while (std::getline(ss, tok, '/'))
        parts.push_back(tok);
    if (parts.size() < 3)
        throw std::invalid_argument("Unparseable ORE key: " + ore_key);
    p.series_type = parts[0];
    p.metric = parts[1];
    p.qualifier = parts[2];
    for (std::size_t i = 3; i < parts.size(); ++i)
        p.qualifier += "/" + parts[i];
    return p;
}

} // namespace

feed_ingest_loop::feed_ingest_loop(ores::nats::service::client& nats, ores::database::context ctx)
    : nats_(nats)
    , ctx_(std::move(ctx)) {}

feed_ingest_loop::~feed_ingest_loop() {
    stop_flag_.store(true, std::memory_order_relaxed);
    if (status_thread_.joinable())
        status_thread_.join();
}

void feed_ingest_loop::start() {
    BOOST_LOG_SEV(lg(), info) << "Starting feed ingest loop";
    refresh();
    status_thread_ = std::thread(&feed_ingest_loop::status_loop, this);
}

void feed_ingest_loop::refresh() {
    // DB read outside the lock — no shared state touched.
    repository::feed_binding_repository repo;
    const auto bindings = repo.read_latest_all_tenants(ctx_);

    std::lock_guard lock(mu_);

    // Build the set of source_names that should be active
    std::set<std::string> wanted;
    for (const auto& b : bindings)
        if (b.enabled)
            wanted.insert(b.source_name);

    // Unsubscribe anything no longer wanted
    std::vector<std::string> to_remove;
    for (const auto& [source_name, _] : subs_)
        if (!wanted.contains(source_name))
            to_remove.push_back(source_name);
    for (const auto& s : to_remove)
        unsubscribe_binding_locked(s);

    // Subscribe anything new
    for (const auto& b : bindings) {
        if (b.enabled && !subs_.contains(b.source_name))
            subscribe_binding_locked(b.ore_key,
                                     b.source_name,
                                     b.tenant_id.to_string(),
                                     boost::uuids::to_string(b.party_id));
    }

    BOOST_LOG_SEV(lg(), info) << "Feed ingest loop: " << subs_.size() << " active subscription(s)";
}

// Called only from refresh(), which holds mu_.
void feed_ingest_loop::subscribe_binding_locked(const std::string& ore_key,
                                                const std::string& source_name,
                                                const std::string& tenant_id_str,
                                                const std::string& party_id_str) {
    const std::string producer_subject = "synthetic.v1.tick." + source_name;
    const std::string publish_subject = ore_key_to_publish_subject(tenant_id_str, ore_key);
    const std::string ore_key_copy = ore_key;

    BOOST_LOG_SEV(lg(), info) << "INGEST SUBSCRIBE: source='" << source_name << "' listening on '"
                              << producer_subject << "' → republishing on '" << publish_subject
                              << "'";

    auto st = std::make_shared<feed_stats>();
    st->ore_key = ore_key;
    st->nats_subject = producer_subject;
    stats_.emplace(source_name, st); // mu_ already held by caller (refresh)

    boost::uuids::random_generator uuid_gen;
    const auto party_uuid = boost::lexical_cast<boost::uuids::uuid>(party_id_str);

    // Plain subscribe (fan-out) rather than queue_subscribe: this service
    // runs as a single instance. If horizontal scaling is ever needed,
    // switch to queue_subscribe("ores.marketdata.service") to avoid
    // duplicate observations and duplicate republish.
    auto tenant_ctx =
        ctx_.with_tenant(ores::utility::uuid::tenant_id::from_string(tenant_id_str).value(),
                         "ores.marketdata.service");
    auto sub = nats_.subscribe(
        producer_subject,
        [this,
         ore_key_copy,
         publish_subject,
         source_name,
         uuid_gen,
         st,
         party_uuid,
         tenant_ctx = std::move(tenant_ctx)](ores::nats::message msg) mutable {
            auto tick = rfl::json::read<domain::fx_spot_tick>(ores::nats::as_string_view(msg.data));
            if (!tick) {
                BOOST_LOG_SEV(lg(), warn)
                    << "Failed to decode fx_spot_tick: " << tick.error().what();
                return;
            }

            const auto now_rep = std::chrono::system_clock::now().time_since_epoch().count();
            const auto prev_count = st->tick_count.fetch_add(1, std::memory_order_relaxed);
            st->last_tick_rep.store(now_rep, std::memory_order_relaxed);

            if (prev_count == 0) {
                BOOST_LOG_SEV(lg(), info)
                    << "INGEST FIRST TICK: source='" << ore_key_copy << "' subject='"
                    << publish_subject << "' mid=" << tick->mid;
            }

            // Persist the observation
            try {
                const auto kp = parse_ore_key(ore_key_copy);
                repository::market_series_repository series_repo;
                auto existing =
                    series_repo.read_latest_by_type(tenant_ctx,
                                                    kp.series_type,
                                                    kp.metric,
                                                    kp.qualifier,
                                                    boost::uuids::to_string(party_uuid));
                if (existing.empty()) {
                    BOOST_LOG_SEV(lg(), info) << "Auto-creating market series for " << ore_key_copy;
                    std::string ac_str = kp.series_type;
                    std::transform(
                        ac_str.begin(), ac_str.end(), ac_str.begin(), [](unsigned char c) {
                            return static_cast<char>(std::tolower(c));
                        });
                    domain::market_series series;
                    series.id = uuid_gen();
                    series.tenant_id = tenant_ctx.tenant_id();
                    series.party_id = party_uuid;
                    series.series_type = kp.series_type;
                    series.metric = kp.metric;
                    series.qualifier = kp.qualifier;
                    series.asset_class = rfl::string_to_enum<domain::asset_class>(ac_str).value_or(
                        domain::asset_class::fx);
                    series.series_subclass =
                        rfl::string_to_enum<domain::series_subclass>(ac_str).value_or(
                            domain::series_subclass::spot);
                    series.is_scalar = true;
                    series.modified_by = ctx_.service_account();
                    series.performed_by = ctx_.service_account();
                    series.change_reason_code = "system.initial_load";
                    series.change_commentary = ore_key_copy + " synthetic feed auto-created";
                    series_repo.write(tenant_ctx, series);
                    existing.push_back(std::move(series));
                }

                domain::market_observation obs;
                obs.id = uuid_gen();
                obs.tenant_id = tenant_ctx.tenant_id();
                obs.party_id = party_uuid;
                obs.series_id = existing.front().id;
                obs.observation_datetime = tick->datetime;
                obs.value = std::to_string(tick->mid);
                obs.source = source_name;
                obs.point_id = "SPOT"; // scalar FX spot series has no tenor/surface coordinate

                repository::market_observations_repository obs_repo;
                obs_repo.write(tenant_ctx, obs);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error)
                    << "Failed to persist observation for " << ore_key_copy << ": " << e.what();
            }

            nats_.js_publish(publish_subject, msg.data);
        });

    subs_.emplace(source_name, std::move(sub));
}

// Called only from refresh(), which holds mu_.
void feed_ingest_loop::unsubscribe_binding_locked(const std::string& source_name) {
    BOOST_LOG_SEV(lg(), info) << "INGEST UNSUBSCRIBE: source='" << source_name << "'";
    subs_.erase(source_name);
    stats_.erase(source_name); // mu_ already held by caller (refresh)
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
    for (const auto& [source, st] : stats_) {
        const auto count = st->tick_count.load(std::memory_order_relaxed);
        const auto last_rep = st->last_tick_rep.load(std::memory_order_relaxed);
        const auto last_tp = system_clock::time_point{system_clock::duration{last_rep}};
        const bool ever = (last_tp != system_clock::time_point::min());
        const auto age_s = ever ? duration_cast<seconds>(now - last_tp).count() : -1LL;

        BOOST_LOG_SEV(lg(), info) << "INGEST STATUS: source='" << source << "' ore_key='"
                                  << st->ore_key << "' subject='" << st->nats_subject
                                  << "' ticks=" << count
                                  << (ever ? std::format(" last_tick={}s ago", age_s) :
                                             " last_tick=never");
    }
}

} // namespace ores::marketdata::service::app
