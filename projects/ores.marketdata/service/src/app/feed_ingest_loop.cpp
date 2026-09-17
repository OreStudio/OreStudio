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
#include "ores.marketdata.api/domain/ir_curve_tick.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
#include "ores.marketdata.core/repository/feed_binding_repository.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.ore.core/market/series_key_registry.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <format>
#include <set>
#include <stdexcept>
#include <string_view>
#include <vector>

namespace ores::marketdata::service::app {

using namespace ores::logging;

namespace {

// One wildcard over the unified tick family: synthetic.v1.tick.<kind>.<source_name>.
// '>' (multi-token wildcard), not '*' (single-token): source_name is dotted
// (e.g. "ir_curve.usd.sofr"), so the subject has more than one token after the
// prefix. Sandboxed feeds publish under synthetic.v1.sandbox.tick., which this
// prefix never matches.
constexpr auto* unified_wildcard_subject = "synthetic.v1.tick.>";

// "FX/RATE/EUR/USD" →
// "marketdata.v1.tick.<tenant>.<workspace>.<party>.fx.rate.eur.usd"
std::string ore_key_to_publish_subject(const std::string& tenant_id_str,
                                       const std::string& workspace_id_str,
                                       const std::string& party_id_str,
                                       std::string ore_key) {
    std::transform(ore_key.begin(), ore_key.end(), ore_key.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    std::replace(ore_key.begin(), ore_key.end(), '/', '.');
    return "marketdata.v1.tick." + tenant_id_str + "." + workspace_id_str + "." + party_id_str +
           "." + ore_key;
}

// "FX/RATE/EUR/USD" → {series_type="FX", metric="RATE", qualifier="EUR/USD"} -- thin
// throwing adapter over the shared oresmd split, preserving this file's existing
// try/catch-based error handling at both call sites.
ores::marketdata::core::market_series_key parse_ore_key(const std::string& ore_key) {
    auto kp = ores::marketdata::core::oresmd_projections::split_market_series_key(ore_key);
    if (!kp)
        throw std::invalid_argument("Unparseable ORE key: " + ore_key);
    return *kp;
}

} // namespace

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
    BOOST_LOG_SEV(lg(), info) << "Starting feed ingest loop: subscribing to '"
                              << unified_wildcard_subject << "'";
    tick_sub_ = nats_.subscribe(unified_wildcard_subject,
                                [this](ores::nats::message msg) { on_tick(msg); });
    refresh();
    status_thread_ = std::thread(&feed_ingest_loop::status_loop, this);
}

void feed_ingest_loop::refresh() {
    // DB read outside the lock — no shared state touched.
    repository::feed_binding_repository repo;
    const auto bindings = repo.read_latest_all_tenants(ctx_);

    std::map<std::string, std::vector<domain::feed_binding>> wanted;
    for (const auto& b : bindings) {
        if (!b.enabled)
            continue;
        wanted[b.source_name].push_back(b);
    }

    std::lock_guard lock(mu_);
    bindings_by_source_ = std::move(wanted);
    unbound_warned_.clear();

    // Keep a stats entry per bound consumer, so a binding that never ticks is
    // still visible in INGEST STATUS. Entries whose binding went away are
    // dropped.
    std::map<feed_ingest_loop::binding_key, std::shared_ptr<feed_stats>> kept;
    for (const auto& [source_name, source_bindings] : bindings_by_source_) {
        for (const auto& b : source_bindings) {
            feed_ingest_loop::binding_key key{b.source_name,
                                              b.tenant_id.to_string(),
                                              boost::uuids::to_string(b.party_id),
                                              boost::uuids::to_string(b.workspace_id)};
            const auto it = fx_stats_.find(key);
            auto& st = kept[key];
            st = (it != fx_stats_.end()) ? it->second : std::make_shared<feed_stats>();
            if (st->series_identity.empty()) {
                st->series_identity = b.ore_key;
                st->nats_subject = ores::marketdata::domain::synthetic_tick_subject(
                    ores::marketdata::domain::fx_spot_kind_token, source_name);
                st->publish_subject = ore_key_to_publish_subject(
                    key.tenant_id, key.workspace_id, key.party_id, b.ore_key);
            }
        }
    }
    fx_stats_ = std::move(kept);

    BOOST_LOG_SEV(lg(), info) << "Feed ingest loop: " << bindings_by_source_.size()
                              << " bound source(s)";
}

void feed_ingest_loop::ingest_bound_tick(ores::nats::message msg, const std::string& source_name) {
    auto tick = ores::nats::default_wire_codec().decode<domain::fx_spot_tick>(msg.data);
    if (!tick) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to decode fx_spot_tick: " << tick.error().what();
        return;
    }

    std::vector<domain::feed_binding> bindings;
    {
        std::lock_guard lock(mu_);
        const auto it = bindings_by_source_.find(source_name);
        if (it == bindings_by_source_.end()) {
            if (unbound_warned_.insert(source_name).second)
                BOOST_LOG_SEV(lg(), warn) << "Dropping tick for unbound source '" << source_name
                                          << "' - no enabled feed_binding";
            return;
        }
        bindings = it->second;
    }

    for (const auto& b : bindings) {
        const feed_ingest_loop::binding_key key{b.source_name,
                                                b.tenant_id.to_string(),
                                                boost::uuids::to_string(b.party_id),
                                                boost::uuids::to_string(b.workspace_id)};
        const std::string publish_subject =
            ore_key_to_publish_subject(key.tenant_id, key.workspace_id, key.party_id, b.ore_key);

        const auto now_rep = std::chrono::system_clock::now().time_since_epoch().count();
        std::uint64_t prev_count = 0;
        {
            std::lock_guard lock(mu_);
            auto& st = fx_stats_[key];
            if (!st) {
                st = std::make_shared<feed_stats>();
                st->series_identity = b.ore_key;
                st->nats_subject = ores::marketdata::domain::synthetic_tick_subject(
                    ores::marketdata::domain::fx_spot_kind_token, source_name);
                st->publish_subject = publish_subject;
            }
            prev_count = st->tick_count.fetch_add(1, std::memory_order_relaxed);
            st->last_tick_rep.store(now_rep, std::memory_order_relaxed);
        }

        if (prev_count == 0) {
            BOOST_LOG_SEV(lg(), info) << "INGEST FIRST TICK: source='" << b.ore_key << "' subject='"
                                      << publish_subject << "' mid=" << tick->mid;
        }

        // Persist the observation; the republish below is gated on this
        // write, so the republished stream cannot diverge from the
        // observations table.
        bool persisted = false;
        try {
            const auto kp = parse_ore_key(b.ore_key);
            // The subclass is spot by construction of the fx_spot producer
            // kind. The point is left to the series type's own answer, which
            // is SPOT for FX.
            persisted = persist_tick_observation(ctx_,
                                                 b.tenant_id,
                                                 b.party_id,
                                                 kp.series_type,
                                                 kp.metric,
                                                 kp.qualifier,
                                                 b.asset_class,
                                                 "spot",
                                                 tick->datetime,
                                                 std::to_string(tick->mid),
                                                 source_name,
                                                 {});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to persist observation for " << b.ore_key << ": " << e.what();
        }

        // Offer the tick to the CRM as a candidate driver update,
        // tenant-wide -- every party in the tenant with a matching
        // driver edge gets it, not just the party that owns this
        // feed_binding; see crm_ingest_bridge's own class doc for why.
        // A no-op if no party in this tenant has a CRM configured, or
        // the pair isn't a driver edge of any of them. With per-party
        // bindings of the same source the offer is made once per party;
        // that is harmless -- the bridge update only sets the in-memory
        // driver quote to the same value. Currency driver pairs are
        // FX-shaped, so the bridge is an fx_spot concern only.
        if (crm_bridge_) {
            try {
                const auto kp = parse_ore_key(b.ore_key);
                if (kp.series_type == "FX" && kp.metric == "RATE") {
                    const auto slash = kp.qualifier.find('/');
                    if (slash != std::string::npos) {
                        crm_bridge_->update(b.tenant_id.to_string(),
                                            kp.qualifier.substr(0, slash),
                                            kp.qualifier.substr(slash + 1),
                                            tick->mid,
                                            tick->datetime);
                    }
                }
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn)
                    << "CRM update failed for " << b.ore_key << ": " << e.what();
            }
        }

        if (persisted)
            nats_.js_publish(publish_subject, msg.data);
    }
}

void feed_ingest_loop::on_tick(const ores::nats::message& msg) {
    // "synthetic.v1.tick.<kind>.<source_name>" — the kind token is the segment
    // right after the shared prefix.
    const auto kind_begin = ores::marketdata::domain::synthetic_tick_subject_prefix.size();
    const auto kind_end = msg.subject.find('.', kind_begin);
    if (kind_end == std::string::npos) {
        BOOST_LOG_SEV(lg(), warn) << "Malformed tick subject: '" << msg.subject << "'";
        return;
    }
    const std::string_view kind(msg.subject.data() + kind_begin, kind_end - kind_begin);
    const std::string_view source_name(msg.subject.data() + kind_end + 1);

    if (kind == ores::marketdata::domain::ir_curve_kind_token)
        ingest_ir_curve(msg);
    else if (kind == ores::marketdata::domain::fx_spot_kind_token)
        ingest_bound_tick(msg, std::string(source_name));
    else
        BOOST_LOG_SEV(lg(), warn) << "Unknown tick kind '" << kind << "' on subject '"
                                  << msg.subject << "'";
}

void feed_ingest_loop::ingest_ir_curve(const ores::nats::message& msg) {
    auto tick = ores::nats::default_wire_codec().decode<domain::ir_curve_tick>(msg.data);
    if (!tick) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to decode ir_curve_tick: " << tick.error().what();
        return;
    }

    const std::string ore_key = tick->series_type + "/" + tick->metric + "/" + tick->qualifier;
    // ir_curve_tick is self-describing but carries no workspace; the Live
    // sentinel is the consuming party's workspace, like the binding path.
    const std::string publish_subject =
        ore_key_to_publish_subject(tick->tenant_id.to_string(),
                                   boost::uuids::to_string(utility::uuid::live_workspace_id()),
                                   boost::uuids::to_string(tick->party_id),
                                   ore_key);

    const auto now_rep = std::chrono::system_clock::now().time_since_epoch().count();
    std::shared_ptr<feed_stats> st;
    {
        std::lock_guard lock(mu_);
        st = ir_stats_
                 .try_emplace(std::pair{std::string(ores::marketdata::domain::ir_curve_kind_token),
                                        tick->source_name},
                              std::make_shared<feed_stats>())
                 .first->second;
        if (st->series_identity.empty()) {
            st->series_identity = ore_key;
            st->nats_subject = msg.subject;
            st->publish_subject = publish_subject;
        }
    }
    const auto prev_count = st->tick_count.fetch_add(1, std::memory_order_relaxed);
    st->last_tick_rep.store(now_rep, std::memory_order_relaxed);

    if (prev_count == 0) {
        BOOST_LOG_SEV(lg(), info) << "INGEST FIRST TICK: series='" << st->series_identity
                                  << "' subject='" << msg.subject << "' value=" << tick->value;
    }

    const bool persisted = persist_tick_observation(ctx_,
                                                    tick->tenant_id,
                                                    tick->party_id,
                                                    tick->series_type,
                                                    tick->metric,
                                                    tick->qualifier,
                                                    tick->asset_class,
                                                    tick->subclass,
                                                    tick->datetime,
                                                    std::to_string(tick->value),
                                                    tick->source_name,
                                                    tick->point_id);
    if (persisted)
        nats_.js_publish(publish_subject, msg.data);
}

bool feed_ingest_loop::persist_tick_observation(const ores::database::context& ctx,
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
                                                const std::string& point_id) {
    // Local generator per call: the per-party subscriptions dispatch
    // callbacks concurrently, so a shared generator would race.
    boost::uuids::random_generator uuid_gen;
    try {
        auto tenant_ctx = ctx.with_tenant(tenant_id, "ores.marketdata.service");

        const std::string ore_key = series_type + "/" + metric + "/" + qualifier;
        repository::market_series_repository series_repo;
        auto existing = series_repo.read_latest_by_type(
            tenant_ctx, series_type, metric, qualifier, boost::uuids::to_string(party_id));
        if (existing.empty()) {
            BOOST_LOG_SEV(lg(), info) << "Auto-creating market series for " << ore_key;

            domain::market_series series;
            series.id = uuid_gen();
            series.tenant_id = tenant_ctx.tenant_id();
            series.party_id = party_id;
            series.series_type = series_type;
            series.metric = metric;
            series.qualifier = qualifier;
            series.asset_class = asset_class;
            series.series_subclass = series_subclass;
            series.modified_by = ctx.service_account();
            series.performed_by = ctx.service_account();
            series.change_reason_code = "system.initial_load";
            series.change_commentary = ore_key + " synthetic feed auto-created";
            series_repo.write(tenant_ctx, series);
            existing.push_back(std::move(series));
        }

        domain::market_observation obs;
        obs.id = uuid_gen();
        obs.tenant_id = tenant_ctx.tenant_id();
        obs.party_id = party_id;
        obs.series_id = existing.front().id;
        obs.observation_datetime = datetime;
        obs.value = value;
        obs.source = source;
        // A producer that names no point takes the series type's own answer.
        obs.point_id =
            point_id.empty() ? ores::ore::market::default_point_for(series_type) : point_id;

        repository::market_observations_repository obs_repo;
        obs_repo.write(tenant_ctx, obs);
        return true;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to persist observation for " << series_type << "/"
                                   << metric << "/" << qualifier << ": " << e.what();
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
    if (fx_stats_.empty() && ir_stats_.empty()) {
        BOOST_LOG_SEV(lg(), info) << "INGEST STATUS: no active subscriptions";
        return;
    }
    const auto now = system_clock::now();
    for (const auto& [key, st] : fx_stats_) {
        const auto count = st->tick_count.load(std::memory_order_relaxed);
        const auto last_rep = st->last_tick_rep.load(std::memory_order_relaxed);
        const auto last_tp = system_clock::time_point{system_clock::duration{last_rep}};
        const bool ever = (last_tp != system_clock::time_point::min());
        const auto age_s = ever ? duration_cast<seconds>(now - last_tp).count() : -1LL;

        BOOST_LOG_SEV(lg(), info) << "INGEST STATUS: source='" << key.source_name << "' tenant='"
                                  << key.tenant_id << "' party='" << key.party_id << "' workspace='"
                                  << key.workspace_id << "' ore_key='" << st->series_identity
                                  << "' subject='" << st->nats_subject << "' publish='"
                                  << st->publish_subject << "' ticks=" << count
                                  << (ever ? std::format(" last_tick={}s ago", age_s) :
                                             " last_tick=never");
    }
    for (const auto& [key, st] : ir_stats_) {
        const auto& [kind, source] = key;
        const auto count = st->tick_count.load(std::memory_order_relaxed);
        const auto last_rep = st->last_tick_rep.load(std::memory_order_relaxed);
        const auto last_tp = system_clock::time_point{system_clock::duration{last_rep}};
        const bool ever = (last_tp != system_clock::time_point::min());
        const auto age_s = ever ? duration_cast<seconds>(now - last_tp).count() : -1LL;

        BOOST_LOG_SEV(lg(), info) << "INGEST STATUS: kind='" << kind << "' source='" << source
                                  << "' series='" << st->series_identity << "' subject='"
                                  << st->nats_subject << "' ticks=" << count
                                  << (ever ? std::format(" last_tick={}s ago", age_s) :
                                             " last_tick=never");
    }
}

} // namespace ores::marketdata::service::app
