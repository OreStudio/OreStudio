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
#include "ores.marketdata.api/domain/asset_class.hpp"
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
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <format>
#include <rfl/enums.hpp>
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
    ir_sub_ = nats_.subscribe(unified_wildcard_subject,
                              [this](ores::nats::message msg) { on_tick(msg); });
    refresh();
    status_thread_ = std::thread(&feed_ingest_loop::status_loop, this);
}

void feed_ingest_loop::refresh() {
    // DB read outside the lock — no shared state touched.
    repository::feed_binding_repository repo;
    const auto bindings = repo.read_latest_all_tenants(ctx_);

    std::lock_guard lock(mu_);

    // Build the set of subscription keys that should be active: one per
    // (source_name, tenant, party, workspace). The same source_name can
    // appear in many bindings -- every party consumes the shared stream.
    std::set<feed_ingest_loop::subscription_key> wanted;
    for (const auto& b : bindings) {
        if (!b.enabled)
            continue;
        if (b.asset_class != domain::asset_class::fx) {
            // ir_curve ticks are self-describing and need no binding; a
            // non-FX binding would subscribe to the wrong payload type.
            BOOST_LOG_SEV(lg(), warn) << "Skipping non-FX feed binding for '"
                                      << b.source_name << "'";
            continue;
        }
        wanted.insert(feed_ingest_loop::subscription_key{
            b.source_name,
            b.tenant_id.to_string(),
            boost::uuids::to_string(b.party_id),
            boost::uuids::to_string(b.workspace_id)});
    }

    // Unsubscribe anything no longer wanted
    std::vector<feed_ingest_loop::subscription_key> to_remove;
    for (const auto& [key, _] : subs_)
        if (!wanted.contains(key))
            to_remove.push_back(key);
    for (const auto& k : to_remove)
        unsubscribe_binding_locked(k);

    // Subscribe anything new
    for (const auto& b : bindings) {
        if (!b.enabled || b.asset_class != domain::asset_class::fx)
            continue;
        const feed_ingest_loop::subscription_key key{
            b.source_name,
            b.tenant_id.to_string(),
            boost::uuids::to_string(b.party_id),
            boost::uuids::to_string(b.workspace_id)};
        if (!subs_.contains(key))
            subscribe_binding_locked(key, b.ore_key);
    }

    bound_sources_.clear();
    for (const auto& k : wanted)
        bound_sources_.insert(k.source_name);

    BOOST_LOG_SEV(lg(), info) << "Feed ingest loop: " << subs_.size() << " active subscription(s)";
}

// Called only from refresh(), which holds mu_.
void feed_ingest_loop::subscribe_binding_locked(const subscription_key& key,
                                                const std::string& ore_key) {
    const std::string producer_subject = ores::marketdata::domain::synthetic_tick_subject(
        ores::marketdata::domain::fx_spot_kind_token, key.source_name);
    const std::string source_name = key.source_name;
    const std::string publish_subject = ore_key_to_publish_subject(
        key.tenant_id, key.workspace_id, key.party_id, ore_key);
    const std::string ore_key_copy = ore_key;

    BOOST_LOG_SEV(lg(), info) << "INGEST SUBSCRIBE: source='" << key.source_name
                              << "' tenant='" << key.tenant_id << "' party='" << key.party_id
                              << "' workspace='" << key.workspace_id << "' listening on '"
                              << producer_subject << "' → republishing on '" << publish_subject
                              << "'";

    auto st = std::make_shared<feed_stats>();
    st->series_identity = ore_key;
    st->nats_subject = producer_subject;
    st->publish_subject = publish_subject;
    fx_stats_.emplace(key, st); // mu_ already held by caller (refresh)

    const auto party_uuid = boost::lexical_cast<boost::uuids::uuid>(key.party_id);
    const auto tenant_id = ores::utility::uuid::tenant_id::from_string(key.tenant_id).value();

    // Plain subscribe (fan-out) rather than queue_subscribe: this service
    // runs as a single instance. If horizontal scaling is ever needed,
    // switch to queue_subscribe("ores.marketdata.service") to avoid
    // duplicate observations and duplicate republish.
    auto sub = nats_.subscribe(
        producer_subject,
        [this,
         ore_key_copy,
         publish_subject,
         source_name,
         st,
         party_uuid,
         tenant_id](ores::nats::message msg) {
            auto tick = ores::nats::default_wire_codec().decode<domain::fx_spot_tick>(msg.data);
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

            // Persist the observation; the republish below is gated on this
            // write, so the republished stream cannot diverge from the
            // observations table.
            bool persisted = false;
            try {
                const auto kp = parse_ore_key(ore_key_copy);
                // Scalar FX spot series: no curve coordinate.
                persisted = persist_tick_observation(ctx_,
                                                     tenant_id,
                                                     party_uuid,
                                                     kp.series_type,
                                                     kp.metric,
                                                     kp.qualifier,
                                                     std::nullopt,
                                                     true,
                                                     tick->datetime,
                                                     std::to_string(tick->mid),
                                                     source_name,
                                                     "SPOT");
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error)
                    << "Failed to persist observation for " << ore_key_copy << ": " << e.what();
            }

            // Offer the tick to the CRM as a candidate driver update,
            // tenant-wide -- every party in the tenant with a matching
            // driver edge gets it, not just the party that owns this
            // feed_binding; see crm_ingest_bridge's own class doc for why.
            // A no-op if no party in this tenant has a CRM configured, or
            // the pair isn't a driver edge of any of them. With per-party
            // subscriptions the offer is made once per party binding of the
            // same source; that is harmless -- the bridge update only sets
            // the in-memory driver quote to the same value. Currency driver
            // pairs are FX-shaped, so the bridge is an fx_spot concern only.
            if (crm_bridge_) {
                try {
                    const auto kp = parse_ore_key(ore_key_copy);
                    if (kp.series_type == "FX" && kp.metric == "RATE") {
                        const auto slash = kp.qualifier.find('/');
                        if (slash != std::string::npos) {
                            crm_bridge_->update(tenant_id.to_string(),
                                                kp.qualifier.substr(0, slash),
                                                kp.qualifier.substr(slash + 1),
                                                tick->mid,
                                                tick->datetime);
                        }
                    }
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(lg(), warn)
                        << "CRM update failed for " << ore_key_copy << ": " << e.what();
                }
            }

            if (persisted)
                nats_.js_publish(publish_subject, msg.data);
        });

    subs_.emplace(key, std::move(sub));
}

// Called only from refresh(), which holds mu_.
void feed_ingest_loop::unsubscribe_binding_locked(const subscription_key& key) {
    BOOST_LOG_SEV(lg(), info) << "INGEST UNSUBSCRIBE: source='" << key.source_name
                              << "' tenant='" << key.tenant_id << "' party='" << key.party_id
                              << "' workspace='" << key.workspace_id << "'";
    subs_.erase(key);
    fx_stats_.erase(key); // mu_ already held by caller (refresh)
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
    else if (kind == ores::marketdata::domain::fx_spot_kind_token) {
        // The per-party binding subscriptions ingest fx_spot ticks; the
        // wildcard only sees the unbound ones.
        const std::string source(source_name);
        std::lock_guard lock(mu_);
        if (!bound_sources_.contains(source) && unbound_warned_.insert(source).second)
            BOOST_LOG_SEV(lg(), warn) << "Dropping tick for unbound source '" << source
                                      << "' — no enabled feed_binding";
    } else
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

    // Curve series: one observation per point_id, not a scalar line.
    const bool is_scalar = false;
    const bool persisted = persist_tick_observation(ctx_,
                                                    tick->tenant_id,
                                                    tick->party_id,
                                                    tick->series_type,
                                                    tick->metric,
                                                    tick->qualifier,
                                                    tick->subclass,
                                                    is_scalar,
                                                    tick->datetime,
                                                    std::to_string(tick->value),
                                                    tick->source_name,
                                                    tick->point_id);
    if (persisted)
        nats_.js_publish(publish_subject, msg.data);
}

bool feed_ingest_loop::persist_tick_observation(
    const ores::database::context& ctx,
    ores::utility::uuid::tenant_id tenant_id,
    const boost::uuids::uuid& party_id,
    const std::string& series_type,
    const std::string& metric,
    const std::string& qualifier,
    std::optional<domain::series_subclass> series_subclass,
    bool is_scalar,
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

            // One derivation for every kind: the lowercased series_type is the
            // asset-class enum name (FX → fx, RATES → rates).
            std::string ac_str = series_type;
            std::transform(ac_str.begin(), ac_str.end(), ac_str.begin(), [](unsigned char c) {
                return static_cast<char>(std::tolower(c));
            });

            domain::market_series series;
            series.id = uuid_gen();
            series.tenant_id = tenant_ctx.tenant_id();
            series.party_id = party_id;
            series.series_type = series_type;
            series.metric = metric;
            series.qualifier = qualifier;
            series.asset_class =
                rfl::string_to_enum<domain::asset_class>(ac_str).value_or(domain::asset_class::fx);
            series.series_subclass = series_subclass.value_or(
                rfl::string_to_enum<domain::series_subclass>(ac_str).value_or(
                    domain::series_subclass::spot));
            series.is_scalar = is_scalar;
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
        obs.point_id = point_id;

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

        BOOST_LOG_SEV(lg(), info) << "INGEST STATUS: source='" << key.source_name
                                  << "' tenant='" << key.tenant_id << "' party='" << key.party_id
                                  << "' workspace='" << key.workspace_id << "' ore_key='"
                                  << st->series_identity << "' subject='" << st->nats_subject
                                  << "' publish='" << st->publish_subject << "' ticks=" << count
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
