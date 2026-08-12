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
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <rfl/enums.hpp>
#include <stdexcept>
#include <string_view>

namespace ores::marketdata::service::app {

using namespace ores::logging;

namespace {

// One wildcard over the unified tick family: synthetic.v1.tick.<kind>.<source_name>.
// '>' (multi-token wildcard), not '*' (single-token): source_name is dotted
// (e.g. "ir_curve.usd.sofr"), so the subject has more than one token after the
// prefix. Sandboxed feeds publish under synthetic.v1.sandbox.tick., which this
// prefix never matches.
constexpr auto* unified_wildcard_subject = "synthetic.v1.tick.>";

// "FX/RATE/EUR/USD" → "marketdata.v1.tick.fx.rate.eur.usd"
std::string ore_key_to_publish_subject(const std::string& tenant_id_str, std::string ore_key) {
    std::transform(ore_key.begin(), ore_key.end(), ore_key.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    std::replace(ore_key.begin(), ore_key.end(), '/', '.');
    return "marketdata.v1.tick." + tenant_id_str + "." + ore_key;
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
    refresh();
    sub_ = nats_.subscribe(unified_wildcard_subject,
                           [this](ores::nats::message msg) mutable { on_tick(msg); });
    status_thread_ = std::thread(&feed_ingest_loop::status_loop, this);
}

void feed_ingest_loop::refresh() {
    // DB read outside the lock — no shared state touched.
    repository::feed_binding_repository repo;
    const auto bindings = repo.read_latest_all_tenants(ctx_);

    std::lock_guard lock(mu_);

    bindings_.clear();
    for (const auto& b : bindings) {
        if (!b.enabled)
            continue;
        binding_record rec;
        rec.ore_key = b.ore_key;
        rec.tenant_id_str = b.tenant_id.to_string();
        rec.party_id = b.party_id;
        rec.publish_subject = ore_key_to_publish_subject(rec.tenant_id_str, rec.ore_key);
        bindings_.emplace(b.source_name, std::move(rec));
        unbound_warned_.erase(b.source_name);
    }

    BOOST_LOG_SEV(lg(), info)
        << "Feed ingest loop: " << bindings_.size() << " enabled binding(s) in cache";
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

    if (kind == ores::marketdata::domain::fx_spot_kind_token)
        ingest_fx_spot(msg, source_name);
    else if (kind == ores::marketdata::domain::ir_curve_kind_token)
        ingest_ir_curve(msg);
    else
        BOOST_LOG_SEV(lg(), warn) << "Unknown tick kind '" << kind << "' on subject '"
                                  << msg.subject << "'";
}

void feed_ingest_loop::ingest_fx_spot(const ores::nats::message& msg,
                                      std::string_view source_name) {
    // Identity comes from the binding cache — fx_spot_tick is not self-describing.
    binding_record binding;
    {
        std::lock_guard lock(mu_);
        const std::string source(source_name);
        const auto it = bindings_.find(source);
        if (it == bindings_.end()) {
            if (unbound_warned_.insert(source).second)
                BOOST_LOG_SEV(lg(), warn) << "Dropping tick for unbound source '" << source
                                          << "' — no enabled feed_binding";
            return;
        }
        binding = it->second;
    }

    auto tick = ores::nats::default_wire_codec().decode<domain::fx_spot_tick>(msg.data);
    if (!tick) {
        BOOST_LOG_SEV(lg(), warn)
            << "Failed to decode fx_spot_tick: " << tick.error().what();
        return;
    }

    const auto now_rep = std::chrono::system_clock::now().time_since_epoch().count();
    std::shared_ptr<feed_stats> st;
    {
        std::lock_guard lock(mu_);
        st = stats_.try_emplace(std::string(source_name), std::make_shared<feed_stats>())
                 .first->second;
        if (st->series_identity.empty()) {
            st->series_identity = binding.ore_key;
            st->nats_subject = msg.subject;
        }
    }
    const auto prev_count = st->tick_count.fetch_add(1, std::memory_order_relaxed);
    st->last_tick_rep.store(now_rep, std::memory_order_relaxed);

    if (prev_count == 0) {
        BOOST_LOG_SEV(lg(), info)
            << "INGEST FIRST TICK: source='" << binding.ore_key << "' subject='"
            << binding.publish_subject << "' mid=" << tick->mid;
    }

    // Persist the observation
    try {
        const auto kp = parse_ore_key(binding.ore_key);
        auto tenant_ctx =
            ctx_.with_tenant(ores::utility::uuid::tenant_id::from_string(binding.tenant_id_str)
                                 .value(),
                             "ores.marketdata.service");
        repository::market_series_repository series_repo;
        auto existing = series_repo.read_latest_by_type(tenant_ctx,
                                                        kp.series_type,
                                                        kp.metric,
                                                        kp.qualifier,
                                                        boost::uuids::to_string(binding.party_id));
        if (existing.empty()) {
            BOOST_LOG_SEV(lg(), info) << "Auto-creating market series for " << binding.ore_key;
            std::string ac_str = kp.series_type;
            std::transform(
                ac_str.begin(), ac_str.end(), ac_str.begin(), [](unsigned char c) {
                    return static_cast<char>(std::tolower(c));
                });
            domain::market_series series;
            series.id = boost::uuids::random_generator{}();
            series.tenant_id = tenant_ctx.tenant_id();
            series.party_id = binding.party_id;
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
            series.change_commentary = binding.ore_key + " synthetic feed auto-created";
            series_repo.write(tenant_ctx, series);
            existing.push_back(std::move(series));
        }

        domain::market_observation obs;
        obs.id = boost::uuids::random_generator{}();
        obs.tenant_id = tenant_ctx.tenant_id();
        obs.party_id = binding.party_id;
        obs.series_id = existing.front().id;
        obs.observation_datetime = tick->datetime;
        obs.value = std::to_string(tick->mid);
        obs.source = std::string(source_name);
        obs.point_id = "SPOT"; // scalar FX spot series has no tenor/surface coordinate

        repository::market_observations_repository obs_repo;
        obs_repo.write(tenant_ctx, obs);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to persist observation for " << binding.ore_key << ": " << e.what();
    }

    // Offer the tick to the CRM as a candidate driver update,
    // tenant-wide -- every party in the tenant with a matching
    // driver edge gets it, not just the party that owns this
    // feed_binding; see crm_ingest_bridge's own class doc for why.
    // A no-op if no party in this tenant has a CRM configured, or
    // the pair isn't a driver edge of any of them. Currency driver
    // pairs are FX-shaped, so the bridge is an fx_spot concern only.
    if (crm_bridge_) {
        try {
            const auto kp = parse_ore_key(binding.ore_key);
            if (kp.series_type == "FX" && kp.metric == "RATE") {
                const auto slash = kp.qualifier.find('/');
                if (slash != std::string::npos) {
                    crm_bridge_->update(binding.tenant_id_str,
                                        kp.qualifier.substr(0, slash),
                                        kp.qualifier.substr(slash + 1),
                                        tick->mid,
                                        tick->datetime);
                }
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn)
                << "CRM update failed for " << binding.ore_key << ": " << e.what();
        }
    }

    nats_.js_publish(binding.publish_subject, msg.data);
}

void feed_ingest_loop::ingest_ir_curve(const ores::nats::message& msg) {
    auto tick = ores::nats::default_wire_codec().decode<domain::ir_curve_tick>(msg.data);
    if (!tick) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to decode ir_curve_tick: " << tick.error().what();
        return;
    }

    const auto now_rep = std::chrono::system_clock::now().time_since_epoch().count();
    std::shared_ptr<feed_stats> st;
    {
        std::lock_guard lock(mu_);
        st = stats_.try_emplace(tick->source_name, std::make_shared<feed_stats>())
                 .first->second;
        if (st->series_identity.empty()) {
            st->series_identity =
                tick->series_type + "/" + tick->metric + "/" + tick->qualifier;
            st->nats_subject = msg.subject;
        }
    }
    const auto prev_count = st->tick_count.fetch_add(1, std::memory_order_relaxed);
    st->last_tick_rep.store(now_rep, std::memory_order_relaxed);

    if (prev_count == 0) {
        BOOST_LOG_SEV(lg(), info)
            << "INGEST FIRST TICK: series='" << st->series_identity
            << "' subject='" << msg.subject << "' value=" << tick->value;
    }

    try {
        auto tenant_ctx = ctx_.with_tenant(tick->tenant_id, "ores.marketdata.service");

        repository::market_series_repository series_repo;
        auto existing = series_repo.read_latest_by_type(tenant_ctx,
                                                        tick->series_type,
                                                        tick->metric,
                                                        tick->qualifier,
                                                        boost::uuids::to_string(tick->party_id));
        if (existing.empty()) {
            BOOST_LOG_SEV(lg(), info) << "Auto-creating market series for " << tick->series_type
                                      << "/" << tick->metric << "/" << tick->qualifier;
            domain::market_series series;
            series.id = boost::uuids::random_generator{}();
            series.tenant_id = tenant_ctx.tenant_id();
            series.party_id = tick->party_id;
            series.series_type = tick->series_type;
            series.metric = tick->metric;
            series.qualifier = tick->qualifier;
            series.asset_class = domain::asset_class::rates;
            series.series_subclass = tick->subclass;
            series.is_scalar = false;
            series.modified_by = ctx_.service_account();
            series.performed_by = ctx_.service_account();
            series.change_reason_code = "system.initial_load";
            series.change_commentary = tick->series_type + "/" + tick->metric + "/" +
                                       tick->qualifier + " synthetic curve feed auto-created";
            series_repo.write(tenant_ctx, series);
            existing.push_back(std::move(series));
        }

        domain::market_observation obs;
        obs.id = boost::uuids::random_generator{}();
        obs.tenant_id = tenant_ctx.tenant_id();
        obs.party_id = tick->party_id;
        obs.series_id = existing.front().id;
        obs.observation_datetime = tick->datetime;
        obs.value = std::to_string(tick->value);
        obs.source = tick->source_name;
        obs.point_id = tick->point_id;

        repository::market_observations_repository obs_repo;
        obs_repo.write(tenant_ctx, obs);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to persist IR curve observation for " << tick->series_type << "/"
            << tick->metric << "/" << tick->qualifier << " point=" << tick->point_id << ": "
            << e.what();
    }

    // Uniform republish: the ORE-key form of the series identity, same conversion
    // as fx_spot (lowercased, '/' → '.'), so curve consumers can subscribe per
    // series on marketdata.v1.tick.<tenant>.<series> like FX consumers do.
    const std::string ore_key =
        tick->series_type + "/" + tick->metric + "/" + tick->qualifier;
    nats_.js_publish(ore_key_to_publish_subject(tick->tenant_id.to_string(), ore_key), msg.data);
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

        BOOST_LOG_SEV(lg(), info) << "INGEST STATUS: source='" << source << "' series='"
                                  << st->series_identity << "' subject='" << st->nats_subject
                                  << "' ticks=" << count
                                  << (ever ? std::format(" last_tick={}s ago", age_s) :
                                             " last_tick=never");
    }
}

} // namespace ores::marketdata::service::app
