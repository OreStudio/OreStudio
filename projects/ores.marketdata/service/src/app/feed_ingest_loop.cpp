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
#include "ores.marketdata.core/repository/market_series_mapper.hpp"
#include "ores.nats/domain/message.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <rfl/json.hpp>
#include <algorithm>
#include <set>
#include <stdexcept>

namespace ores::marketdata::service::app {

using namespace ores::logging;

namespace {

// "FX/RATE/EUR/USD" → "marketdata.v1.tick.fx.rate.eur.usd"
std::string ore_key_to_publish_subject(std::string ore_key) {
    std::transform(ore_key.begin(), ore_key.end(), ore_key.begin(),
                   [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    std::replace(ore_key.begin(), ore_key.end(), '/', '.');
    return "marketdata.v1.tick." + ore_key;
}

// "FX/RATE/EUR/USD" → {series_type="FX", metric="RATE", qualifier="EUR/USD"}
struct ore_key_parts { std::string series_type, metric, qualifier; };
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

feed_ingest_loop::feed_ingest_loop(ores::nats::service::client& nats,
                                   ores::database::context ctx)
    : nats_(nats)
    , ctx_(std::move(ctx)) {}

void feed_ingest_loop::start() {
    BOOST_LOG_SEV(lg(), info) << "Starting feed ingest loop";
    refresh();
}

void feed_ingest_loop::refresh() {
    repository::feed_binding_repository repo;
    const auto bindings = repo.read_latest(ctx_);

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
        unsubscribe_binding(s);

    // Subscribe anything new
    for (const auto& b : bindings) {
        if (b.enabled && !subs_.contains(b.source_name))
            subscribe_binding(b.ore_key, b.source_name);
    }

    BOOST_LOG_SEV(lg(), info) << "Feed ingest loop: " << subs_.size() << " active subscription(s)";
}

void feed_ingest_loop::subscribe_binding(const std::string& ore_key,
                                         const std::string& source_name) {
    const std::string producer_subject = "synthetic.v1.tick." + source_name;
    const std::string publish_subject = ore_key_to_publish_subject(ore_key);
    const std::string ore_key_copy = ore_key;

    BOOST_LOG_SEV(lg(), info) << "Subscribing ingest: " << producer_subject
                               << " → " << publish_subject;

    boost::uuids::random_generator uuid_gen;

    auto sub = nats_.subscribe(
        producer_subject,
        [this, ore_key_copy, publish_subject, uuid_gen](ores::nats::message msg) mutable {
            auto tick = rfl::json::read<domain::fx_spot_tick>(
                ores::nats::as_string_view(msg.data));
            if (!tick) {
                BOOST_LOG_SEV(lg(), warn) << "Failed to decode fx_spot_tick: "
                                          << tick.error().what();
                return;
            }

            // Persist the observation
            try {
                const auto kp = parse_ore_key(ore_key_copy);
                repository::market_series_repository series_repo;
                auto existing = series_repo.read_latest_by_type(
                    ctx_, kp.series_type, kp.metric, kp.qualifier);
                if (existing.empty()) {
                    BOOST_LOG_SEV(lg(), info) << "Auto-creating market series for "
                                              << ore_key_copy;
                    std::string ac_str = kp.series_type;
                    std::transform(ac_str.begin(), ac_str.end(), ac_str.begin(),
                                   [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
                    domain::market_series series;
                    series.id = uuid_gen();
                    series.tenant_id = ctx_.tenant_id();
                    series.series_type = kp.series_type;
                    series.metric = kp.metric;
                    series.qualifier = kp.qualifier;
                    series.asset_class = repository::market_series_mapper::asset_class_from_string(ac_str);
                    series.is_scalar = true;
                    series_repo.write(ctx_, series);
                    existing.push_back(std::move(series));
                }

                domain::market_observation obs;
                obs.id = uuid_gen();
                obs.tenant_id = ctx_.tenant_id();
                obs.series_id = existing.front().id;
                obs.observation_datetime = tick->datetime;
                obs.value = tick->mid;

                repository::market_observations_repository obs_repo;
                obs_repo.write(ctx_, obs);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error) << "Failed to persist observation for "
                                           << ore_key_copy << ": " << e.what();
            }

            // Republish on the official tenant-scoped stream
            nats_.publish(publish_subject, msg.data);
        });

    subs_.emplace(source_name, std::move(sub));
}

void feed_ingest_loop::unsubscribe_binding(const std::string& source_name) {
    BOOST_LOG_SEV(lg(), info) << "Unsubscribing ingest for source: " << source_name;
    subs_.erase(source_name);
}

} // namespace ores::marketdata::service::app
