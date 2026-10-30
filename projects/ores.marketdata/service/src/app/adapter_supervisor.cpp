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
#include "ores.marketdata.service/app/adapter_supervisor.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.marketdata.api/domain/adapter_subjects.hpp"
#include "ores.marketdata.api/domain/provider.hpp"
#include "ores.marketdata.core/repository/adapter_repository.hpp"
#include "ores.marketdata.core/repository/live_tenant_repository.hpp"
#include "ores.marketdata.core/repository/market_data_mapping_repository.hpp"
#include "ores.marketdata.core/repository/feed_repository.hpp"
#include "ores.marketdata.core/repository/market_data_vendor_instrument_repository.hpp"
#include "ores.marketdata.core/repository/provider_repository.hpp"
#include "ores.marketdata.core/repository/route_repository.hpp"
#include "ores.marketdata.core/repository/subscription_repository.hpp"
#include "ores.marketdata.service/app/live_tenants.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <tuple>

namespace ores::marketdata::service::app {

using namespace ores::logging;

namespace {

constexpr auto demand_retry_delay = std::chrono::seconds(2);

}

/// What the supervisor reads from the database: the demand catalogue and the provider codes.
struct adapter_supervisor::snapshot {
    demand_catalogue catalogue;
    std::set<std::string> providers;
    /// The adapter modules the catalogue defines. A module outside it is refused.
    std::set<std::string> adapter_codes;
    /// The vendor ids of the catalogue as provider, scheme, id and field, for the universe check.
    std::set<std::tuple<std::string, std::string, std::string, std::string>> ids;
    /// The routes that name each source, by source name. A route that names no catalogue
    /// source is not counted.
    std::map<std::string, std::uint32_t> routes_by_source;
};

adapter_supervisor::adapter_supervisor(ores::nats::service::client& nats,
                                       ores::database::context ctx,
                                       std::chrono::milliseconds heartbeat_timeout)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , registry_(heartbeat_timeout)
    , snapshot_(std::make_shared<snapshot>()) {}

adapter_supervisor::~adapter_supervisor() {
    stop_.store(true, std::memory_order_relaxed);
    announce_sub_.reset();
    heartbeat_sub_.reset();
    if (monitor_.joinable())
        monitor_.join();
}

void adapter_supervisor::start() {
    refresh();
    const auto announce = domain::adapter_announce_subject();
    const auto heartbeat = domain::adapter_heartbeat_subject_all();
    BOOST_LOG_SEV(lg(), info) << "Starting adapter supervisor: '" << announce << "' and '"
                              << heartbeat << "'";
    announce_sub_ =
        nats_.subscribe(announce, [this](ores::nats::message msg) { announce_message(msg); });
    heartbeat_sub_ =
        nats_.subscribe(heartbeat, [this](ores::nats::message msg) { heartbeat_message(msg); });
    monitor_ = std::thread(&adapter_supervisor::monitor_loop, this);
}

void adapter_supervisor::refresh() {
    const auto system_ctx = ores::database::service::tenant_context::with_system_tenant(ctx_);
    auto next = std::make_shared<snapshot>();
    for (const auto& adapter : repository::adapter_repository{}.read_latest(system_ctx))
        next->adapter_codes.insert(adapter.code);
    next->catalogue.providers = repository::provider_repository{}.read_latest(system_ctx);
    for (const auto& provider : next->catalogue.providers)
        next->providers.insert(provider.code);
    next->catalogue.sources = repository::feed_repository{}.read_latest(system_ctx);
    next->catalogue.instruments =
        repository::market_data_vendor_instrument_repository{}.read_latest(system_ctx);
    next->catalogue.mappings = repository::market_data_mapping_repository{}.read_latest(system_ctx);
    for (const auto& v : next->catalogue.instruments)
        next->ids.emplace(v.provider_code, v.id_scheme, v.external_id, v.field);
    next->catalogue.routes = repository::route_repository{}.read_latest_all_tenants(ctx_);
    next->catalogue.subscriptions =
        repository::subscription_repository{}.read_latest_all_tenants(ctx_);
    const auto live = repository::live_tenant_repository{}.read(ctx_);
    keep_live_tenants(next->catalogue.routes, live);
    keep_live_tenants(next->catalogue.subscriptions, live);

    std::map<boost::uuids::uuid, std::string> source_names;
    for (const auto& source : next->catalogue.sources)
        source_names.emplace(source.id, source.name);
    for (const auto& route : next->catalogue.routes)
        if (const auto found = source_names.find(route.feed_id); found != source_names.end())
            ++next->routes_by_source[found->second];

    {
        std::lock_guard lock(mu_);
        snapshot_ = next;
        for (const auto& id : registry_.demandable())
            needs_demand_[id] = std::chrono::steady_clock::now();
    }
    if (listener_)
        listener_(next->catalogue, std::chrono::system_clock::now());
}

void adapter_supervisor::owe_demand(const std::string& adapter_id,
                                    std::chrono::steady_clock::time_point from) {
    std::lock_guard lock(mu_);
    needs_demand_[adapter_id] = from;
}

std::shared_ptr<const adapter_supervisor::snapshot> adapter_supervisor::current() const {
    std::lock_guard lock(mu_);
    return snapshot_;
}

std::vector<messaging::adapter_status> adapter_supervisor::status() const {
    auto adapters = registry_.status();
    const auto snap = current();
    for (auto& adapter : adapters)
        adapter.routes_seen = routes_for(adapter.adapter_id);
    return adapters;
}

std::uint32_t adapter_supervisor::routes_for(const std::string& adapter_id) const {
    const auto snap = current();
    const auto found = snap->routes_by_source.find(adapter_id);
    return found != snap->routes_by_source.end() ? found->second : 0;
}

messaging::adapter_announce_response
adapter_supervisor::on_announce(const messaging::adapter_announce_request& request,
                                std::chrono::system_clock::time_point now) {
    const auto snap = current();
    universe_check check;
    check.adapter_known = [&](const std::string& adapter) {
        return snap->adapter_codes.contains(adapter);
    };
    check.provider_known = [&](const std::string& provider) {
        return snap->providers.contains(provider);
    };
    check.id_known = [&](const std::string& provider, const messaging::vendor_id& id) {
        return snap->ids.contains({provider, id.id_scheme, id.external_id, id.field});
    };
    const auto response = registry_.announce(request, check, now);
    BOOST_LOG_SEV(lg(), response.success ? info : warn)
        << "Adapter '" << request.adapter_id << "' of provider '" << request.provider_code
        << "' announced: " << (response.success ? "ready" : response.message) << " ("
        << response.served_count << " served, " << response.unknown_count << " unknown)";
    if (response.success)
        owe_demand(request.adapter_id);
    return response;
}

void adapter_supervisor::on_heartbeat(const std::string& adapter_id,
                                      std::uint64_t counter,
                                      std::chrono::system_clock::time_point now) {
    const auto outcome = registry_.heartbeat(adapter_id, counter, now);
    if (outcome.recovered) {
        BOOST_LOG_SEV(lg(), info) << "Adapter '" << adapter_id << "' recovered: sending its demand";
        owe_demand(adapter_id);
    } else if (!outcome.known) {
        bool first = false;
        {
            std::lock_guard lock(mu_);
            first = unknown_reported_.insert(adapter_id).second;
        }
        if (first)
            BOOST_LOG_SEV(lg(), warn)
                << "Heartbeat of adapter '" << adapter_id
                << "' that has not announced itself: ignored until it announces";
    }
}

void adapter_supervisor::announce_message(const ores::nats::message& msg) {
    auto request =
        ores::nats::default_wire_codec().decode<messaging::adapter_announce_request>(msg.data);
    if (!request) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to decode an announce on '" << msg.subject
                                  << "': " << request.error().what();
        return;
    }
    const auto response = on_announce(*request, std::chrono::system_clock::now());
    ores::service::messaging::reply(nats_, msg, response);
}

void adapter_supervisor::heartbeat_message(const ores::nats::message& msg) {
    const auto adapter = domain::adapter_heartbeat_subject_adapter(msg.subject);
    if (!adapter) {
        BOOST_LOG_SEV(lg(), warn) << "Dropping a heartbeat on '" << msg.subject
                                  << "': not a heartbeat subject of an adapter";
        return;
    }
    auto beat = ores::nats::default_wire_codec().decode<messaging::adapter_heartbeat>(msg.data);
    if (!beat) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to decode a heartbeat on '" << msg.subject
                                  << "': " << beat.error().what();
        return;
    }
    on_heartbeat(*adapter, beat->counter, std::chrono::system_clock::now());
}

bool adapter_supervisor::send_demand(const std::string& adapter_id) {
    const auto snap = current();
    const auto result = compute_demand(snap->catalogue, std::chrono::system_clock::now());
    for (const auto& refused : result.refused) {
        if (refused.adapter_id == adapter_id)
            BOOST_LOG_SEV(lg(), warn)
                << "Party " << boost::uuids::to_string(refused.party_id) << " is routed to '"
                << adapter_id << "' with no right to its provider: " << refused.reason;
    }

    messaging::adapter_demand_request request;
    const auto found = result.by_adapter.find(adapter_id);
    if (found != result.by_adapter.end())
        request.ids = found->second;

    try {
        const auto bytes = ores::nats::default_wire_codec().encode(request);
        const auto reply = nats_.request_sync(domain::adapter_demand_subject(adapter_id),
                                              std::span<const std::byte>(bytes),
                                              {},
                                              std::chrono::seconds(2));
        auto response =
            ores::nats::default_wire_codec().decode<messaging::adapter_demand_response>(reply.data);
        if (!response) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to decode the demand reply of '" << adapter_id
                                      << "': " << response.error().what();
            return false;
        }
        registry_.demand_replied(adapter_id, request.ids.size(), *response);
        BOOST_LOG_SEV(lg(), info) << "Adapter '" << adapter_id << "' accepted "
                                  << response->accepted.size() << " and rejected "
                                  << response->rejected.size() << " of " << request.ids.size()
                                  << " id(s)";
        // An adapter that is up, that routes name, and that is sent nothing is the
        // shape of the 2026-10-23 failure. Say it once, not once a minute.
        const auto routes = routes_for(adapter_id);
        bool warned_now = false;
        {
            std::lock_guard lock(mu_);
            if (request.ids.empty() && routes > 0)
                warned_now = no_demand_warned_.insert(adapter_id).second;
            else
                no_demand_warned_.erase(adapter_id);
        }
        if (warned_now)
            BOOST_LOG_SEV(lg(), warn)
                << "Adapter '" << adapter_id << "' is up with " << routes
                << " route(s) naming it and no demand: no routed party's ids reached it";
        return true;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to send the demand to '" << adapter_id
                                  << "': " << e.what();
        return false;
    }
}

void adapter_supervisor::monitor_loop() {
    using namespace std::chrono;
    constexpr auto slice = milliseconds(200);
    auto next_refresh = steady_clock::now() + refresh_interval;
    while (!stop_.load(std::memory_order_relaxed)) {
        std::this_thread::sleep_for(slice);

        for (const auto& id : registry_.expire(system_clock::now()))
            BOOST_LOG_SEV(lg(), warn) << "Adapter '" << id << "' missed its heartbeat: stale";

        // Re-read the catalogue on a timer as well as on events, so a missed event
        // delays a demand set by one interval instead of losing it.
        if (steady_clock::now() >= next_refresh) {
            next_refresh = steady_clock::now() + refresh_interval;
            try {
                refresh();
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn) << "Periodic catalogue refresh failed: " << e.what();
            }
        }

        std::vector<std::string> due;
        {
            const auto now = std::chrono::steady_clock::now();
            std::lock_guard lock(mu_);
            for (auto it = needs_demand_.begin(); it != needs_demand_.end();) {
                if (it->second <= now) {
                    due.push_back(it->first);
                    it = needs_demand_.erase(it);
                } else
                    ++it;
            }
        }
        for (const auto& id : due) {
            if (stop_.load(std::memory_order_relaxed))
                break;
            const auto state = registry_.state_of(id);
            if (!state || *state == adapter_state::stale || *state == adapter_state::rejected)
                continue;
            // A demand that did not arrive is sent again after a wait, because the adapter holds
            // none until it does.
            if (!send_demand(id))
                owe_demand(id, std::chrono::steady_clock::now() + demand_retry_delay);
        }
    }
}

}
