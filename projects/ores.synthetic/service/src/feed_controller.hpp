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
#ifndef ORES_SYNTHETIC_SERVICE_FEED_CONTROLLER_HPP
#define ORES_SYNTHETIC_SERVICE_FEED_CONTROLLER_HPP

#include "fx_spot_feed.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/asset_class.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/series_subclass.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "process_factory.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <atomic>
#include <cctype>
#include <chrono>
#include <map>
#include <memory>
#include <mutex>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Runs the synthetic producer feeds; one tick thread per feed.
 *
 * Owned by application::run() as a shared_ptr and passed to the
 * market_feed_config_handler lambdas in the registrar, and driven on startup
 * by the autonomous config-driven auto-starter. Holds a map of running feeds
 * keyed by source name (a producer's unique identity), so several producers —
 * including two for the same pair (e.g. Wiener vs GBM-drift EUR/USD) — run
 * concurrently and publish on distinct subjects.
 *
 * Each feed resolves/creates its own market series from its ORE key and
 * publishes on its synthetic producer channel: "synthetic.v1.tick.<source>".
 *
 * Threading: start() and stop() are called from NATS I/O callbacks and the
 * startup path; both are protected by a mutex. shutdown() is called from the
 * application coroutine after the NATS I/O loop has stopped.
 */
class feed_controller {
private:
    static auto& lg() {
        static auto instance = ores::logging::make_logger("ores.synthetic.service.feed_controller");
        return instance;
    }

public:
    feed_controller(ores::nats::service::client& nats,
                    ores::nats::service::nats_client& auth_nats,
                    ores::utility::uuid::tenant_id tenant_id)
        : nats_(nats)
        , auth_nats_(auth_nats)
        , md_client_(auth_nats)
        , tenant_id_(std::move(tenant_id)) {}

    ~feed_controller() {
        stop_flag_.store(true, std::memory_order_relaxed);
        if (status_thread_.joinable())
            status_thread_.join();
        shutdown();
    }

    enum class start_result { started, already_running, series_unresolved };

    /**
     * @brief Start one producer feed. Keyed by source_name (unique per producer).
     *
     * Resolves/creates the market series for ore_key, derives the producer
     * subject from source_name, builds the process and spawns its tick thread.
     */
    start_result start(const std::string& ore_key,
               const std::string& source_name,
               std::vector<double> means,
               std::vector<double> stdevs,
               std::vector<double> weights,
               double initial_price,
               double ticks_per_hour,
               const std::string& process_type = "geometric") {
        // Resolve the series BEFORE locking — it makes blocking NATS RPCs, and we
        // must not hold mu_ (which stop()/shutdown() also take) across network I/O.
        // resolve_series is find-or-create, so a concurrent duplicate start costs
        // at worst one extra lookup, not a corrupt state.
        boost::uuids::uuid series_id{};
        if (!resolve_series(ore_key, series_id))
            return start_result::series_unresolved;

        std::lock_guard lock(mu_);
        const std::string key = source_name.empty() ? ore_key : source_name;
        if (feeds_.contains(key))
            return start_result::already_running;

        auto process = process_factory::make_process(
            process_type, std::move(means), std::move(stdevs), std::move(weights), initial_price);
        auto feed = std::make_shared<fx_spot_feed>(nats_,
                                                   auth_nats_,
                                                   ore_key,
                                                   producer_subject(key),
                                                   std::move(process),
                                                   ticks_per_hour,
                                                   series_id,
                                                   tenant_id_);
        running_feed rf;
        rf.feed = feed;
        rf.thread = std::thread([feed]() { feed->start([](const auto& /*tick*/) {}); });
        feeds_.emplace(key, std::move(rf));
        BOOST_LOG_SEV(lg(), ores::logging::info)
            << "SYNTHETIC START: source='" << key
            << "' ore_key='" << ore_key
            << "' subject='" << producer_subject(key)
            << "' ticks_per_hour=" << ticks_per_hour
            << " — now " << feeds_.size() << " feed(s) running";
        if (!status_thread_.joinable()) {
            status_thread_ = std::thread(&feed_controller::status_loop, this);
        }
        return start_result::started;
    }

    /**
     * @brief Stop one feed by key (source_name), or all feeds if key is empty.
     *
     * Signals the tick thread(s) to stop, joins, and removes them. Returns the
     * number of feeds stopped.
     */
    std::size_t stop(const std::string& key = {}) {
        std::lock_guard lock(mu_);
        if (key.empty()) {
            const auto n = feeds_.size();
            for (auto& [_, rf] : feeds_)
                join_and_clear(rf);
            feeds_.clear();
            return n;
        }
        auto it = feeds_.find(key);
        if (it == feeds_.end())
            return 0;
        join_and_clear(it->second);
        feeds_.erase(it);
        return 1;
    }

    /**
     * @brief Stop and join every feed. Safe to call even with none running.
     *
     * Intended for orderly application shutdown; must only be called after the
     * NATS I/O loop has stopped (no concurrent handler callbacks).
     */
    void shutdown() {
        stop();
    }

    /** @brief Number of feeds currently running. */
    std::size_t running_count() const {
        std::lock_guard lock(mu_);
        return feeds_.size();
    }

private:
    static constexpr std::chrono::minutes status_interval_{1};

    void status_loop() {
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

    void log_status() const {
        std::lock_guard lock(mu_);
        if (feeds_.empty()) {
            BOOST_LOG_SEV(lg(), ores::logging::info) << "SYNTHETIC STATUS: no feeds running";
            return;
        }
        for (const auto& [key, rf] : feeds_) {
            const auto count = rf.feed ? rf.feed->publish_count() : 0;
            BOOST_LOG_SEV(lg(), ores::logging::info)
                << "SYNTHETIC STATUS: source='" << key
                << "' ore_key='" << rf.feed->ore_key()
                << "' subject='" << producer_subject(key)
                << "' published=" << count;
        }
    }

    struct running_feed {
        std::shared_ptr<fx_spot_feed> feed;
        std::thread thread;
    };

    // Build the producer subject from source_name. '.' is kept (it is the NATS
    // hierarchy separator and source names are dotted), but any character that
    // is not a safe subject token — whitespace, wildcards ('*', '>'), or
    // non-alphanumerics other than '.', '_', '-' — is replaced with '_' so a
    // stray value cannot produce surprise routing or a publish error.
    static std::string producer_subject(const std::string& source_name) {
        std::string token;
        token.reserve(source_name.size());
        for (unsigned char c : source_name) {
            const bool safe = std::isalnum(c) || c == '.' || c == '_' || c == '-';
            token += safe ? static_cast<char>(c) : '_';
        }
        return "synthetic.v1.tick." + token;
    }

    static void join_and_clear(running_feed& rf) {
        if (rf.feed)
            rf.feed->stop();
        if (rf.thread.joinable())
            rf.thread.join();
    }

    // Find-or-create the FX spot market series for an ORE key like
    // "FX/RATE/EUR/USD" → series_type "FX", metric "RATE", qualifier "EUR/USD".
    bool resolve_series(const std::string& ore_key, boost::uuids::uuid& out) {
        using namespace ores::marketdata::domain;

        std::vector<std::string> parts;
        std::stringstream ss(ore_key);
        std::string tok;
        while (std::getline(ss, tok, '/'))
            parts.push_back(tok);
        if (parts.size() < 3) {
            BOOST_LOG_SEV(lg(), ores::logging::warn) << "Cannot parse ORE key '" << ore_key << "'.";
            return false;
        }
        const std::string series_type = parts[0];
        const std::string metric = parts[1];
        std::string qualifier = parts[2];
        for (std::size_t i = 3; i < parts.size(); ++i)
            qualifier += "/" + parts[i];

        auto existing = md_client_.list_series(series_type);
        if (!existing) {
            BOOST_LOG_SEV(lg(), ores::logging::warn)
                << "Failed to list series for '" << series_type << "': " << existing.error();
            return false;
        }
        for (const auto& s : *existing) {
            if (s.metric == metric && s.qualifier == qualifier) {
                out = s.id;
                return true;
            }
        }

        market_series s;
        s.id = uuid_gen_();
        s.tenant_id = tenant_id_;
        s.series_type = series_type;
        s.metric = metric;
        s.qualifier = qualifier;
        s.asset_class = asset_class::fx;
        s.series_subclass = series_subclass::spot;
        s.is_scalar = true;
        s.modified_by = "ores.synthetic.service";
        s.performed_by = "ores.synthetic.service";
        s.change_reason_code = "system.initial_load";
        s.change_commentary = ore_key + " synthetic feed initialisation";

        const auto saved = md_client_.save_series({s});
        if (!saved) {
            BOOST_LOG_SEV(lg(), ores::logging::warn)
                << "Failed to create series for '" << ore_key << "': " << saved.error();
            return false;
        }
        BOOST_LOG_SEV(lg(), ores::logging::info) << "Created series for " << ore_key << ".";
        out = s.id;
        return true;
    }

    ores::nats::service::client& nats_;
    ores::nats::service::nats_client& auth_nats_;
    ores::marketdata::client::market_data_client md_client_;
    ores::utility::uuid::tenant_id tenant_id_;
    boost::uuids::random_generator uuid_gen_;

    mutable std::mutex mu_;
    std::map<std::string, running_feed> feeds_;

    std::atomic<bool> stop_flag_{false};
    std::thread status_thread_;
};

}

#endif
