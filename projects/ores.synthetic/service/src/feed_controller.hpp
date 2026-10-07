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

#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/i_feed.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.marketdata.core/datum/ore_key_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.synthetic.api/domain/binding_mode.hpp"
#include "ores.synthetic.api/feeds/vintage_lookup.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <atomic>
#include <chrono>
#include <expected>
#include <format>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <random>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief The marketdata feed_binding store a feed_controller binds started
 * feeds into.
 *
 * A seam, not an abstraction: the controller owns the one rule "a bound feed
 * is bound, a sandboxed feed is not" and the store owns the marketdata
 * round-trip that carries it out, so tests can drive the rule without a live
 * marketdata service.
 */
class feed_binding_store {
public:
    virtual ~feed_binding_store() = default;

    /**
     * @brief Bind @p source_name unless a binding for it already exists.
     *
     * Idempotent: a source that is already bound is left alone, so a feed
     * started, stopped and restarted keeps the one row instead of gaining a
     * second.
     */
    virtual std::expected<void, std::string>
    save_if_absent(const std::string& source_name, const std::string& caller_bearer_token) = 0;
};

/**
 * @brief Whether two running/candidate feeds would collide: same qualifier
 * AND same role.
 *
 * role is deliberately part of the comparison — a discount feed and a
 * projection feed for the same qualifier are expected to coexist, not
 * conflict. FX feeds have an empty role, which makes the comparison
 * qualifier-only: two FX feeds on the same pair cannot both run, because
 * both would publish into the same observation series — the same hazard
 * the IR rule guards against. A feed with an empty qualifier (an
 * unparseable ORE key) has no published market-data key to protect and
 * conflicts with nothing. Pure and free of IFeed/NATS so it is directly
 * unit-testable without a live NATS client.
 */
inline bool feeds_conflict(const std::string& qualifier_a,
                           const std::string& role_a,
                           const std::string& qualifier_b,
                           const std::string& role_b) {
    return !qualifier_a.empty() && qualifier_a == qualifier_b && role_a == role_b;
}

/**
 * @brief The production feed_binding store: marketdata over NATS.
 */
std::shared_ptr<feed_binding_store>
make_marketdata_binding_store(ores::nats::service::nats_client& auth_nats);

/**
 * @brief Owns the running synthetic producer feeds; one tick thread per
 * feed.
 *
 * Serves every asset class from one class: the map of running feeds is
 * keyed by source_name (a producer's unique identity), so several
 * producers run concurrently and publish on distinct subjects — but at
 * most one per (qualifier, role) pair (see feeds_conflict), so two feeds
 * on the same pair — two FX feeds included — never both run. Every feed
 * enters the map through the common IFeed interface: the per-kind
 * producers (fx_spot_feed, ir_curve_feed) differ only in what they
 * publish, not in how they are owned.
 *
 * Two paths register a feed, both taking the feed's binding_mode and the
 * caller's bearer token:
 *   - add() — the config-driven path (auto-start at boot, the folder
 *     cascade): the caller has already built the feed from persisted
 *     config via the factory, including any vintage resolution the
 *     builder performed. Returns false (and never starts) when the
 *     source_name is already running or the conflict key is held.
 *   - start(IFeed) — the on-demand path (the per-config control-plane),
 *     reporting already_running when that source is already up.
 *
 * Both create the feed's marketdata feed_binding once the feed is running,
 * from one gate: a bound feed is bound, a sandboxed feed never is. Nothing
 * branches on the asset class — a feed arrives as IFeed and its kind is
 * irrelevant here.
 *
 * At most one feed per (qualifier, role) pair — the pair the feed's own
 * IFeed::conflict_key() encodes (see feeds_conflict) — runs at a time:
 * an add()/start() whose pair is already held by a *different* running
 * feed is rejected, never silently, and never by stopping the existing
 * one. Switching requires an explicit stop() first. The check compares
 * the pair via feeds_conflict() rather than the key string, because an
 * empty qualifier (an unparseable ORE key) must conflict with nothing
 * even though its key string is non-empty. The conflict is reported with
 * the running feed's source_name, via the conflicting-source-name out
 * parameter or running_source_name_for_conflict_key().
 *
 * Threading: start() and stop() are called from NATS I/O callbacks and
 * the startup path; both are protected by a mutex. shutdown() is called
 * from the application coroutine after the NATS I/O loop has stopped.
 */
class feed_controller {
private:
    static auto& lg() {
        static auto instance = ores::logging::make_logger("ores.synthetic.service.feed_controller");
        return instance;
    }

public:
    // nats is the raw transport the feeds build with; the controller itself
    // needs only the authenticated client, for the vintage and binding calls.
    feed_controller(ores::nats::service::client& nats,
                    ores::nats::service::nats_client& auth_nats,
                    std::shared_ptr<feed_binding_store> bindings = {})
        : auth_nats_(auth_nats)
        , bindings_(bindings ? std::move(bindings) : make_marketdata_binding_store(auth_nats)) {
        (void)nats;
    }

    ~feed_controller() {
        stop_flag_.store(true, std::memory_order_relaxed);
        if (status_thread_.joinable())
            status_thread_.join();
        shutdown();
    }

    enum class start_result { started, already_running, qualifier_conflict, vintage_data_missing };

    /**
     * @brief Config-driven path: registers an already-constructed feed as
     * running and spawns its tick thread (auto-start at boot, the folder
     * cascade), then binds it. The caller built the feed from persisted
     * config via the factory, including any vintage resolution the builder
     * performed, so this path does no vintage check and no process
     * construction.
     *
     * @p binding_mode is the mode the caller built the feed with, and it
     * drives the bind below: =bound= creates or confirms the feed_binding on
     * "synthetic.v1.ops.tick.<source>", the subject the marketdata ingest loop
     * subscribes; =sandboxed= publishes on the sandbox prefix instead and
     * never gains a binding, because a binding naming this source would
     * claim ingestion happens on the bound subject when it does not.
     *
     * @p caller_bearer_token is the requesting end-user's token, forwarded to
     * marketdata as X-Delegated-Authorization so the binding is created in
     * that caller's tenant/party context. An internal call with no end-user
     * session (boot auto-start) passes nothing and binds nothing: it has no
     * party to bind under.
     *
     * Returns false without starting when a feed with the same source_name
     * is already running, or when the feed's conflict key is held by a
     * different running feed (with @p out_conflicting_source_name set to
     * the holder's source_name).
     */
    bool add(std::shared_ptr<ores::marketdata::domain::IFeed> feed,
             ores::synthetic::domain::binding_mode binding_mode,
             const std::string& caller_bearer_token = {},
             std::string* out_conflicting_source_name = nullptr) {
        const auto source_name = feed->source_name();
        {
            std::lock_guard lock(mu_);
            // The caller's own source_name is excluded from conflict detection,
            // so a duplicate add for it would pass the qualifier check and then
            // emplace-collide: the discarded node would destroy a joinable
            // thread (std::terminate). Concurrent request paths (folder cascade)
            // can race past a caller's pre-checks, so guard the same way start()
            // does.
            if (feeds_.contains(source_name))
                return false;
            if (const auto conflict = find_conflict(*feed, source_name)) {
                if (out_conflicting_source_name)
                    *out_conflicting_source_name = *conflict;
                return false;
            }
            start_running(std::move(feed), binding_mode);
        }
        // Binding creation is a blocking marketdata round-trip, so it runs
        // after mu_ is released rather than inside the critical section every
        // other start()/stop()/running_count()/list() call shares.
        bind_feed(source_name, binding_mode, caller_bearer_token);
        return true;
    }

    /**
     * @brief On-demand path: starts a feed by source_name if not already
     * running, and if no *different* feed already holds its conflict key.
     * Use running_source_name_for_conflict_key() to build an actionable
     * message when this returns qualifier_conflict.
     *
     * @p binding_mode and @p caller_bearer_token mean what they mean in add():
     * the mode the caller built the feed with decides whether a feed_binding
     * is created, and the token is the session that binding belongs to. A
     * re-start of an already-running feed still confirms its binding, so a
     * feed whose row was deleted by hand regains it on the next start.
     */
    start_result start(std::shared_ptr<ores::marketdata::domain::IFeed> feed,
                       ores::synthetic::domain::binding_mode binding_mode,
                       const std::string& caller_bearer_token = {}) {
        const auto source_name = feed->source_name();
        bool already_running = false;
        {
            std::lock_guard lock(mu_);
            already_running = feeds_.contains(source_name);
            if (already_running) {
                // An already-running feed keeps the mode it is running with,
                // not this call's argument: a caller that does not know the
                // feed is sandboxed must not bind it.
                binding_mode = feeds_.at(source_name).binding_mode;
            } else {
                if (find_conflict(*feed, source_name))
                    return start_result::qualifier_conflict;
                start_running(std::move(feed), binding_mode);
            }
        }
        // Runs outside mu_ (a blocking marketdata round-trip): binding inside
        // the critical section would stall every other start()/stop()/
        // running_count()/list() call for the round-trip's duration. It runs
        // on both outcomes, so a feed whose binding row was deleted by hand
        // regains it on the next start.
        bind_feed(source_name, binding_mode, caller_bearer_token);
        return already_running ? start_result::already_running : start_result::started;
    }

    /**
     * @brief The source_name of the running feed currently holding @p
     * conflict_key, if any — for building the "already running as X — stop
     * it first" message after a qualifier_conflict result.
     *
     * Feeds with an empty qualifier never hold a conflict key (see
     * feeds_conflict) and are skipped, so the lookup cannot false-positive
     * on one of them.
     */
    std::optional<std::string>
    running_source_name_for_conflict_key(const std::string& conflict_key) const {
        std::lock_guard lock(mu_);
        for (const auto& [name, rf] : feeds_) {
            if (rf.feed->qualifier().empty())
                continue;
            if (rf.feed->conflict_key() == conflict_key)
                return name;
        }
        return std::nullopt;
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

    /**
     * @brief Snapshot of source_names for all currently running feeds,
     * optionally scoped to one feed kind (IFeed::kind()) — the per-kind
     * control-plane handlers list only their own kind, so a collapsed
     * controller still answers each handler's list as before.
     */
    std::vector<std::string> list(const std::string& kind = {}) const {
        std::lock_guard lock(mu_);
        std::vector<std::string> names;
        names.reserve(feeds_.size());
        for (const auto& [key, rf] : feeds_) {
            if (!kind.empty() && rf.feed->kind() != kind)
                continue;
            names.push_back(key);
        }
        return names;
    }

    /**
     * @brief Check whether a feed's required vintage data exists, without
     * starting it. Powers the Market Simulator "validate all" action.
     *
     * FX-only surface (the IR producers resolve vintage at build time, in
     * the factory builder): the vintage_validity_handler iterates
     * fx_spot_generation_config rows exclusively.
     *
     * @param error_detail Set to an actionable message when unavailable;
     * untouched otherwise.
     * @param resolved_price Set to the found observation's value on success;
     * untouched otherwise.
     */
    bool validate(const std::string& ore_key,
                  const std::string& vintage_source,
                  const std::string& vintage_date,
                  std::string& error_detail,
                  const std::string& caller_bearer_token = {},
                  double* resolved_price = nullptr) {
        return vintage_data_available(ore_key,
                                      vintage_source,
                                      vintage_date,
                                      error_detail,
                                      caller_bearer_token,
                                      resolved_price);
    }

private:
    // The one binding rule, applied by every start path: a bound feed is
    // bound, a sandboxed feed never is. Skipped when there is no caller
    // bearer token — an internal start with no end-user session has no party
    // to bind under. A store failure is logged, not thrown: the feed itself
    // has already started either way, and the missing binding is visible (no
    // ticks reach the CRM/market series) rather than silently wrong.
    void bind_feed(const std::string& source_name,
                   ores::synthetic::domain::binding_mode binding_mode,
                   const std::string& caller_bearer_token) {
        if (binding_mode != ores::synthetic::domain::binding_mode::bound)
            return;
        if (caller_bearer_token.empty())
            return;
        const auto saved = bindings().save_if_absent(source_name, caller_bearer_token);
        if (!saved) {
            BOOST_LOG_SEV(lg(), ores::logging::warn) << "Failed to auto-create feed binding for "
                                                     << source_name << ": " << saved.error();
            return;
        }
        BOOST_LOG_SEV(lg(), ores::logging::info)
            << "Auto-created feed binding for source " << source_name;
    }

    feed_binding_store& bindings() {
        return *bindings_;
    }

    // Core vintage-availability check shared by start() and validate(). Delegates
    // to the one paged vintage read, which looks the series and the row up with the
    // caller's own bearer token when available, so the lookup runs in the caller's
    // tenant/party context rather than this service's own (system-tenant) service
    // account, which cannot see another tenant's market_observation rows under RLS.
    // Falls back to the service's own client if no token is supplied (e.g. an
    // internal/ad-hoc call with no end-user session).
    //
    // On success, @p resolved_price (if non-null) is set to the matching
    // observation's value — the real imported spot, not a placeholder.
    bool vintage_data_available(const std::string& ore_key,
                                const std::string& vintage_source,
                                const std::string& vintage_date,
                                std::string& error_detail,
                                const std::string& caller_bearer_token = {},
                                double* resolved_price = nullptr) {
        const auto datum = ores::marketdata::datum::ore_key_codec::read(ore_key);
        if (!datum) {
            error_detail = "ORE key '" + ore_key + "': " + datum.error();
            return false;
        }
        const auto series_uri = ores::marketdata::datum::oresmd_uri_codec::write(
                                    ores::marketdata::datum::series_of(*datum))
                                    .value();
        const auto datum_uri =
            ores::marketdata::datum::oresmd_uri_codec::write(*datum).value();

        const auto missing_message =
            "No vintage data found for source=" + vintage_source + ", date=" + vintage_date + ".";
        const auto found = ores::synthetic::feed::find_vintage_observation(auth_nats_,
                                                                          caller_bearer_token,
                                                                          series_uri,
                                                                          datum_uri,
                                                                          vintage_source,
                                                                          vintage_date,
                                                                          missing_message,
                                                                          ore_key);
        if (!found) {
            error_detail = found.error();
            return false;
        }
        if (resolved_price)
            *resolved_price = *found;
        return true;
    }

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
                << "SYNTHETIC STATUS: source='" << key << "' published=" << count;
        }
    }

    struct running_feed {
        std::shared_ptr<ores::marketdata::domain::IFeed> feed;
        std::thread thread;
        // The mode this feed is running with. A re-start of an already-running
        // source is gated on this, not on the re-start call's argument, so a
        // caller unaware the feed is sandboxed cannot bind it.
        ores::synthetic::domain::binding_mode binding_mode =
            ores::synthetic::domain::binding_mode::bound;
    };

    static void join_and_clear(running_feed& rf) {
        if (rf.feed)
            rf.feed->stop();
        if (rf.thread.joinable())
            rf.thread.join();
    }

    // Emplace a feed as running, spawn its tick thread, and start the
    // status thread on the first feed. Caller must already hold mu_.
    void start_running(std::shared_ptr<ores::marketdata::domain::IFeed> feed,
                       ores::synthetic::domain::binding_mode binding_mode) {
        const auto key = feed->source_name();
        auto* raw = feed.get();
        running_feed rf;
        rf.feed = std::move(feed);
        rf.binding_mode = binding_mode;
        rf.thread = std::thread([raw] { raw->start(); });
        feeds_.emplace(key, std::move(rf));
        BOOST_LOG_SEV(lg(), ores::logging::info) << "SYNTHETIC START: source='" << key << "' — now "
                                                 << feeds_.size() << " feed(s) running";
        if (!status_thread_.joinable())
            status_thread_ = std::thread(&feed_controller::status_loop, this);
    }

    // The source_name of a *different* running feed already holding @p
    // feed's conflict key, if any — a running feed with the same qualifier
    // but a *different* role (e.g. discount vs. projection) is not a
    // conflict. Excludes @p excluding_source_name so re-adding/restarting
    // the same config never self-conflicts. Caller must already hold mu_.
    std::optional<std::string> find_conflict(const ores::marketdata::domain::IFeed& feed,
                                             const std::string& excluding_source_name) const {
        const auto qualifier = feed.qualifier();
        const auto role = feed.role();
        for (const auto& [name, rf] : feeds_) {
            if (name == excluding_source_name)
                continue;
            if (feeds_conflict(rf.feed->qualifier(), rf.feed->role(), qualifier, role))
                return name;
        }
        return std::nullopt;
    }

    ores::nats::service::nats_client& auth_nats_;
    // Injected or, by default, the marketdata-over-NATS store.
    std::shared_ptr<feed_binding_store> bindings_;

    mutable std::mutex mu_;
    std::map<std::string, running_feed> feeds_;

    std::atomic<bool> stop_flag_{false};
    std::thread status_thread_;
};

/**
 * @brief The production feed_binding store: marketdata over NATS, delegated to
 * the requesting end-user's bearer token so the binding lands in that caller's
 * tenant/party context.
 */
class marketdata_binding_store final : public feed_binding_store {
public:
    explicit marketdata_binding_store(ores::nats::service::nats_client& auth_nats)
        : auth_nats_(auth_nats) {}

    /**
     * @brief The binding this store creates for @p source_name.
     *
     * The store names a generated producer, so the binding it creates carries
     * SYNTHETIC rather than the entity's VENDOR default. The ingest loop
     * stamps the series it creates from this code, so the axis travels with
     * the binding onto every series this feed's ticks land in. Public because
     * the store's rule is what a test asserts, without a live marketdata
     * service.
     */
    static ores::marketdata::domain::feed_binding binding_for(const std::string& source_name) {
        ores::marketdata::domain::feed_binding b;
        boost::uuids::random_generator uuid_gen;
        b.id = uuid_gen();
        b.source_name = source_name;
        b.producer_kind = "SYNTHETIC";
        b.enabled = true;
        b.change_reason_code = "system.new_record";
        b.change_commentary = "Auto-created by feed_controller on feed start.";
        return b;
    }

    std::expected<void, std::string>
    save_if_absent(const std::string& source_name,
                   const std::string& caller_bearer_token) override {
        auto delegated = auth_nats_.with_delegation(caller_bearer_token);
        ores::marketdata::client::market_data_client md_client(delegated);
        // Read-then-write rather than an insert that would fail on the
        // (tenant, party, source_name) unique index: a feed started, stopped
        // and restarted must keep one binding, not gain a second.
        auto existing = md_client.list_feed_bindings();
        if (!existing)
            return std::unexpected(existing.error());
        for (const auto& b : *existing)
            if (b.source_name == source_name)
                return {};

        auto saved = md_client.save_feed_binding(binding_for(source_name));
        if (!saved)
            return std::unexpected(saved.error());
        return {};
    }

private:
    ores::nats::service::nats_client& auth_nats_;
};

inline std::shared_ptr<feed_binding_store>
make_marketdata_binding_store(ores::nats::service::nats_client& auth_nats) {
    return std::make_shared<marketdata_binding_store>(auth_nats);
}

}

#endif
