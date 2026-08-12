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

#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/i_feed.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
#include "ores.synthetic.api/domain/binding_mode.hpp"
#include "ores.synthetic.api/feeds/fx_spot_feed.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <atomic>
#include <chrono>
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

using ores::synthetic::feed::fx_spot_feed;
using ores::synthetic::feed::synthetic_producer_subject;

/**
 * @brief Whether start() should auto-create a marketdata feed_binding for
 * this call.
 *
 * Gated on @p stored_binding_mode (the mode the feed *actually* holds --
 * i.e. what it was started with) when @p already_running is true, not on
 * @p requested_binding_mode (this call's argument). A feed keeps whatever
 * mode it was started with; a later start() through a binding_mode-unaware
 * caller (e.g. the ad-hoc NATS control-plane, which always passes the
 * bound default) must not be able to flip a running sandboxed feed's
 * binding decision just by not knowing about it. For a brand-new feed
 * (@p already_running false), @p stored_binding_mode is meaningless (no
 * feed exists yet) and @p requested_binding_mode governs, since that is
 * what the feed is about to be started with.
 */
inline bool should_ensure_feed_binding(bool already_running,
                                       ores::synthetic::domain::binding_mode requested_binding_mode,
                                       ores::synthetic::domain::binding_mode stored_binding_mode) {
    const auto effective = already_running ? stored_binding_mode : requested_binding_mode;
    return effective == ores::synthetic::domain::binding_mode::bound;
}

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
 * Three paths register a feed:
 *   - add() — the config-driven path (auto-start at boot, the folder
 *     cascade): the caller has already built the feed from persisted
 *     config via the factory, including any vintage resolution the
 *     builder performed. Returns false (and never starts) when the
 *     source_name is already running or the conflict key is held.
 *   - start(IFeed) — the on-demand path (the per-config control-plane),
 *     distinguishing already_running from qualifier_conflict.
 *   - start(ore_key, ...) — the client-supplied-params path, the FX-only
 *     ad-hoc surface (raw GMM parameters over the wire) that the
 *     per-config control-plane task deletes. It is the only path that
 *     performs the vintage-availability check and feed-binding
 *     auto-creation; the config-driven path does neither.
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
    feed_controller(ores::nats::service::client& nats, ores::nats::service::nats_client& auth_nats)
        : nats_(nats)
        , auth_nats_(auth_nats) {}

    ~feed_controller() {
        stop_flag_.store(true, std::memory_order_relaxed);
        if (status_thread_.joinable())
            status_thread_.join();
        shutdown();
    }

    enum class start_result { started, already_running, qualifier_conflict, vintage_data_missing };

    /**
     * @brief Start one producer feed. Keyed by source_name (unique per producer).
     *
     * Derives the producer subject from source_name, builds the process and
     * spawns its tick thread. Series resolution is handled lazily by the
     * marketdata ingest loop on first tick arrival — the synthetic service has
     * no marketdata writes to perform.
     *
     * If @p vintage_source is non-empty ("vintage" price_source), the feed's
     * required vintage data (source, date) is checked in market_observation
     * before anything is started; a missing vintage returns
     * vintage_data_missing with @p error_detail set to an actionable message,
     * and no feed is spawned. On success, the feed is seeded from the real
     * imported spot value rather than @p initial_price (which is a sentinel
     * 0 in vintage mode — see fx_spot_generation_config.price_source). An
     * empty vintage_source skips the check entirely and uses @p
     * initial_price as-is ("fixed" price_source, or an ad-hoc/default
     * request).
     *
     * @p caller_bearer_token, when non-empty, is forwarded as
     * X-Delegated-Authorization on the vintage lookup so it runs in the
     * calling user's tenant/party context — market_observation is
     * tenant-scoped (RLS), and this service's own service-account token is
     * bound to the system tenant, which cannot see another tenant's data.
     *
     * @p binding_mode selects the publish namespace: =bound= (the default —
     * matches every existing caller, including the ad-hoc NATS control-plane,
     * which has no config/binding_mode concept) publishes on
     * "synthetic.v1.tick.<source>", the subject the marketdata ingest loop
     * subscribes real feed_binding rows to. =sandboxed= publishes on
     * "synthetic.v1.sandbox.tick.<source>" instead — a different subject the
     * ingest loop never subscribes to, regardless of any feed_binding row —
     * and skips auto-creating a feed_binding entirely, since a binding
     * naming this source would misleadingly claim ingestion is happening on
     * the bound subject when it is not. This is what "provably excluded from
     * bound-feed resolution" means here: the exclusion is structural (a
     * different subject the consumer never listens to), not merely a
     * missing opt-in.
     *
     * The conflict rule is the uniform one: a feed whose conflict key is
     * already held by a different running feed returns qualifier_conflict
     * with @p out_conflicting_source_name set to the holder's source_name.
     */
    start_result start(const std::string& ore_key,
                       const std::string& source_name,
                       std::vector<double> means,
                       std::vector<double> stdevs,
                       std::vector<double> weights,
                       double initial_price,
                       double ticks_per_hour,
                       const std::string& process_type = "geometric",
                       const std::string& vintage_source = {},
                       const std::string& vintage_date = {},
                       std::string* error_detail = nullptr,
                       const std::string& caller_bearer_token = {},
                       ores::synthetic::domain::binding_mode binding_mode =
                           ores::synthetic::domain::binding_mode::bound,
                       std::string* out_conflicting_source_name = nullptr) {
        if (!vintage_source.empty()) {
            std::string detail;
            double resolved_price = 0.0;
            if (!vintage_data_available(ore_key,
                                        vintage_source,
                                        vintage_date,
                                        detail,
                                        caller_bearer_token,
                                        &resolved_price)) {
                if (error_detail)
                    *error_detail = std::move(detail);
                return start_result::vintage_data_missing;
            }
            initial_price = resolved_price;
        }

        const std::string key = source_name.empty() ? ore_key : source_name;
        bool already_running = false;
        // Only meaningful when already_running -- see should_ensure_feed_binding's
        // doc comment for why an already-running feed's *stored* mode, not this
        // call's argument, must govern the ensure_feed_binding gate below.
        ores::synthetic::domain::binding_mode stored_binding_mode = binding_mode;
        {
            std::lock_guard lock(mu_);
            already_running = feeds_.contains(key);
            if (already_running)
                stored_binding_mode = feeds_.at(key).binding_mode;
            if (!already_running) {
                // Use a persistent random_device so the OS entropy pool is not
                // re-seeded between rapid successive calls (which can produce
                // equal values on some platforms when called on separate
                // temporaries).
                static std::random_device rd;
                const std::uint32_t seed = rd();
                BOOST_LOG_SEV(lg(), ores::logging::info)
                    << "SYNTHETIC SEED: source='" << key << "' seed=" << seed;
                auto process = ores::analytics::quant::service::process_factory::make_process(
                    process_type,
                    std::move(means),
                    std::move(stdevs),
                    std::move(weights),
                    initial_price,
                    seed);
                auto feed =
                    std::make_shared<fx_spot_feed>(nats_,
                                                   ore_key,
                                                   key,
                                                   synthetic_producer_subject(key, binding_mode),
                                                   std::move(process),
                                                   ticks_per_hour);
                if (const auto conflict = find_conflict(*feed, key)) {
                    if (out_conflicting_source_name)
                        *out_conflicting_source_name = *conflict;
                    return start_result::qualifier_conflict;
                }
                start_running(std::move(feed), binding_mode);
            }
        }
        // Binding creation does a blocking NATS round-trip -- always done
        // after mu_ has been released, for both outcomes, so a caller
        // re-starting an already-running feed (e.g. "enable all") never
        // blocks every other start()/stop()/running_count()/list() call for
        // the duration of that round-trip. Skipped entirely for sandboxed
        // feeds: a feed_binding on this source_name would claim ingestion is
        // happening on the bound subject, which is not true (see start()'s
        // doc comment on binding_mode and should_ensure_feed_binding).
        if (should_ensure_feed_binding(already_running, binding_mode, stored_binding_mode))
            ensure_feed_binding(ore_key, key, caller_bearer_token);
        return already_running ? start_result::already_running : start_result::started;
    }

    /**
     * @brief Config-driven path: registers an already-constructed feed as
     * running and spawns its tick thread (auto-start at boot, the folder
     * cascade). Unlike the client-supplied-params start(), it does no
     * vintage check, binding auto-creation, or process construction — the
     * caller built the feed from persisted config via the factory,
     * including any vintage resolution the builder performed.
     *
     * @p binding_mode is stored with the running feed purely as the
     * client-supplied-params start()'s restart gate (see
     * should_ensure_feed_binding): the config path itself never creates a
     * feed_binding, but a later ad-hoc start() of the same source must see
     * the mode this feed actually started with — a sandboxed feed must not
     * gain a bound feed_binding through a caller that never knew about its
     * mode. Callers that built the feed with a sandboxed mode (the folder
     * cascade, the boot auto-start walk) pass it through here.
     *
     * Returns false without starting when a feed with the same source_name
     * is already running, or when the feed's conflict key is held by a
     * different running feed (with @p out_conflicting_source_name set to
     * the holder's source_name).
     */
    bool add(std::shared_ptr<ores::marketdata::domain::IFeed> feed,
             ores::synthetic::domain::binding_mode binding_mode,
             std::string* out_conflicting_source_name = nullptr) {
        std::lock_guard lock(mu_);
        const auto source_name = feed->source_name();
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
        return true;
    }

    /** @brief add() with the default (bound) binding mode. */
    bool add(std::shared_ptr<ores::marketdata::domain::IFeed> feed,
             std::string* out_conflicting_source_name) {
        return add(std::move(feed),
                   ores::synthetic::domain::binding_mode::bound,
                   out_conflicting_source_name);
    }

    /**
     * @brief On-demand path: starts a feed by source_name if not already
     * running, and if no *different* feed already holds its conflict key.
     * Use running_source_name_for_conflict_key() to build an actionable
     * message when this returns qualifier_conflict.
     */
    start_result start(std::shared_ptr<ores::marketdata::domain::IFeed> feed) {
        std::lock_guard lock(mu_);
        const auto source_name = feed->source_name();
        if (feeds_.contains(source_name))
            return start_result::already_running;
        if (find_conflict(*feed, source_name))
            return start_result::qualifier_conflict;
        start_running(std::move(feed));
        return start_result::started;
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
    // ISO date part of a observation_datetime, e.g. "2016-02-05" from a
    // timestamp at any time-of-day on that date (observations are always
    // recorded at midnight UTC for date-only vintages, but this tolerates
    // otherwise).
    static std::string date_part(std::chrono::system_clock::time_point tp) {
        const auto days = std::chrono::floor<std::chrono::days>(tp);
        return std::format("{:%F}", days);
    }

    // Auto-creates the marketdata feed_binding for a feed once it is
    // running, so ingestion is wired up without any caller (Qt, ores.shell,
    // a wt workflow step) having to know that bindings are a separate
    // concept from starting a feed. Idempotent — checks for an existing
    // (ore_key, source_name) binding first, since start() calls this on
    // every start (including the already-running path) so a feed started
    // once, stopped, then restarted stays bound instead of silently
    // ticking with nothing ingesting it.
    //
    // Silently skipped (not an error) when there is no caller bearer token
    // — an internal/ad-hoc start with no end-user session has no party to
    // bind under. Failures are logged but non-fatal: the feed itself has
    // already started either way, and a missing binding is visible (no
    // ticks reach the CRM/market series) rather than silently wrong.
    void ensure_feed_binding(const std::string& ore_key,
                             const std::string& source_name,
                             const std::string& caller_bearer_token) {
        if (caller_bearer_token.empty())
            return;

        auto delegated_nats = auth_nats_.with_delegation(caller_bearer_token);
        ores::marketdata::client::market_data_client md_client(delegated_nats);

        auto existing = md_client.list_feed_bindings();
        if (!existing) {
            BOOST_LOG_SEV(lg(), ores::logging::warn)
                << "Could not check existing feed bindings for " << source_name << ": "
                << existing.error();
            return;
        }
        for (const auto& b : *existing)
            if (b.ore_key == ore_key && b.source_name == source_name)
                return; // already bound

        ores::marketdata::domain::feed_binding b;
        boost::uuids::random_generator uuid_gen;
        b.id = uuid_gen();
        b.ore_key = ore_key;
        b.source_name = source_name;
        b.enabled = true;
        b.change_reason_code = "system.new_record";
        b.change_commentary = "Auto-created by feed_controller on feed start.";
        const auto saved = md_client.save_feed_binding(b);
        if (!saved) {
            BOOST_LOG_SEV(lg(), ores::logging::warn) << "Failed to auto-create feed binding for "
                                                     << source_name << ": " << saved.error();
            return;
        }
        BOOST_LOG_SEV(lg(), ores::logging::info)
            << "Auto-created feed binding: " << ore_key << " <- " << source_name;
    }

    // Core vintage-availability check shared by start() and validate(). Uses a
    // market_data_client delegated with the caller's own bearer token when
    // available, so the lookup runs in the caller's tenant/party context
    // rather than this service's own (system-tenant) service account, which
    // cannot see another tenant's market_observation rows under RLS. Falls
    // back to the service's own client if no token is supplied (e.g. an
    // internal/ad-hoc call with no end-user session).
    //
    // On success, @p resolved_price (if non-null) is set to the matching
    // observation's value — the real imported spot, not a placeholder — so
    // callers in "vintage" mode can seed the process from it instead of an
    // arbitrary/zero initial price.
    bool vintage_data_available(const std::string& ore_key,
                                const std::string& vintage_source,
                                const std::string& vintage_date,
                                std::string& error_detail,
                                const std::string& caller_bearer_token = {},
                                double* resolved_price = nullptr) {
        const auto missing_message = [&] {
            return "No vintage data found for source=" + vintage_source + ", date=" + vintage_date +
                   ".";
        };

        const auto key =
            ores::marketdata::core::oresmd_projections::split_market_series_key(ore_key);
        if (!key) {
            error_detail = "Cannot parse ORE key '" + ore_key + "'.";
            return false;
        }

        auto delegated_nats = auth_nats_.with_delegation(caller_bearer_token);
        ores::marketdata::client::market_data_client md_client(delegated_nats);

        auto series = md_client.find_series(key->series_type, key->metric, key->qualifier);
        if (!series) {
            error_detail = "Failed to look up series for '" + ore_key + "': " + series.error();
            return false;
        }
        if (!series->has_value()) {
            error_detail = missing_message();
            return false;
        }

        // Paged scan, not a single unbounded fetch: a series with a long
        // tick history (this service's own synthetic ticks accumulate
        // fast) can produce a response larger than NATS's max payload,
        // which fails silently -- the handler completes server-side but
        // the reply never arrives, so the caller just sees a timeout.
        // Observations come back newest-first, so a vintage lookup for a
        // recent-ish date converges in the first page or two; only a very
        // old vintage date pays for a full scan.
        constexpr std::uint32_t page_size = 200;
        const auto series_id_str = boost::uuids::to_string((*series)->id);
        std::uint32_t offset = 0;
        for (;;) {
            auto observations = md_client.list_observations_page(series_id_str, offset, page_size);
            if (!observations) {
                error_detail =
                    "Failed to look up observations for '" + ore_key + "': " + observations.error();
                return false;
            }
            for (const auto& obs : *observations) {
                if (obs.source == vintage_source && obs.point_id == "SPOT" &&
                    date_part(obs.observation_datetime) == vintage_date) {
                    if (resolved_price) {
                        try {
                            *resolved_price = std::stod(obs.value);
                        } catch (const std::exception& e) {
                            error_detail = "Vintage observation value '" + obs.value +
                                           "' is not a valid number: " + e.what();
                            return false;
                        }
                    }
                    return true;
                }
            }
            if (observations->size() < page_size)
                break;
            offset += page_size;
        }
        error_detail = missing_message();
        return false;
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
        // The ad-hoc start()'s restart gate (see should_ensure_feed_binding);
        // the config-driven add() path never reads it.
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
                       ores::synthetic::domain::binding_mode binding_mode =
                           ores::synthetic::domain::binding_mode::bound) {
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

    ores::nats::service::client& nats_;
    ores::nats::service::nats_client& auth_nats_;

    mutable std::mutex mu_;
    std::map<std::string, running_feed> feeds_;

    std::atomic<bool> stop_flag_{false};
    std::thread status_thread_;
};

}

#endif
