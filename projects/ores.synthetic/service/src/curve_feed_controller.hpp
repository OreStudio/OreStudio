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
#ifndef ORES_SYNTHETIC_SERVICE_CURVE_FEED_CONTROLLER_HPP
#define ORES_SYNTHETIC_SERVICE_CURVE_FEED_CONTROLLER_HPP

#include "ores.marketdata.api/domain/i_feed.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <thread>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Whether two running/candidate feeds would collide: same (qualifier, role) pair.
 * role is part of the comparison deliberately -- a discount feed and a projection feed for the
 * same qualifier are expected to coexist, not conflict. Pure and free of ir_curve_feed/NATS so
 * it's directly unit-testable (curve_feed_controller_tests.cpp) without a live NATS client,
 * which neither ir_curve_feed nor curve_feed_controller itself can be constructed without.
 */
inline bool ir_curve_feeds_conflict(const std::string& qualifier_a,
                                    const std::string& role_a,
                                    const std::string& qualifier_b,
                                    const std::string& role_b) {
    return qualifier_a == qualifier_b && role_a == role_b;
}

/**
 * @brief Owns the running ir_curve_feed producers; one tick thread per feed, keyed by
 * source_name -- same shape as feed_controller's own feeds_ map for FX.
 *
 * Supports both auto-start (add(), used at application startup for every enabled+auto_start
 * config) and on-demand start/stop/list via ir_curve_feed_config_handler's NATS control-plane,
 * mirroring feed_controller/market_feed_config_handler's manual-control surface for FX.
 *
 * Enforces one additional invariant neither source_name uniqueness nor the config-level
 * enabled/auto_start flags can, on their own: at most one running feed per published
 * (qualifier, role) pair (currency_code + index_family[+tenor] -- see ir_curve_feed::qualifier(),
 * what every consumer actually looks up market_observation rows by -- and role(), oresmd's
 * curve_role). Two different configs (different source_name, e.g. one from the basic dataset,
 * one from realistic, or a legacy-vs-recent vintage pair) can carry the identical
 * (currency_code, index_family, tenor) and would otherwise race to publish into the same
 * observation series if both were ever started with the *same* role -- but a discount config and
 * a projection config for that same currency+index+tenor are expected to coexist and must NOT be
 * treated as conflicting, which is why role is part of the conflict key rather than excluded from
 * it. Both add() and start() reject a feed whose (qualifier, role) pair already has a different
 * feed running -- never silently, and never by stopping the existing one; switching requires an
 * explicit stop() first, real consent rather than an implicit rebind.
 */
class curve_feed_controller final {
public:
    enum class start_result { started, already_running, qualifier_conflict };

    /**
     * @brief Auto-start path: adds an already-constructed feed and starts its tick thread, unless
     * its qualifier is already held by a different running feed, in which case the feed is
     * dropped without starting (auto-start must not crash the service over a seed-data
     * misconfiguration -- the caller logs the skip).
     *
     * @return false if skipped because the source_name is already running, or due to a qualifier
     * conflict (with @p out_conflicting_source_name set to the running feed's source_name), true
     * if started.
     */
    bool add(std::shared_ptr<ores::marketdata::domain::IFeed> feed,
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
        if (const auto conflict =
                find_qualifier_conflict(feed->qualifier(), feed->role(), source_name)) {
            if (out_conflicting_source_name)
                *out_conflicting_source_name = *conflict;
            return false;
        }
        auto* raw = feed.get();
        running_feed rf;
        rf.feed = feed;
        rf.qualifier = feed->qualifier();
        rf.role = feed->role();
        rf.thread = std::thread([raw] { raw->start(); });
        feeds_.emplace(source_name, std::move(rf));
        return true;
    }

    /**
     * @brief On-demand path: starts a feed by source_name if not already running, and if no
     * *different* feed already holds its qualifier. Use running_source_name_for_qualifier() to
     * build an actionable message when this returns qualifier_conflict.
     */
    start_result start(std::shared_ptr<ores::marketdata::domain::IFeed> feed) {
        std::lock_guard lock(mu_);
        const auto source_name = feed->source_name();
        if (feeds_.contains(source_name))
            return start_result::already_running;
        if (find_qualifier_conflict(feed->qualifier(), feed->role(), source_name))
            return start_result::qualifier_conflict;
        auto* raw = feed.get();
        running_feed rf;
        rf.feed = feed;
        rf.qualifier = feed->qualifier();
        rf.role = feed->role();
        rf.thread = std::thread([raw] { raw->start(); });
        feeds_.emplace(source_name, std::move(rf));
        return start_result::started;
    }

    /**
     * @brief The source_name of the running feed currently holding @p qualifier for @p role, if
     * any -- for building the "already running as X -- stop it first" message after a
     * qualifier_conflict result.
     */
    std::optional<std::string> running_source_name_for_qualifier(const std::string& qualifier,
                                                                 const std::string& role) const {
        std::lock_guard lock(mu_);
        for (const auto& [name, rf] : feeds_)
            if (rf.qualifier == qualifier && rf.role == role)
                return name;
        return std::nullopt;
    }

    /**
     * @brief Stops one feed by source_name, or all feeds if source_name is empty.
     * Returns the number of feeds stopped.
     */
    std::size_t stop(const std::string& source_name = {}) {
        std::lock_guard lock(mu_);
        if (source_name.empty()) {
            const auto n = feeds_.size();
            for (auto& [_, rf] : feeds_)
                join_and_clear(rf);
            feeds_.clear();
            return n;
        }
        auto it = feeds_.find(source_name);
        if (it == feeds_.end())
            return 0;
        join_and_clear(it->second);
        feeds_.erase(it);
        return 1;
    }

    /** @brief Snapshot of source_names for all currently running feeds. */
    std::vector<std::string> list() const {
        std::lock_guard lock(mu_);
        std::vector<std::string> names;
        names.reserve(feeds_.size());
        for (const auto& [name, _] : feeds_)
            names.push_back(name);
        return names;
    }

    std::size_t running_count() const {
        std::lock_guard lock(mu_);
        return feeds_.size();
    }

    void shutdown() {
        stop();
    }

    ~curve_feed_controller() {
        shutdown();
    }

private:
    struct running_feed {
        std::shared_ptr<ores::marketdata::domain::IFeed> feed;
        std::string qualifier;
        std::string role;
        std::thread thread;
    };

    static void join_and_clear(running_feed& rf) {
        if (rf.feed)
            rf.feed->stop();
        if (rf.thread.joinable())
            rf.thread.join();
    }

    /**
     * @brief Source_name of a *different* running feed already holding @p qualifier for the same
     * @p role, if any -- a running feed with the same qualifier but a *different* role (e.g.
     * discount vs. projection) is not a conflict. Excludes @p excluding_source_name so
     * re-adding/restarting the same config never self-conflicts. Caller must already hold mu_.
     */
    std::optional<std::string>
    find_qualifier_conflict(const std::string& qualifier,
                            const std::string& role,
                            const std::string& excluding_source_name) const {
        for (const auto& [name, rf] : feeds_)
            if (ir_curve_feeds_conflict(rf.qualifier, rf.role, qualifier, role) &&
                name != excluding_source_name)
                return name;
        return std::nullopt;
    }

    mutable std::mutex mu_;
    std::map<std::string, running_feed> feeds_;
};

}

#endif
