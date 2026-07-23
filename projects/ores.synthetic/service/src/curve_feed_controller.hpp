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

#include "ir_curve_feed.hpp"
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <thread>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Owns the running ir_curve_feed producers; one tick thread per feed, keyed by
 * source_name -- same shape as feed_controller's own feeds_ map for FX.
 *
 * Supports both auto-start (add(), used at application startup for every enabled+auto_start
 * config) and on-demand start/stop/list via ir_curve_feed_config_handler's NATS control-plane,
 * mirroring feed_controller/market_feed_config_handler's manual-control surface for FX.
 *
 * Enforces one additional invariant neither source_name uniqueness nor the config-level
 * enabled/auto_start flags can, on their own: at most one running feed per published qualifier
 * (currency_code + index_name -- see ir_curve_feed::qualifier(), what every consumer actually
 * looks up market_observation rows by). Two different configs (different source_name, e.g. one
 * from the basic dataset, one from realistic, or a legacy-vs-recent vintage pair) can carry the
 * identical (currency_code, index_name) and would otherwise race to publish into the same
 * observation series if both were ever started. Both add() and start() reject a feed whose
 * qualifier already has a different feed running -- never silently, and never by stopping the
 * existing one; switching requires an explicit stop() first, real consent rather than an implicit
 * rebind.
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
     * @return false if skipped due to a qualifier conflict (with @p out_conflicting_source_name
     * set to the running feed's source_name), true if started.
     */
    bool add(std::shared_ptr<ir_curve_feed> feed,
             std::string* out_conflicting_source_name = nullptr) {
        std::lock_guard lock(mu_);
        const auto source_name = feed->source_name();
        if (const auto conflict = find_qualifier_conflict(feed->qualifier(), source_name)) {
            if (out_conflicting_source_name)
                *out_conflicting_source_name = *conflict;
            return false;
        }
        auto* raw = feed.get();
        running_feed rf;
        rf.feed = feed;
        rf.qualifier = feed->qualifier();
        rf.thread = std::thread([raw] { raw->start(); });
        feeds_.emplace(source_name, std::move(rf));
        return true;
    }

    /**
     * @brief On-demand path: starts a feed by source_name if not already running, and if no
     * *different* feed already holds its qualifier. Use running_source_name_for_qualifier() to
     * build an actionable message when this returns qualifier_conflict.
     */
    start_result start(std::shared_ptr<ir_curve_feed> feed) {
        std::lock_guard lock(mu_);
        const auto source_name = feed->source_name();
        if (feeds_.contains(source_name))
            return start_result::already_running;
        if (find_qualifier_conflict(feed->qualifier(), source_name))
            return start_result::qualifier_conflict;
        auto* raw = feed.get();
        running_feed rf;
        rf.feed = feed;
        rf.qualifier = feed->qualifier();
        rf.thread = std::thread([raw] { raw->start(); });
        feeds_.emplace(source_name, std::move(rf));
        return start_result::started;
    }

    /**
     * @brief The source_name of the running feed currently holding @p qualifier, if any --
     * for building the "already running as X -- stop it first" message after a qualifier_conflict
     * result.
     */
    std::optional<std::string>
    running_source_name_for_qualifier(const std::string& qualifier) const {
        std::lock_guard lock(mu_);
        for (const auto& [name, rf] : feeds_)
            if (rf.qualifier == qualifier)
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
        std::shared_ptr<ir_curve_feed> feed;
        std::string qualifier;
        std::thread thread;
    };

    static void join_and_clear(running_feed& rf) {
        if (rf.feed)
            rf.feed->stop();
        if (rf.thread.joinable())
            rf.thread.join();
    }

    /**
     * @brief Source_name of a *different* running feed already holding @p qualifier, if any --
     * excludes @p excluding_source_name so re-adding/restarting the same config never
     * self-conflicts. Caller must already hold mu_.
     */
    std::optional<std::string>
    find_qualifier_conflict(const std::string& qualifier,
                            const std::string& excluding_source_name) const {
        for (const auto& [name, rf] : feeds_)
            if (rf.qualifier == qualifier && name != excluding_source_name)
                return name;
        return std::nullopt;
    }

    mutable std::mutex mu_;
    std::map<std::string, running_feed> feeds_;
};

}

#endif
