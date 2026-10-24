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
#ifndef ORES_SYNTHETIC_SERVICE_FEED_RUN_CONTROL_HPP
#define ORES_SYNTHETIC_SERVICE_FEED_RUN_CONTROL_HPP

#include "feed_controller.hpp"
#include "feed_kind_registry.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include <functional>
#include <map>
#include <set>
#include <string>

namespace ores::synthetic::service {

namespace {
inline auto& feed_run_control_lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.service.feed_run_control");
    return instance;
}
} // namespace

/** What a start did, in total and by feed kind. */
struct start_outcome {
    int started = 0;
    int already_running = 0;
    int skipped = 0;
    std::map<std::string, ores::marketdata::messaging::feed_kind_counts> by_kind;
};

/** What a stop did, in total and by feed kind. */
struct stop_outcome {
    int stopped = 0;
    /** Feeds the caller may not stop: a system feed asked for from another tenant. */
    int refused = 0;
    std::map<std::string, int> by_kind;
};

/**
 * @brief Starts every feed the caller wants, and counts what happened.
 *
 * The one place that turns "start these feeds" into producer starts, so the folder
 * verbs and the verbs of the feed table behave the same: the same gate (the feed and
 * its collection are enabled), the same skips (a feed that cannot be built, or whose
 * series another running feed holds) and the same counts. Every registered kind is
 * present in the result, whether or not a feed of it was wanted.
 *
 * @p build carries the clients every builder shares. The feeds run as this service, not as the
 * person who starts them.
 */
inline start_outcome start_feeds(const feed_kind_registry& registry,
                                 feed_controller& ctrl,
                                 const ores::database::context& ctx,
                                 const ores::synthetic::feed::feed_build_context& build,
                                 const std::function<bool(const feed_start_target&)>& wanted,
                                 const std::string& scope_label) {
    using namespace ores::logging;
    start_outcome out;
    for (const auto* k : registry.all())
        out.by_kind.emplace(k->kind, ores::marketdata::messaging::feed_kind_counts{});

    for (const auto& target : registry.targets(ctx)) {
        if (!wanted(target))
            continue;
        const auto& c = target.row.candidate;
        auto& counts = out.by_kind.at(target.row.kind->kind);
        const auto skip = [&](const std::string& reason) {
            ++counts.skipped;
            BOOST_LOG_SEV(feed_run_control_lg(), warn)
                << "Skipping " << c.display_name << " under " << scope_label << " — " << reason;
        };
        // A system feed runs once for every tenant, so only the system tenant starts it.
        if (target.container_is_system && !feed_kind_registry::is_system_caller(ctx)) {
            skip("a system feed starts only in the system tenant.");
            continue;
        }
        try {
            auto attempt = registry.make_feed(target, build);
            if (!attempt.feed) {
                skip(attempt.failure);
                continue;
            }
            std::string conflicting_source_name;
            if (ctrl.add(std::move(attempt.feed), &conflicting_source_name))
                ++counts.started;
            else if (conflicting_source_name.empty())
                // A concurrent start of the same config landed between this loop's
                // checks and the add.
                ++counts.already_running;
            else
                skip("already held by running feed '" + conflicting_source_name + "'.");
        } catch (const std::exception& e) {
            skip(std::string("failed to start: ") + e.what());
        }
    }
    for (const auto& [kind, counts] : out.by_kind) {
        out.started += counts.started;
        out.already_running += counts.already_running;
        out.skipped += counts.skipped;
    }
    return out;
}

/** Stops every running feed the caller wants. Stopping reads no collection, so it uses the rows. */
inline stop_outcome stop_feeds(const feed_kind_registry& registry,
                               feed_controller& ctrl,
                               const ores::database::context& ctx,
                               const std::function<bool(const feed_kind_row&)>& wanted) {
    stop_outcome out;
    for (const auto* k : registry.all())
        out.by_kind.emplace(k->kind, 0);
    const auto system = feed_kind_registry::is_system_caller(ctx) ? std::set<boost::uuids::uuid>{} :
                                                                    registry.system_containers(ctx);
    for (const auto& row : registry.rows(ctx)) {
        if (!wanted(row))
            continue;
        if (system.contains(row.candidate.container_id)) {
            ++out.refused;
            continue;
        }
        const auto stopped = static_cast<int>(ctrl.stop(row.candidate.source_name));
        out.by_kind.at(row.kind->kind) += stopped;
        out.stopped += stopped;
    }
    return out;
}

}

#endif
