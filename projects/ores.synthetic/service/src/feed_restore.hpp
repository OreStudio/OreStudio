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
#ifndef ORES_SYNTHETIC_SERVICE_FEED_RESTORE_HPP
#define ORES_SYNTHETIC_SERVICE_FEED_RESTORE_HPP

#include "feed_controller.hpp"
#include "feed_kind_registry.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include <cstdint>
#include <exception>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace ores::synthetic::service {

namespace {
inline auto& feed_restore_lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.service.feed_restore");
    return instance;
}
} // namespace

/** One feed that a pass did not start, and whether another pass can start it. */
struct restore_failure final {
    std::string feed_config_id;
    std::string display_name;
    std::string reason;
    /** False when a wait cannot cure the reason: another feed holds the series. */
    bool retry = true;
};

/** What one pass over one context did. */
struct restore_report final {
    std::uint32_t started = 0;
    std::vector<restore_failure> failures;
};

/**
 * @brief One pass that starts the enabled auto-start feeds a context sees.
 *
 * A restore has no person, so the grant that a person gave stands in for them.
 * The pass makes one try for each feed and reports the feeds that did not
 * start. A feed that already runs is not a failure, so a second pass leaves the
 * running feeds as they are.
 *
 * @see The identity a feed runs as (doc/knowledge/architecture/feed_execution_identity.org)
 */
class feed_restorer final {
public:
    feed_restorer(feed_controller& ctrl,
                  const feed_kind_registry& registry,
                  ores::synthetic::feed::feed_build_context bctx)
        : ctrl_(ctrl)
        , registry_(registry)
        , bctx_(std::move(bctx)) {}

    /**
     * @param ctx The tenant and party that the feeds are read in.
     * @param only When set, the pass tries only the feeds with these config ids.
     */
    [[nodiscard]] restore_report pass(const ores::database::context& ctx,
                                      const std::set<std::string>* only = nullptr) const {
        restore_report report;
        for (const auto& target : registry_.targets(ctx)) {
            const auto& c = target.row.candidate;
            if (!c.auto_start || !target.startable())
                continue;
            if (only && !only->contains(c.feed_config_id))
                continue;
            // A system feed runs once for every tenant, so only the system
            // tenant starts it.
            if (target.container_scope == ores::synthetic::domain::scope::system &&
                !feed_kind_registry::is_system_caller(ctx))
                continue;
            try_start(target, ctx, report);
        }
        return report;
    }

private:
    void try_start(const feed_start_target& target,
                   const ores::database::context& ctx,
                   restore_report& report) const {
        const auto& c = target.row.candidate;
        const auto fail = [&](std::string reason, bool retry) {
            BOOST_LOG_SEV(feed_restore_lg(), ores::logging::warn)
                << "Feed " << c.display_name << " is not started — " << reason;
            report.failures.push_back({.feed_config_id = c.feed_config_id,
                                       .display_name = c.display_name,
                                       .reason = std::move(reason),
                                       .retry = retry});
        };
        try {
            const auto token = ctrl_.identity().run_token_for(target, ctx, {});
            if (!token)
                return fail(token.error(), true);
            auto context = bctx_;
            context.run_token = *token;
            auto attempt = registry_.make_feed(target, context);
            if (!attempt.feed)
                return fail(attempt.failure, true);
            std::string holder;
            if (ctrl_.add(std::move(attempt.feed), target.binding_mode, {}, &holder))
                ++report.started;
            else if (!holder.empty())
                fail("feed '" + holder + "' already runs for the same market data key", false);
        } catch (const std::exception& e) {
            fail(e.what(), true);
        }
    }

    feed_controller& ctrl_;
    const feed_kind_registry& registry_;
    ores::synthetic::feed::feed_build_context bctx_;
};

}

#endif
