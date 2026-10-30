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
#ifndef ORES_MARKETDATA_SERVICE_APP_USAGE_PLANNER_HPP
#define ORES_MARKETDATA_SERVICE_APP_USAGE_PLANNER_HPP

#if defined(_MSC_VER)
#    pragma once
#endif

#include "ores.marketdata.api/domain/demand_interval.hpp"
#include "ores.marketdata.api/domain/subscription_interval.hpp"
#include "ores.marketdata.service/app/demand_calculator.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstddef>
#include <exception>
#include <set>
#include <string>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief The use of a feed that a party holds now: a route to a source, and the subscription
 * of the party to the provider with its rights.
 */
struct subscription_state {
    ores::utility::uuid::tenant_id tenant_id = ores::utility::uuid::tenant_id::system();
    boost::uuids::uuid party_id;
    std::string provider_code;
    boost::uuids::uuid feed_id;
    bool display = false;
    bool non_display = false;
    bool derived = false;
};

/**
 * @brief One item in the demand of a party now.
 */
struct demand_state {
    ores::utility::uuid::tenant_id tenant_id = ores::utility::uuid::tenant_id::system();
    boost::uuids::uuid party_id;
    std::string provider_code;
    boost::uuids::uuid feed_id;
    std::string oresmd_uri;
};

/**
 * @brief The intervals to open and the open intervals to close.
 *
 * An interval to close is the open interval with @c interval_to set to the time of the plan, so
 * a writer sends it as the next version of the row.
 */
template <typename Interval>
struct interval_plan {
    std::vector<Interval> to_open;
    std::vector<Interval> to_close;
};

/**
 * @brief What writing an interval plan did.
 */
struct plan_outcome final {
    /// Closes that were written.
    std::size_t closed = 0;
    /// Opens that were written.
    std::size_t opened = 0;
    /// Writes the store refused.
    std::size_t failed = 0;
    /// Opens held back because a close of the same tenant failed.
    std::size_t held = 0;
};

/**
 * @brief Writes @p plan through @p write, closes first, and reports each refused write to
 * @p report.
 *
 * A close is the next version of the row that was open, so it goes first, and a tenant whose
 * close fails opens nothing until the next sync: opening first would leave the old interval
 * open beside the new one. Another tenant's writes go ahead, and the next sync plans what was
 * not written again from what the store holds.
 */
template <typename Interval, typename Write, typename Report>
plan_outcome write_plan(const interval_plan<Interval>& plan, Write&& write, Report&& report) {
    plan_outcome out;
    std::set<boost::uuids::uuid> held;
    for (const auto& i : plan.to_close) {
        try {
            write(i);
            ++out.closed;
        } catch (const std::exception& e) {
            ++out.failed;
            held.insert(i.tenant_id.to_uuid());
            report(i, e);
        }
    }
    for (const auto& i : plan.to_open) {
        if (held.contains(i.tenant_id.to_uuid())) {
            ++out.held;
            continue;
        }
        try {
            write(i);
            ++out.opened;
        } catch (const std::exception& e) {
            ++out.failed;
            report(i, e);
        }
    }
    return out;
}

/// The routes that hold a subscription with at least one right, with the rights of the
/// subscription.
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::vector<subscription_state>
subscription_states(const demand_catalogue& catalogue);

/// The items in the demand of each entitled party.
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::vector<demand_state>
demand_states(const std::vector<party_demand>& demand);

/**
 * @brief Compares the open subscription intervals with the state now.
 *
 * A state with no open interval opens one. An open interval whose state is gone, or whose rights
 * or source changed, closes. A change of source or rights therefore closes one interval and opens
 * the next. An interval already open for a state stays as it is. Every interval it plans is
 * stamped with @p actor, the account the store writes it as.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT interval_plan<domain::subscription_interval>
plan_subscription_intervals(const std::vector<subscription_state>& now_state,
                            const std::vector<domain::subscription_interval>& open,
                            std::chrono::system_clock::time_point now,
                            const std::string& actor);

/// Compares the open demand intervals with the demand now, by the same rule.
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT interval_plan<domain::demand_interval>
plan_demand_intervals(const std::vector<demand_state>& now_state,
                      const std::vector<domain::demand_interval>& open,
                      std::chrono::system_clock::time_point now,
                      const std::string& actor);
}

#endif
