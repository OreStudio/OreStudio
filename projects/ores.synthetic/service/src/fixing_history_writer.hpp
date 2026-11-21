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
#ifndef ORES_SYNTHETIC_SERVICE_FIXING_HISTORY_WRITER_HPP
#define ORES_SYNTHETIC_SERVICE_FIXING_HISTORY_WRITER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.service/service/reconciler.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <expected>
#include <map>
#include <mutex>
#include <optional>
#include <set>
#include <string>

namespace ores::synthetic::service {

/**
 * @brief Writes a curve's fixing history into the market's series for its index.
 *
 * The fixings a run states stand in for a history no publisher gives, so they go into the
 * series the index already has, under the system party, and only on the days that series
 * does not hold: a value a publisher answered is the one that stands. The system party is
 * where a publisher's own fixings sit, and it is what a party's export falls back to, so
 * every party reads the run's history without the simulator knowing which parties will
 * read it.
 *
 * The writes call the market data service, which may not answer yet when the
 * curves start, or may be away for a while later. A plan stays with the writer
 * until it is written, and a reconciler runs the plans again with a growing
 * wait until each one is, so the history does not depend on which service
 * started first.
 */
class fixing_history_writer final {
private:
    [[nodiscard]] static auto& lg();

public:
    /// @param db_ctx The service's own context, which names the system party.
    /// @param svc_nats The service's authenticated client, which writes as the service.
    fixing_history_writer(const ores::database::context& db_ctx,
                          ores::nats::service::nats_client& svc_nats);
    ~fixing_history_writer();

    fixing_history_writer(const fixing_history_writer&) = delete;
    fixing_history_writer& operator=(const fixing_history_writer&) = delete;

    /**
     * @brief Keeps @p plan until it is written, and asks for a pass now.
     *
     * One plan is one curve's history, so a run that starts a hundred curves keeps a hundred.
     * A later plan for the same index replaces the one before it.
     */
    void enqueue(feed::ir_curve_feed::fixing_plan plan);

    /// Stops after the pass that is running; the plans not yet written are dropped.
    void stop();

private:
    /// Writes each plan not yet written, and keeps those that fail.
    ores::service::service::reconcile_result write_pending();

    /**
     * @brief Writes the days of @p plan the index's series does not hold.
     *
     * The error is the reason to try again. A history the plan cannot state is logged and
     * counts as written, because a second attempt states the same nothing.
     */
    std::expected<void, std::string> write(const feed::ir_curve_feed::fixing_plan& plan);

    [[nodiscard]] std::expected<boost::uuids::uuid, std::string> system_party();
    [[nodiscard]] std::expected<boost::uuids::uuid, std::string>
    series_for(const std::string& index_uri, const boost::uuids::uuid& party);
    [[nodiscard]] std::set<std::chrono::year_month_day> held_days(const std::string& index_uri,
                                                                  const boost::uuids::uuid& party,
                                                                  std::chrono::year_month_day to);

    ores::database::context db_ctx_;
    ores::nats::service::nats_client& svc_nats_;
    std::optional<boost::uuids::uuid> system_party_;

    struct pending_plan final {
        feed::ir_curve_feed::fixing_plan plan;
        /// Which enqueue the plan came from, so a pass removes only the plan it wrote.
        std::uint64_t generation = 0;
    };
    std::mutex mu_;
    std::map<std::string, pending_plan> pending_;
    std::uint64_t generation_ = 0;
    /// Last, so its thread starts after every member it reads and stops before any goes.
    ores::service::service::reconciler reconciler_;
};

}

#endif
