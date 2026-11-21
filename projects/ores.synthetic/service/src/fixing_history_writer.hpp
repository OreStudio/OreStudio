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
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <condition_variable>
#include <deque>
#include <expected>
#include <mutex>
#include <optional>
#include <set>
#include <string>
#include <thread>

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
     * @brief Queues @p plan, which a thread of the writer's own writes.
     *
     * The queue is unbounded and never sheds: one plan is one curve's history, so a run that
     * starts a hundred curves queues a hundred, which the thread writes one after another.
     */
    void enqueue(feed::ir_curve_feed::fixing_plan plan);

    /// Writes what is queued and stops the thread.
    void stop();

private:
    /**
     * @brief Writes the days of @p plan the index's series does not hold.
     *
     * A fault is logged and nothing else: a run whose history cannot be written has no
     * fixings, and the feed it belongs to still ticks.
     */
    void write(const feed::ir_curve_feed::fixing_plan& plan);
    void run();

    [[nodiscard]] std::expected<boost::uuids::uuid, std::string> system_party();
    [[nodiscard]] std::expected<boost::uuids::uuid, std::string>
    series_for(const std::string& index_uri, const boost::uuids::uuid& party);
    [[nodiscard]] std::set<std::chrono::year_month_day> held_days(const std::string& index_uri,
                                                                  const boost::uuids::uuid& party,
                                                                  std::chrono::year_month_day to);

    ores::database::context db_ctx_;
    ores::nats::service::nats_client& svc_nats_;
    std::optional<boost::uuids::uuid> system_party_;
    mutable std::mutex mu_;
    std::condition_variable ready_;
    std::deque<feed::ir_curve_feed::fixing_plan> queued_;
    bool stopping_ = false;
    std::thread thread_;
};

}

#endif
