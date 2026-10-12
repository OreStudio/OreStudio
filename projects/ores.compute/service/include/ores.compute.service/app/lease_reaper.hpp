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
#ifndef ORES_COMPUTE_SERVICE_APP_LEASE_REAPER_HPP
#define ORES_COMPUTE_SERVICE_APP_LEASE_REAPER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/client.hpp"
#include <boost/asio/awaitable.hpp>
#include <cstdint>

namespace ores::compute::service::app {

/**
 * @brief Returns the jobs of lost nodes to the queue, or ends them.
 *
 * A node holds a job under a lease that its heartbeat extends. A node that
 * stops, or loses its network, stops extending it. This reaper walks the work
 * ledger every few seconds. A claimed entry whose lease has ended and that has
 * attempts left goes back to the queue, where the next free node takes it. An
 * entry with no attempts left ends its result as Done with outcome 4, NoReply,
 * and a message that names the lease, so a person sees the failure. The entry
 * is then removed.
 *
 * Runs as an async coroutine co-spawned beside the other service pollers.
 */
class lease_reaper {
private:
    inline static std::string_view logger_name = "ores.compute.service.app.lease_reaper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    lease_reaper(std::uint32_t interval_seconds,
                 ores::nats::service::client& nats,
                 ores::database::context ctx);

    boost::asio::awaitable<void> run();

    /**
     * @brief One sweep over the ledger. Public so a test can drive it.
     */
    void sweep();

private:
    void end_exhausted();

    std::uint32_t interval_seconds_;
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
};

}

#endif
