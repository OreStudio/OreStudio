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
#include "ores.compute.wrapper/app/application.hpp"

#include "ores.utility/version/version.hpp"

namespace ores::compute::wrapper::app {

using namespace ores::logging;

boost::asio::awaitable<void>
application::run(boost::asio::io_context& /*io_ctx*/,
    const config::options& cfg) const {

    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.compute.wrapper", 0, 1);
    BOOST_LOG_SEV(lg(), info) << "Host ID: " << cfg.host_id;
    BOOST_LOG_SEV(lg(), info) << "Work dir: " << cfg.work_dir;
    BOOST_LOG_SEV(lg(), info) << "Heartbeat interval: "
                              << cfg.heartbeat_interval_seconds << "s";

    // TODO: connect to NATS, subscribe to JetStream work queue, process jobs.

    co_return;
}

}
