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
#pragma once

#include "ores.database/domain/context.hpp"

namespace ores::mq::service {

/**
 * @brief Registers the MQ metrics scrape pg_cron job at service startup.
 *
 * Schedules ores_mq_scrape_metrics_fn() to run every minute via pg_cron,
 * persisting the definition in ores_scheduler_job_definitions_tbl so the
 * job is visible and controllable from the scheduler UI.
 *
 * The operation is idempotent: if a job named "ores.mq.metrics_scrape" already
 * exists in pg_cron, pg_cron updates it in place and returns the existing jobid.
 */
class mq_job_initializer final {
public:
    using context = ores::database::context;

    /**
     * @brief Register the metrics scrape job with pg_cron.
     *
     * Looks up the system party for the system tenant, then calls
     * cron_scheduler::schedule() to register:
     *   name:     "ores.mq.metrics_scrape"
     *   command:  "SELECT ores_mq_scrape_metrics_fn()"
     *   schedule: "* * * * *"  (every minute)
     *
     * @throws std::runtime_error if the system party cannot be found or
     *         if pg_cron scheduling fails.
     */
    static void initialise(const context& ctx);
};

} // namespace ores::mq::service
