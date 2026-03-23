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

namespace ores::scheduler::domain {

/**
 * @brief Execution status of a job instance as reported by pg_cron.
 *
 * Maps directly to the string values stored in cron.job_run_details.status.
 */
enum class job_status {
    starting,   ///< pg_cron has started the job but it has not yet completed.
    succeeded,  ///< The job ran to completion without error.
    failed      ///< The job encountered an error during execution.
};

} // namespace ores::scheduler::domain
