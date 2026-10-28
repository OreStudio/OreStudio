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
#ifndef ORES_REPORTING_CORE_SERVICE_REPORT_ERROR_RECORDER_HPP
#define ORES_REPORTING_CORE_SERVICE_REPORT_ERROR_RECORDER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/service/engine_log_error.hpp"
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Writes a run's errors against the run.
 *
 * The two places a run's errors come from reach this one recorder, so that a
 * reader finds them in one table: the engine's structured log, which the run's
 * own output carries, and the platform's own account of a step it could not
 * complete, which never reaches the engine.
 *
 * A repair that cannot be written is returned rather than thrown, because the
 * caller still has a run to finish and a refusal is what it has to record.
 */
class ORES_REPORTING_CORE_EXPORT report_error_recorder final {
public:
    explicit report_error_recorder(ores::database::context ctx);

    /**
     * @brief Writes what the engine said about the run.
     *
     * @return An empty string on success, or why the errors were not written.
     */
    [[nodiscard]] std::string record_engine_errors(const std::string& report_instance_id,
                                                   const std::vector<engine_log_error>& errors);

    /**
     * @brief Writes the platform's own reason the run failed.
     *
     * The reason is about the run and names no subject: the step that did not
     * complete is not a trade, an analytic or a curve.
     *
     * @return An empty string on success, or why the error was not written.
     */
    [[nodiscard]] std::string record_run_failure(const std::string& report_instance_id,
                                                 const std::string& reason);

private:
    ores::database::context ctx_;
};

}

#endif
