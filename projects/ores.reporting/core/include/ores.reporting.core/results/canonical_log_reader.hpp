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
#ifndef ORES_REPORTING_CORE_RESULTS_CANONICAL_LOG_READER_HPP
#define ORES_REPORTING_CORE_RESULTS_CANONICAL_LOG_READER_HPP

#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/service/engine_error_log.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::reporting::results {

/**
 * @brief What reading a run's log rows produced, and what it refused.
 */
struct log_read_outcome {
    /**
     * @brief The rows the node wrote, in the order the engine wrote them.
     */
    std::vector<service::engine_log_error> rows;
    /**
     * @brief The reasons the file or its rows could not be read.
     */
    std::vector<std::string> refusals;
};

/**
 * @brief Reads the log rows a node marshalled from the engine's log.
 *
 * The file is the contract between the grid and this service: the node alone
 * knows how the engine writes its log, and this reads
 * =ordinal,severity,scope,message,subject,subject_id,cause,event_time,source=.
 * A file whose header is not that, or a row with the wrong number of fields, is
 * refused rather than half read.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT log_read_outcome
read_canonical_log_csv(std::string_view csv);

}

#endif
