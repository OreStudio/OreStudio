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
#ifndef ORES_REPORTING_CORE_RESULTS_RUN_OUTPUT_READER_HPP
#define ORES_REPORTING_CORE_RESULTS_RUN_OUTPUT_READER_HPP

#include "ores.reporting.api/domain/report_cashflow.hpp"
#include "ores.reporting.api/domain/report_metric.hpp"
#include "ores.reporting.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <vector>

namespace ores::reporting::results {

/**
 * @brief The run a file's figures belong to, as every row of it is stamped.
 */
struct run_stamp {
    /**
     * @brief The party that owns the run.
     */
    boost::uuids::uuid party_id;
    /**
     * @brief The run that produced the figures.
     */
    boost::uuids::uuid report_instance_id;
    /**
     * @brief The report the run came from.
     */
    boost::uuids::uuid report_definition_id;
    /**
     * @brief The date the run priced at, used where the file states no date.
     */
    std::chrono::system_clock::time_point priced_at;
};

/**
 * @brief One file of a run's output archive.
 *
 * The archive is the engine's own directory of output files, so a file is named
 * as the engine named it and not by a path inside a particular host.
 */
struct output_file {
    /**
     * @brief The file's name, without a directory.
     */
    std::string name;
    /**
     * @brief The file's bytes.
     */
    std::string body;
};

/**
 * @brief What reading a run's output produced, and what it refused.
 *
 * A refusal means no row of the run must be loaded, because a partial load
 * reads as a complete one. A file whose family the catalogue does not state is
 * counted rather than refused: a run may write a file this version does not
 * model yet, and the archive still holds it.
 */
struct run_output_outcome {
    /**
     * @brief The figures whose family states one figure for each subject.
     */
    std::vector<domain::report_metric> metrics;
    /**
     * @brief The figures of the families that state a payment.
     */
    std::vector<domain::report_cashflow> cashflows;
    /**
     * @brief The reasons the output could not be read whole.
     */
    std::vector<std::string> refusals;
    /**
     * @brief The files whose family the catalogue states.
     */
    std::vector<std::string> files_read;
    /**
     * @brief The files no family covers, so nothing was read from them.
     */
    std::vector<std::string> files_not_modelled;
};

/**
 * @brief Reads every file of a run's output, one family at a time.
 *
 * The catalogue says which family a file name belongs to and how its row is
 * shaped, so this reads the files the run wrote without knowing any of their
 * formats itself. A family whose row states a payment is read as cashflows, and
 * every other family as metrics.
 *
 * @param files  the files the run wrote, in the order the archive holds them.
 * @param run    the run every row is stamped with.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT run_output_outcome
read_run_outputs(const std::vector<output_file>& files, const run_stamp& run);

}

#endif
