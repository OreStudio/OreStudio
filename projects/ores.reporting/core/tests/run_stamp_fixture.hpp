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
#ifndef ORES_REPORTING_CORE_TESTS_RUN_STAMP_FIXTURE_HPP
#define ORES_REPORTING_CORE_TESTS_RUN_STAMP_FIXTURE_HPP

#include "ores.reporting.core/results/run_output_reader.hpp"
#include <boost/uuid/random_generator.hpp>
#include <chrono>

namespace ores::reporting::tests {

/**
 * @brief The stamp of a run priced on one date, with fresh ids for its party,
 * instance, definition and result.
 */
inline results::run_stamp a_run_stamp(std::chrono::sys_days priced_at) {
    return results::run_stamp{.party_id = boost::uuids::random_generator()(),
                              .report_instance_id = boost::uuids::random_generator()(),
                              .report_definition_id = boost::uuids::random_generator()(),
                              .result_id = boost::uuids::random_generator()(),
                              .priced_at = priced_at};
}

}

#endif
