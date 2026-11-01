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
#ifndef ORES_MARKETDATA_CORE_SERVICE_MARKET_STATE_READER_HPP
#define ORES_MARKETDATA_CORE_SERVICE_MARKET_STATE_READER_HPP

#include "ores.marketdata.api/domain/market_state_payload.hpp"
#include "ores.marketdata.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief One file of a run's output archive.
 */
struct market_state_file {
    /**
     * @brief The compute result that wrote the file.
     *
     * A run split into trade sets runs the engine once for each set, and each
     * run writes a file of the same name.
     */
    boost::uuids::uuid result_id;
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
 * @brief The run a payload belongs to, as every row of it is stamped.
 */
struct market_state_run {
    /**
     * @brief The party that owns the run.
     */
    boost::uuids::uuid party_id;
    /**
     * @brief The run that priced against the state.
     */
    boost::uuids::uuid report_instance_id;
    /**
     * @brief The report the run came from.
     */
    boost::uuids::uuid report_definition_id;
    /**
     * @brief The date the run priced at, which is the state's observation date.
     */
    std::chrono::system_clock::time_point as_of;
};

/**
 * @brief What reading a run's output produced, and what it left alone.
 *
 * A file whose family is not a market state is left alone rather than refused:
 * the run's results belong to reporting, and the two components read the same
 * archive.
 */
struct market_state_outcome {
    /**
     * @brief The payload rows to store, one for each market-state file.
     */
    std::vector<domain::market_state_payload> payloads;
    /**
     * @brief The files that are not market state, so the caller can account for
     * them.
     */
    std::vector<std::string> left_alone;
    /**
     * @brief The reasons a file that is market state could not be read.
     */
    std::vector<std::string> refusals;
};

/**
 * @brief Reads every market-state file of a run's output.
 *
 * The catalogue says which families state market state and which columns are
 * their keys, so this indexes a cube without knowing its shape: the payload is
 * the file as the engine wrote it, compressed, and the index beside it is the
 * headings, the row count and the distinct values of each key.
 *
 * @param files  the files the run wrote.
 * @param run    the run every row is stamped with.
 */
[[nodiscard]] ORES_MARKETDATA_CORE_EXPORT market_state_outcome
read_market_state(const std::vector<market_state_file>& files, const market_state_run& run);

}

#endif
