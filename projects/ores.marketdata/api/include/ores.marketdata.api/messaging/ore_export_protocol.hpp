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
#ifndef ORES_MARKETDATA_API_MESSAGING_ORE_EXPORT_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_ORE_EXPORT_PROTOCOL_HPP

#include <string>
#include <string_view>

namespace ores::marketdata::messaging {

/**
 * @brief Request to write the tenant's market data back out as ORE text.
 *
 * Takes no arguments: the export is the whole tenant's, because that is the
 * scope the files have. An ORE =market.txt= is a snapshot of everything a run
 * needs, not a slice of one series, and an export that could name a subset
 * would produce a file that does not reproduce anything.
 */
struct export_market_data_request {
    using response_type = struct export_market_data_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.export";
};

/**
 * @brief The two file bodies, as the caller's own text to write.
 *
 * The service returns content rather than writing it: the caller owns the file
 * system, and the same bodies are what the round-trip test compares against
 * the file it imported.
 */
struct export_market_data_response {
    bool success = false;
    std::string message;

    /**
     * @brief The =market.txt= body: one DATE<TAB>KEY<TAB>VALUE line per
     * observation, empty when the tenant has no observations.
     */
    std::string market_data_content;

    /**
     * @brief The =fixings.txt= body: one DATE<TAB>INDEX<TAB>VALUE line per
     * fixing, empty when the tenant has no fixings.
     */
    std::string fixings_content;

    int series_count = 0;
    int observation_count = 0;
    int fixing_count = 0;
};

}

#endif
