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
#ifndef ORES_MARKETDATA_API_MESSAGING_IMPORT_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_IMPORT_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief Request to import ORE market.txt and/or fixings.txt content.
 *
 * The service parses both files, decomposes keys via the series key registry,
 * upserts market series catalog entries, and bulk-inserts observations and
 * fixings.  Either content field may be empty if only one file type is being
 * imported.
 */
struct import_market_data_request {
    using response_type = struct import_market_data_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.import";

    /**
     * @brief Full text content of an ORE market.txt file.
     *
     * Supports both YYYYMMDD and YYYY-MM-DD date formats, and whitespace or
     * comma delimiters.  Empty string means no market observations to import.
     */
    std::string market_data_content;

    /**
     * @brief Full text content of an ORE fixings.txt file.
     *
     * Same format rules as market_data_content.
     * Empty string means no fixings to import.
     */
    std::string fixings_content;

    /**
     * @brief Tag stamped on every imported market_observation's source field.
     *
     * Distinguishes where an observation came from (e.g. "ore.reference" for
     * a named ORE example vintage) so downstream consumers can query for a
     * specific import rather than an untagged mix. Empty string (the
     * default) preserves the pre-existing untagged behaviour.
     */
    std::string source;

    /**
     * @brief How a (date, key) pair repeated within market_data_content or
     * fixings_content is handled.
     *
     * false (default): the later occurrence wins (last-line-wins), each
     * repeat is reported in @c warnings, and the import proceeds — matches
     * real ORE example data, which does contain such repeats.
     *
     * true: same de-duplication, but each repeat is reported in @c errors
     * instead, and the affected content (market data and/or fixings,
     * whichever contained repeats) is not persisted — a strict mode for
     * callers that want repeats treated as a hard failure.
     */
    bool duplicates_are_errors = false;
};

struct import_market_data_response {
    bool success = false;
    std::string message;
    int series_count = 0;
    int observation_count = 0;
    int fixing_count = 0;

    /**
     * @brief Non-fatal issues from de-duplicating repeated (date, key)
     * pairs, one string per repeat, e.g. "market data line 12: duplicate
     * key '...' — superseded by line 34". Populated regardless of
     * duplicates_are_errors; which of warnings/errors they land in depends
     * on it.
     */
    std::vector<std::string> warnings;

    /**
     * @brief Same shape as warnings, populated only when
     * duplicates_are_errors is true. A non-empty errors means the content
     * with repeats (market data and/or fixings) was not persisted.
     */
    std::vector<std::string> errors;
};

}

#endif
