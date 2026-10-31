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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_API_DOMAIN_MARKET_STATE_PAYLOAD_HPP
#define ORES_MARKETDATA_API_DOMAIN_MARKET_STATE_PAYLOAD_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief The market state a run saw, as one compressed payload with an index of its dates,
 * scenarios and factors.
 *
 * The market state an engine run priced against, kept as the file the engine
 * wrote. A cube states a column for every curve and tenor the run built — the
 * widest checked-in sample states 789 — so a row for each value would be millions
 * of rows per run, and a read that needs one factor would unpack all of them.
 *
 * The payload is therefore stored once, compressed, with an index beside it: the
 * headings, the number of rows, and the distinct values of each key the catalogue
 * names, which are the dates, the scenarios, the factors or the market objects the
 * file covers. A reader that wants one slice asks the index first and unpacks only
 * what it must.
 *
 * This is market state and not a run result: it says what the world looked like,
 * it is what the engine read, and it belongs beside the series and the fixings the
 * market data store already keeps.
 */
struct market_state_payload final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this payload.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the run the state came from.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The run whose engine read this state.
     */
    boost::uuids::uuid report_instance_id;

    /**
     * @brief The family the file belongs to, as the catalogue names it: stress_scenarios,
     * scenariostatistics, rawcube, todaysmarketcalibration.
     */
    std::string family;

    /**
     * @brief The name the engine gave the file.
     */
    std::string file_name;

    /**
     * @brief The date the run priced at, which is the state's observation date.
     */
    std::chrono::system_clock::time_point as_of;

    /**
     * @brief The report the run came from, so a definition's states are one query.
     */
    boost::uuids::uuid report_definition_id;

    /**
     * @brief How many rows the file states, not counting the heading.
     */
    int row_count = 0;

    /**
     * @brief How many columns the file states. It is the size of the index's column list and is
     * carried beside it so a reader can size a read before parsing the list.
     */
    int column_count = 0;

    /**
     * @brief The headings the file states, in its order, as a JSON array. A reader finds the
     * columns it wants here rather than in the payload.
     */
    std::string column_names;

    /**
     * @brief The distinct values of each key the catalogue names, as a JSON object of key name to
     * array, in the order the file states them. These are the dates, the scenarios, the factors or
     * the market objects the file covers.
     */
    std::string key_values;

    /**
     * @brief Whether a key stated more values than the index records. A reader that needs the exact
     * set of such a key reads the payload.
     */
    bool keys_truncated = false;

    /**
     * @brief The file as the engine wrote it, gzipped and held as base64 text, because the store's
     * columns are text and a binary column would be a second way to hold a blob.
     */
    std::string payload;

    /**
     * @brief The size of the compressed file, so a reader sizes its read from the row rather than
     * from the length of the text.
     */
    int payload_bytes = 0;

    /**
     * @brief The digest of the file as the engine wrote it, so two runs of the same state are one
     * payload and a corrupted one is detected.
     */
    std::string content_sha256;

    /**
     * @brief The family the payload came from, for provenance.
     */
    std::string source;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const market_state_payload&, const market_state_payload&) = default;
};

/**
 * @brief Dispatch-key identifier for market_state_payload, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const market_state_payload&) {
    return "ores.marketdata.market_state_payload";
}

}

#endif
