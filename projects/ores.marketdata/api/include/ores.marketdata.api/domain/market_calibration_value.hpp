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
#ifndef ORES_MARKETDATA_API_DOMAIN_MARKET_CALIBRATION_VALUE_HPP
#define ORES_MARKETDATA_API_DOMAIN_MARKET_CALIBRATION_VALUE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief One value the engine stated about a market object a run calibrated.
 *
 * What the market a run built came out as, one value at a time. The calibration
 * file states a row for each value of each curve, surface or model the engine
 * built: a pillar's rate and time, a day counter, a currency. The value is a number
 * as often as it is text, and the row says which, so the text is kept as stated and
 * the number beside it when there is one.
 *
 * This is market state and not a run result: it is the market the engine priced
 * against, and it belongs beside the series and the fixings the market data store
 * already keeps.
 */
struct market_calibration_value final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this value.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the run.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The run that built the market.
     */
    boost::uuids::uuid report_instance_id;

    /**
     * @brief The compute result whose output states the value. A run fans out into one result for
     * each workunit, and each result writes its own files.
     */
    boost::uuids::uuid result_id;

    /**
     * @brief The file the reader read, as the engine named it.
     */
    std::string source;

    /**
     * @brief The line of the file that states the value, counting the heading as line one.
     */
    int line = 0;

    /**
     * @brief The date the run priced at, which is the date the market was built for.
     */
    std::chrono::system_clock::time_point as_of;

    /**
     * @brief The report the run came from, so a definition's runs are one query.
     */
    boost::uuids::uuid report_definition_id;

    /**
     * @brief The kind of market object, such as yieldCurve.
     */
    std::string market_object_type;

    /**
     * @brief The engine's name for the market object, such as USD3M.
     */
    std::string market_object_id;

    /**
     * @brief The engine's name for the value, such as time or dayCounter. The file calls it
     * ResultId; it is a name and not an identifier, so it is named for what it is.
     */
    std::string result_name;

    /**
     * @brief The first key that places the value within the object, such as a pillar date, empty
     * when unstated.
     */
    std::string result_key_1;

    /**
     * @brief The second key, such as the instrument the pillar was built from, empty when unstated.
     */
    std::string result_key_2;

    /**
     * @brief The third key, empty when the object needs only two.
     */
    std::string result_key_3;

    /**
     * @brief The type the engine stated for the value, such as double or string.
     */
    std::string result_type;

    /**
     * @brief The value as the engine wrote it.
     */
    std::string value;

    /**
     * @brief The value as a number, when the engine stated it as a double.
     */
    std::optional<double> figure;

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
    friend bool operator==(const market_calibration_value&,
                           const market_calibration_value&) = default;
};

/**
 * @brief Dispatch-key identifier for market_calibration_value, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const market_calibration_value&) {
    return "ores.marketdata.market_calibration_value";
}

}

#endif
