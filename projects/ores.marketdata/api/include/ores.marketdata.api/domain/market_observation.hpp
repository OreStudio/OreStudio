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
#ifndef ORES_MARKETDATA_API_DOMAIN_MARKET_OBSERVATION_HPP
#define ORES_MARKETDATA_API_DOMAIN_MARKET_OBSERVATION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief One observed value for a (series, observation_datetime, coordinate) triple; TimescaleDB
 * hypertable partitioned by observation_datetime.
 *
 * A single market data observation: the value of a series at a given
 * observation_datetime and oresmd_uri (the datum's coordinate).
 * observation_datetime is the financial valid-time (UTC); valid_from/valid_to
 * is the transaction time. Corrections replace the previous value via the
 * soft-update trigger.
 *
 * TimescaleDB hypertable partitioned by observation_datetime with 30-day chunks;
 * GIST exclusion and DELETE RULEs are incompatible with hypertables — uniqueness
 * is enforced via partial unique index and the soft-update/soft-delete trigger pair.
 *
 * No audit trail columns (version, modified_by, performed_by, change_reason_code,
 * change_commentary) — tick-level data volumes make these impractical.
 */
struct market_observation final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this observation row.
     */
    boost::uuids::uuid id;

    /**
     * @brief Party that owns this observation.
     *
     * Set server-side from the authenticated session. Enforced by RLS.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Reference to ores_marketdata_market_series_tbl(id) — identifies what was observed.
     */
    boost::uuids::uuid series_id;

    /**
     * @brief Financial valid-time: when the market value was observed (UTC). Also the hypertable
     * partition column.
     */
    std::chrono::system_clock::time_point observation_datetime;

    /**
     * @brief The datum's canonical oresmd URI: the series' URI with =type=quote= and the coordinate
     * fields of this observation, e.g.
     * =oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M&term=5Y=.
     *
     * It is the row's only identity. The ORE key is not stored: ore_key_codec writes it from the
     * datum this URI names, so a row cannot hold two identities that disagree.
     */
    std::string oresmd_uri;

    /**
     * @brief Serialised market value (numeric string; format is series-type-specific).
     */
    std::string value;

    /**
     * @brief Source tag identifying the producer channel that published this observation (e.g.
     * synthetic.v1.tick.fx_spot.eur-usd).
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
    friend bool operator==(const market_observation&, const market_observation&) = default;
};

/**
 * @brief Dispatch-key identifier for market_observation, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const market_observation&) {
    return "ores.marketdata.market_observation";
}

}

#endif
