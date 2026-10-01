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
#include <optional>
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
     * @brief The datum's canonical oresmd URI: the series' URI plus the coordinate keys of this
     * observation, e.g.
     * =oresmd://ir/usd?tenor=3m&type=quote&quote=ir_swap&metric=rate&maturity=5y=.
     *
     * It is the same syntax the series' own oresmd_uri uses, so a reader needs no translation
     * between the row and the URI: the coordinate keys the series does not carry are the ones this
     * row adds. The ORE-form point the registry derives (1Y, 5y,2y,atm) is a spelling of the key,
     * not of the URI, and is read from the decomposition where a key is projected rather than
     * stored here.
     */
    std::string oresmd_uri;

    /**
     * @brief The instrument key exactly as the producer wrote it, when the producer wrote one.
     *
     * An import sets this from the key it parsed off the line. It is kept because the import
     * deliberately rewrites that key: a key oresmd can name is stored under the canonical spelling
     * its own projection emits, and an FX/RATE key refdata reports as reversed is stored under the
     * corrected pair. The series' columns therefore hold the importer's text, not the file's, and
     * rebuilding a key from them returns the canonical spelling -- the right thing to query by, the
     * wrong thing to write back. This is the file's text.
     *
     * Null for an observation no file produced: a tick from a feed or a curve bootstrap has no
     * producer key to preserve, and an export rebuilds its key from the series, which is exact for
     * every row the import did not rewrite.
     */
    std::string key;

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
