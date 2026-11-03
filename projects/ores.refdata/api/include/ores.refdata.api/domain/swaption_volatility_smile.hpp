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
#ifndef ORES_REFDATA_API_DOMAIN_SWAPTION_VOLATILITY_SMILE_HPP
#define ORES_REFDATA_API_DOMAIN_SWAPTION_VOLATILITY_SMILE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The smile grid a swaption volatility cube is quoted on.
 *
 * ORE's swaptionVolatility is one xs:all with an optional Dimension and
 * every smile element optional. ORE reads the three smile lists only for a Smile
 * surface, and they are one block: in the ORE examples an entry states all three
 * or none. A row of this part is that block, and it exists wherever the entry
 * states one, as the FX smile part does for the FX entry. The examples also keep a
 * grid on three ATM entries whose dimension was switched from Smile, so the
 * part does not depend on the dimension, and the document round trips as written.
 *
 * The part holds SmileOptionTenors, SmileSwapTenors and SmileSpreads, each
 * a comma separated list in ORE's own text. Dimension stays on the entry,
 * because it is the entry's own setting and the smile only depends on it.
 *
 * An entry holds at most one row of this part, so the generated table adds the
 * unique index on the live rows of (tenant_id, swaption_volatility_config_id).
 */
struct swaption_volatility_smile final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key for the row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The swaption volatility entry this smile belongs to.
     */
    boost::uuids::uuid swaption_volatility_config_id;

    /**
     * @brief The option tenors of the smile grid, as ORE's SmileOptionTenors list.
     */
    std::optional<std::string> smile_option_tenors;

    /**
     * @brief The swap tenors of the smile grid, as ORE's SmileSwapTenors list.
     */
    std::optional<std::string> smile_swap_tenors;

    /**
     * @brief The strike spreads of the smile grid, as ORE's SmileSpreads list.
     */
    std::optional<std::string> smile_spreads;

    /**
     * @brief Username of the person who last modified this swaption volatility smile.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

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
    friend bool operator==(const swaption_volatility_smile&,
                           const swaption_volatility_smile&) = default;
};

/**
 * @brief Dispatch-key identifier for swaption_volatility_smile, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const swaption_volatility_smile&) {
    return "ores.refdata.swaption_volatility_smile";
}

}

#endif
