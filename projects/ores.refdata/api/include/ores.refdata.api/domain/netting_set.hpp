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
#ifndef ORES_REFDATA_API_DOMAIN_NETTING_SET_HPP
#define ORES_REFDATA_API_DOMAIN_NETTING_SET_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The trades with one counterparty that may legally offset each other for credit exposure.
 *
 * A netting set is the group of trades with one counterparty whose values
 * offset one another when credit exposure is computed. Every trade names
 * exactly one set, as ORE requires; a trade with nothing to net against
 * sits in a set of its own. See [[id:5F12A37F-9F26-4B49-BAAF-1BAA7B2BB94F][Netting sets]] for the
 * analysis.
 *
 * A set opened under a [[id:6AADB1A7-1229-4D10-8951-58446788957F][netting agreement]] copies the
 * agreement's counterparty and legal entity, and a pinned key holds the copy to the agreement, so a
 * set cannot be filed under another counterparty's agreement. A set with no agreement holds trades
 * that do not net; its counterparty may be unknown, as it is for a set read from an ORE document,
 * which names none. Every set belongs to a legal entity of the firm, and the
 * entity that imports the document is the one it belongs to.
 *
 * The code is the netting set id ORE uses. The call type and initial margin
 * type are the remaining parts of ORE's netting set details, and the risk
 * weight is ORE's.
 */
struct netting_set final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this netting set.
     *
     * Surrogate key for the netting set record.
     */
    boost::uuids::uuid id;

    /**
     * @brief The netting set id, as ORE names it.
     *
     * Unique within the legal entity the set belongs to. A trade's envelope names its set by this
     * code.
     */
    std::string code;

    /**
     * @brief The legal entity of the firm the set belongs to.
     *
     * References the parties table. Fixed for the set's life, so the CSAs and the identifiers that
     * take their party from the set can never be left on another party. When the set has an
     * agreement it equals the agreement's.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The agreement the set is opened under.
     *
     * Absent when the trades in the set do not net.
     */
    std::optional<boost::uuids::uuid> netting_agreement_id;

    /**
     * @brief The counterparty whose trades the set holds.
     *
     * Required when the set has an agreement, and then equal to the agreement's.
     */
    std::optional<boost::uuids::uuid> counterparty_id;

    /**
     * @brief ORE's call type for the set.
     *
     * Part of ORE's netting set details. Free text: ORE's schema types it as a plain string and no
     * example uses it, so there is no list to check against.
     */
    std::optional<std::string> call_type;

    /**
     * @brief ORE's initial margin type for the set.
     *
     * Part of ORE's netting set details. Free text: ORE's schema types it as a plain string and no
     * example uses it, so there is no list to check against.
     */
    std::optional<std::string> initial_margin_type;

    /**
     * @brief ORE's risk weight for the set.
     *
     * Not negative.
     */
    std::optional<double> risk_weight;

    /**
     * @brief Optional description of the set.
     */
    std::optional<std::string> description;

    /**
     * @brief Username of the person who last modified this netting set.
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
    friend bool operator==(const netting_set&, const netting_set&) = default;
};

/**
 * @brief Dispatch-key identifier for netting_set, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const netting_set&) {
    return "ores.refdata.netting_set";
}

}

#endif
