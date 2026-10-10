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
#ifndef ORES_REFDATA_API_DOMAIN_NETTING_AGREEMENT_HPP
#define ORES_REFDATA_API_DOMAIN_NETTING_AGREEMENT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief A legal agreement between a legal entity of the firm and a counterparty that makes their
 * trades nettable.
 *
 * A netting agreement is the legal document, such as an ISDA Master
 * Agreement, that lets one legal entity of the firm and one counterparty
 * replace what each owes the other under it with a single net amount. It is
 * the legal fact behind a [[id:5F12A37F-9F26-4B49-BAAF-1BAA7B2BB94F][netting set]]: a set opened
 * under an agreement belongs to the agreement's two parties, and a set with no agreement holds
 * trades that do not net.
 *
 * The agreement's two parties never change across its versions: both
 * columns are fixed, so a new version that changes either is refused. A
 * netting set copies them and pins the copy to the agreement, so a set
 * cannot be filed under another counterparty's agreement, and the copy stays
 * true. Closing an agreement does not close its sets, as for every soft
 * foreign key in the schema; a set's agreement is checked when the set is
 * written.
 */
struct netting_agreement final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this netting agreement.
     *
     * Surrogate key for the agreement record.
     */
    boost::uuids::uuid id;

    /**
     * @brief The reference the two parties give the agreement.
     *
     * Unique within the legal entity that signed it.
     */
    std::string agreement_number;

    /**
     * @brief The legal entity of the firm that signed the agreement.
     *
     * References the parties table. Fixed for the agreement's life.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The counterparty that signed the agreement.
     *
     * References the counterparties table. Fixed for the agreement's life.
     */
    boost::uuids::uuid counterparty_id;

    /**
     * @brief The family of master agreement.
     *
     * ISDA for the ISDA Master Agreement; AFB and FBF for the French agreements; OTHER for any
     * other signed-off master agreement.
     */
    std::string agreement_type;

    /**
     * @brief The law the agreement is governed by.
     *
     * Free text, such as English law or New York law.
     */
    std::optional<std::string> governing_law;

    /**
     * @brief Optional description of the agreement.
     */
    std::optional<std::string> description;

    /**
     * @brief Username of the person who last modified this netting agreement.
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
    friend bool operator==(const netting_agreement&, const netting_agreement&) = default;
};

/**
 * @brief Dispatch-key identifier for netting_agreement, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const netting_agreement&) {
    return "ores.refdata.netting_agreement";
}

}

#endif
