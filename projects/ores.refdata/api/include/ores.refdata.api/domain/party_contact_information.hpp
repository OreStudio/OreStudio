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
#ifndef ORES_REFDATA_API_DOMAIN_PARTY_CONTACT_INFORMATION_HPP
#define ORES_REFDATA_API_DOMAIN_PARTY_CONTACT_INFORMATION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Contact details for a party organised by purpose.
 *
 * Contact details for parties organised by purpose (Legal, Operations,
 * Settlement, Billing). Each party can have one contact record per type.
 */
struct party_contact_information final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this contact information record.
     *
     * Surrogate key for the party contact information record.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party this contact information belongs to.
     *
     * References the parent party record.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The type or purpose of this contact.
     *
     * References the contact_type lookup table (e.g. Legal, Operations).
     */
    std::string contact_type;

    /**
     * @brief First line of the street address.
     *
     * Primary address line.
     */
    std::string street_line_1;

    /**
     * @brief Second line of the street address.
     *
     * Additional address line (suite, floor, etc.).
     */
    std::string street_line_2;

    /**
     * @brief City name.
     *
     * City or town of the address.
     */
    std::string city;

    /**
     * @brief State or province.
     *
     * State, province, or region.
     */
    std::string state;

    /**
     * @brief ISO 3166-1 alpha-2 country code.
     *
     * References the countries table (soft FK).
     */
    std::string country_code;

    /**
     * @brief Postal or ZIP code.
     *
     * Postal code for the address.
     */
    std::string postal_code;

    /**
     * @brief Phone number.
     *
     * Contact phone number in international format.
     */
    std::string phone;

    /**
     * @brief Email address.
     *
     * Contact email address.
     */
    std::string email;

    /**
     * @brief Web page URL.
     *
     * Contact web page address.
     */
    std::string web_page;

    /**
     * @brief Whether this is the contact to call. At most one live contact row per party may hold
     * it.
     *
     * A party holds one row per contact_type, so the rows are (Legal, Operations, Settlement,
     * Billing) and nothing in them says which one a person should ring. The mark lives on the row
     * so that it survives a reload, rather than in the session that set it.
     */
    bool is_primary = false;

    /**
     * @brief Username of the person who last modified this party contact information.
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
    friend bool operator==(const party_contact_information&,
                           const party_contact_information&) = default;
};

/**
 * @brief Dispatch-key identifier for party_contact_information, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const party_contact_information&) {
    return "ores.refdata.party_contact_information";
}

}

#endif
