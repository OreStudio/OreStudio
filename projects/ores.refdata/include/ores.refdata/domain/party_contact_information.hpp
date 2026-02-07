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
#ifndef ORES_REFDATA_DOMAIN_PARTY_CONTACT_INFORMATION_HPP
#define ORES_REFDATA_DOMAIN_PARTY_CONTACT_INFORMATION_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>

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
    std::string tenant_id;

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
     * @brief Username of the person who last modified this party contact information.
     */
    std::string recorded_by;

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
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
