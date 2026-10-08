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
#ifndef ORES_REFDATA_DOMAIN_COUNTERPARTY_BUSINESS_CENTRE_HPP
#define ORES_REFDATA_DOMAIN_COUNTERPARTY_BUSINESS_CENTRE_HPP

#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Links a counterparty to a business centre it deals through.
 *
 * Many-to-many junction between counterparty and business_centre. A counterparty
 * deals through any number of centres: one is the usual case, and a bank that
 * books in London and Singapore states both.
 *
 * This replaces the single business_center_code column the counterparty used to
 * carry, which could only ever name one centre and left the second as a gap in
 * journey 5.
 */
struct counterparty_business_centre final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief ID of the counterparty.
     *
     * References ores_refdata_counterparties_tbl.id (soft FK).
     */
    boost::uuids::uuid counterparty_id;

    /**
     * @brief Code of the business centre this counterparty deals through.
     *
     * References ores_refdata_business_centres_tbl.code (soft FK).
     */
    std::string business_centre_code;

    /**
     * @brief Username of the person who last modified this counterparty business centre.
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
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality, on the same terms as an entity's.
     */
    friend bool operator==(const counterparty_business_centre&,
                           const counterparty_business_centre&) = default;
};

/**
 * @brief Dispatch-key identifier for counterparty_business_centre, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const counterparty_business_centre&) {
    return "ores.refdata.counterparty_business_centre";
}

}

#endif
