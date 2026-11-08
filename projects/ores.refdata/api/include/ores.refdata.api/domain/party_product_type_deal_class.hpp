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
#ifndef ORES_REFDATA_API_DOMAIN_PARTY_PRODUCT_TYPE_DEAL_CLASS_HPP
#define ORES_REFDATA_API_DOMAIN_PARTY_PRODUCT_TYPE_DEAL_CLASS_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The class one party works a product type as, where it differs from the catalogue.
 *
 * A party that draws the flow line elsewhere than the tenant states it here: one
 * row per (party, product type) whose class differs from the catalogue's. A party
 * with no row takes the product type's own class, and a type nobody has classified
 * reads as non_flow.
 *
 * The product type is named by the code the catalogue states, a soft reference:
 * the catalogue belongs to the trading component and this row to the party that
 * classifies it.
 */
struct party_product_type_deal_class final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The row's identifier.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party whose line this row states.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The code of the product type the party classifies.
     */
    std::string product_type_code;

    /**
     * @brief How the party works the type: flow for the plain trades signed off a few rows at a
     * time, non_flow for the structured ones that get the closer look.
     */
    std::string deal_class;

    /**
     * @brief Username of the person who last modified this party product type deal class.
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
    friend bool operator==(const party_product_type_deal_class&,
                           const party_product_type_deal_class&) = default;
};

/**
 * @brief Dispatch-key identifier for party_product_type_deal_class, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const party_product_type_deal_class&) {
    return "ores.refdata.party_product_type_deal_class";
}

}

#endif
