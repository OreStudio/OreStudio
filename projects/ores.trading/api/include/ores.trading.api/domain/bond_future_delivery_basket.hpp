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
#ifndef ORES_TRADING_API_DOMAIN_BOND_FUTURE_DELIVERY_BASKET_HPP
#define ORES_TRADING_API_DOMAIN_BOND_FUTURE_DELIVERY_BASKET_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One deliverable identifier of a bond future's delivery basket, keyed to the instrument and
 * the identifier's ordinal.
 *
 * One row per deliverable identifier a bond future's basket names, keyed
 * to the instrument and the identifier's ordinal.
 *
 * The schema states the basket as an unbounded list of identifiers, and
 * the ordinal preserves the document's order so export re-emits the
 * identifiers as the document held them.
 *
 * The identifier is text because the document states a name, not a
 * reference to a bond row.
 */
struct bond_future_delivery_basket final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the bond future instrument whose basket named this identifier.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Ordinal of this identifier within the basket.
     */
    int sequence_number;

    /**
     * @brief The deliverable identifier, as the document spelled it.
     *
     * The column holds the document's own text, including an empty identifier, so export re-emits
     * what the document stated.
     */
    std::string delivery_basket_id;

    /**
     * @brief Username of the person who last modified this bond future delivery basket identifier.
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
};

/**
 * @brief Dispatch-key identifier for bond_future_delivery_basket, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_future_delivery_basket&) {
    return "ores.trading.bond_future_delivery_basket";
}

}

#endif
