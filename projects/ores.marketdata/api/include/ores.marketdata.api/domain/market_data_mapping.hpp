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
#ifndef ORES_MARKETDATA_API_DOMAIN_MARKET_DATA_MAPPING_HPP
#define ORES_MARKETDATA_API_DOMAIN_MARKET_DATA_MAPPING_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief Links one vendor instrument to one internal item (oresmd datum) for a period. It belongs
 * to the provider, so every source of the provider shares it.
 *
 * A mapping links a vendor instrument (provider, id scheme, external id, field) to an
 * internal item, which is an oresmd datum. It belongs to the provider and not to a
 * source, so the live and the simulated sources of one provider share it and a party
 * that changes its source keeps its series. The mapping is many to one: several vendor
 * instruments, of different providers, can map to one item.
 *
 * A mapping holds for a period. A vendor instrument has at most one mapping that starts
 * at a given instant. The system tenant and the system party own the rows. Every
 * tenant reads them and only the system tenant writes them.
 */
struct market_data_mapping final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this market data mapping.
     */
    boost::uuids::uuid id;

    /**
     * @brief References market_data_vendor_instrument.id: the vendor instrument that this mapping
     * resolves.
     */
    boost::uuids::uuid vendor_instrument_id;

    /**
     * @brief The first instant at which the mapping holds.
     */
    std::chrono::system_clock::time_point effective_from;

    /**
     * @brief The internal item: the oresmd quote URI of the datum that the vendor instrument stands
     * for.
     */
    std::string oresmd_uri;

    /**
     * @brief The first instant at which the mapping no longer holds. Empty while it holds.
     */
    std::optional<std::chrono::system_clock::time_point> effective_to;

    /**
     * @brief Username of the person who last modified this market data mapping.
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
    friend bool operator==(const market_data_mapping&, const market_data_mapping&) = default;
};

/**
 * @brief Dispatch-key identifier for market_data_mapping, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const market_data_mapping&) {
    return "ores.marketdata.market_data_mapping";
}

}

#endif
