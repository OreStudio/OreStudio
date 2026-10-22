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
#ifndef ORES_MARKETDATA_API_DOMAIN_MARKET_DATA_VENDOR_INSTRUMENT_HPP
#define ORES_MARKETDATA_API_DOMAIN_MARKET_DATA_VENDOR_INSTRUMENT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief One value a provider publishes, named in the vendor's own notation by an id scheme, an
 * external id and a field.
 *
 * A vendor instrument names one value that a provider publishes. It has an id scheme,
 * an external id and a field, all in the vendor's own notation, so a live adapter and
 * the simulator publish the same names. Where the vendor has no single id for what the
 * system needs, the system mints an id in the vendor's style, sets minted and writes
 * the reason in deviation_note.
 *
 * A mapping links a vendor instrument to an internal item. The system tenant and the
 * system party own the rows. Every tenant reads them and only the system tenant
 * writes them.
 */
struct market_data_vendor_instrument final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this market data vendor instrument.
     */
    boost::uuids::uuid id;

    /**
     * @brief References market_data_provider.code: the provider that owns the namespace of this id.
     */
    std::string provider_code;

    /**
     * @brief How to read the external id, for example bbg_ticker or ric.
     */
    std::string id_scheme;

    /**
     * @brief The id exactly as the vendor writes it, including spaces and punctuation, for example
     * EURUSD Curncy or EUR=. Encoding for NATS happens in the subject and never here.
     */
    std::string external_id;

    /**
     * @brief Which value of the instrument is meant, for example PX_LAST or BID.
     */
    std::string field;

    /**
     * @brief True when the system invented the id because the vendor has no single id for the
     * value. A minted id carries a deviation note.
     */
    bool minted = false;

    /**
     * @brief Why the id differs from the vendor's own, when minted is true. Empty otherwise.
     */
    std::string deviation_note;

    /**
     * @brief Username of the person who last modified this market data vendor instrument.
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
    friend bool operator==(const market_data_vendor_instrument&,
                           const market_data_vendor_instrument&) = default;
};

/**
 * @brief Dispatch-key identifier for market_data_vendor_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const market_data_vendor_instrument&) {
    return "ores.marketdata.market_data_vendor_instrument";
}

}

#endif
