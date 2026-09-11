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
#ifndef ORES_TRADING_API_DOMAIN_TRADE_ENVELOPE_ADDITIONAL_FIELD_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_ENVELOPE_ADDITIONAL_FIELD_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One name-and-value pair of a trade envelope's additional fields, keyed to the envelope and
 * the pair's ordinal.
 *
 * One row per additional field a document stated in a trade's envelope,
 * keyed to the envelope and the field's ordinal.
 *
 * The schema states the member as open content: any element name, in any
 * namespace, with any content. A row here keeps the name and the text of
 * one such element. The list is where a document carries the fields ORE
 * Studio does not model, so nothing here is decoded.
 *
 * The name and the value are both text and neither is constrained. A
 * document may state an empty name or an empty value, and both round trip
 * because the columns hold the document's own text rather than a decoded
 * form.
 *
 * The list order is the document's order and the ordinal preserves it.
 */
struct trade_envelope_additional_field final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the trade whose envelope stated this field.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief Ordinal of this field within the envelope's list.
     */
    int sequence_number;

    /**
     * @brief Element name of the field, as the document spelled it.
     */
    std::string name;

    /**
     * @brief Text content of the field, as the document spelled it.
     */
    std::string value;

    /**
     * @brief Username of the person who last modified this trade envelope additional field.
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
 * @brief Dispatch-key identifier for trade_envelope_additional_field, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const trade_envelope_additional_field&) {
    return "ores.trading.trade_envelope_additional_field";
}

}

#endif
