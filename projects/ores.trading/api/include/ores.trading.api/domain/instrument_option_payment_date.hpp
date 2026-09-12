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
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_OPTION_PAYMENT_DATE_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_OPTION_PAYMENT_DATE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One stated payment date of an option block, keyed to the instrument and the date's
 * ordinal.
 *
 * One row per payment date an option block states outright, keyed to the
 * instrument and the date's ordinal.
 *
 * An option block states its payment dates either as this list or as a
 * rule the dates are derived from. The rule lives on the option row; the
 * list lives here. The ordinal preserves the document's order.
 */
struct instrument_option_payment_date final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the instrument whose option block stated this payment date.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Ordinal of this payment date within the option block's list.
     */
    int sequence_number;

    /**
     * @brief The payment date.
     */
    std::string payment_date;

    /**
     * @brief Username of the person who last modified this instrument option payment date.
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
 * @brief Dispatch-key identifier for instrument_option_payment_date, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const instrument_option_payment_date&) {
    return "ores.trading.instrument_option_payment_date";
}

}

#endif
