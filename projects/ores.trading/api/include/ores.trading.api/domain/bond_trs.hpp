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
#ifndef ORES_TRADING_API_DOMAIN_BOND_TRS_HPP
#define ORES_TRADING_API_DOMAIN_BOND_TRS_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Per-trade bond total return swap facts: one row per TRS instrument, keyed by
 * instrument_id.
 *
 * One row per bond total return swap trade, keyed by the instrument row
 * it extends. The columns fix the ER row ("return type, funding index
 * or rate") from the return side (totalReturnData, instruments.xsd
 * lines 2313-2336) and the funding leg (fundingData lines 2297-2301
 * wrapping one legData).
 *
 * Three members of the return side ride here because no other row holds
 * them: the payer flag, the price type and the initial price. The return
 * schedule lands as schedule rows in the shared instrument-keyed
 * schedule tables, under the owner role trs, and the funding leg's own
 * terms land in the shared leg family.
 *
 * The return side states nine more members that no table holds:
 * ObservationLag, ObservationConvention, ObservationCalendar,
 * PaymentLag, PaymentConvention, PaymentCalendar, PaymentDates,
 * FXConversion and FXTerms. The corpus states none of them, so the
 * round trip is whole without them, and they are a recorded scope limit.
 */
struct bond_trs final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the bond TRS instrument this fact row extends.
     *
     * The instrument row carries the trade, workspace and party; the fact row only carries the swap
     * terms. Per the ER, no workspace column rides the fact tables.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Return type of the total return side (TotalReturn, PriceReturn).
     *
     * The flattening mapper hardcodes this value today; the reworked mapper must read it from
     * TotalReturnData.PriceType.
     */
    std::string return_type;

    /**
     * @brief Leg type of the funding leg (Fixed, Floating).
     */
    std::string funding_leg_type;

    /**
     * @brief Fixed rate of the funding leg, when the leg is fixed.
     */
    double funding_rate;

    /**
     * @brief Index code of the funding leg, when the leg is floating.
     */
    std::string funding_index;

    /**
     * @brief Flag saying the seller pays the total return.
     *
     * The schema declares the member required on the return side, so a row built from a document
     * always states it. The column is text because the document states its own spelling and export
     * re-emits that spelling rather than a decoded boolean.
     */
    std::optional<std::string> payer;

    /**
     * @brief Price type the total return is struck on (Dirty, Clean).
     *
     * The schema declares the member required. It is not the return_type column beside it: that one
     * names the return side of the swap, and this one names how the price is quoted.
     */
    std::optional<std::string> price_type;

    /**
     * @brief Initial price of the total return.
     *
     * The schema states the member as a float and declares it optional, so an unengaged column
     * means the document omitted it.
     */
    std::optional<double> initial_price;

    /**
     * @brief Username of the person who last modified this bond trs.
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
 * @brief Dispatch-key identifier for bond_trs, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_trs&) {
    return "ores.trading.bond_trs";
}

}

#endif
