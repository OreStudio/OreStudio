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
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_OPTION_PREMIUM_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_OPTION_PREMIUM_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One entry of an option block's premium list, keyed to the instrument and the entry's
 * ordinal.
 *
 * One row per premium an option block's list states, keyed to the
 * instrument and the entry's ordinal.
 *
 * The schema states the list unbounded, and the entry carries an amount,
 * a currency, a pay date and an optional settlement block. The ordinal
 * preserves the document's order, so export re-emits the entries as the
 * document held them.
 *
 * The amount, the currency and the pay date are required within an
 * entry, so the row holds them unconditionally. The settlement block is
 * optional, and its own two required members would sit in nullable
 * columns, so the row carries has_settlement to say whether the
 * document stated the block at all.
 */
struct instrument_option_premium final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the instrument whose option block stated this premium.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Ordinal of this premium within the option block's list.
     */
    int sequence_number;

    /**
     * @brief Amount of the premium.
     */
    double amount = 0.0;

    /**
     * @brief Currency the premium pays in.
     */
    std::string currency;

    /**
     * @brief Date the premium pays on.
     *
     * The schema types this member as a date, so the generated reader has already parsed it and the
     * writer re-emits a canonical form. A date column therefore carries the member whole.
     */
    std::string pay_date;

    /**
     * @brief True when the premium states a settlement block.
     */
    bool has_settlement = false;

    /**
     * @brief Currency the premium's settlement block pays in.
     */
    std::optional<std::string> settlement_pay_currency;

    /**
     * @brief FX index the premium's settlement block fixes against.
     */
    std::optional<std::string> settlement_fx_index;

    /**
     * @brief Date the premium's settlement block fixes on.
     */
    std::optional<std::string> settlement_fixing_date;

    /**
     * @brief Username of the person who last modified this instrument option premium.
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
 * @brief Dispatch-key identifier for instrument_option_premium, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const instrument_option_premium&) {
    return "ores.trading.instrument_option_premium";
}

}

#endif
