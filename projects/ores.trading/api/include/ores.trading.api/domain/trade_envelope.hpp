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
#ifndef ORES_TRADING_API_DOMAIN_TRADE_ENVELOPE_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_ENVELOPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief The envelope a document states around a trade, keyed to the trade.
 *
 * One row per trade whose document stated an envelope, keyed to the
 * trade.
 *
 * The envelope is the block a document wraps around a trade. It names a
 * counterparty, a netting set, a list of portfolios and a list of further
 * fields the document states in its own vocabulary. The schema states all
 * four as optional and the last as open content.
 *
 * The trade row already carries a counterparty and a netting set, but
 * those are the trade's own attributes and the trade states them
 * unconditionally. The envelope is the document's statement and may omit
 * either. A row here holds what the document said, so a reader
 * distinguishes an omitted member from a stated one.
 *
 * A scalar member needs no flag of its own: a null column says the
 * document omitted it and an empty string says the document stated it
 * empty. The two list members are not like that, because an omitted list
 * and a stated empty list both write no child row. Each therefore carries
 * a flag on this row that says the document stated it.
 */
struct trade_envelope final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the trade this envelope wraps.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief Counterparty name the document stated.
     *
     * The name is kept as the document spelled it rather than resolved to a counterparty row, so
     * export re-emits the document's own text.
     */
    std::optional<std::string> counter_party;

    /**
     * @brief Netting set identifier the document stated.
     */
    std::optional<std::string> netting_set_id;

    /**
     * @brief True when the document stated a portfolio list, empty or not.
     *
     * Without this flag an omitted list and a stated empty list would both write no child row and
     * export would not know which to emit.
     */
    bool has_portfolio_ids = false;

    /**
     * @brief True when the document stated an additional-fields list, empty or not.
     */
    bool has_additional_fields = false;

    /**
     * @brief Username of the person who last modified this trade envelope.
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
 * @brief Dispatch-key identifier for trade_envelope, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const trade_envelope&) {
    return "ores.trading.trade_envelope";
}

}

#endif
