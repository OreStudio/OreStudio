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
#ifndef ORES_TRADING_API_DOMAIN_TRADE_SUMMARY_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_SUMMARY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One row for each trade or structure, with its book, state, counterparty, authorisation
 * status and identifiers.
 *
 * The read model under the trade list. A row holds a trade with its book, its
 * current state, its booking, its counterparty, its instrument type and its
 * authorisation status, and every identifier of the trade as one search text.
 * The service pages, sorts and filters, so the client does no joining.
 *
 * The row reads the current state of every part. It offers no as_of: a view
 * cannot take the instant, and the trade list does not need it yet.
 */
struct trade_summary final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade or the structure this row stands for.
     */
    boost::uuids::uuid id;

    /**
     * @brief Whether the row is a trade or a structure.
     */
    std::string row_kind;

    /**
     * @brief The legal entity that owns the trade or the structure.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The party the firm faces. Absent only on an intra-entity trade.
     */
    std::optional<boost::uuids::uuid> counterparty_id;

    /**
     * @brief The book the trade sits in. A structure row has none.
     */
    std::optional<boost::uuids::uuid> book_id;

    /**
     * @brief The trade type code of a trade, or the kind of a structure.
     */
    std::string instrument_type;

    /**
     * @brief The state of the trade in the trade status machine. A structure row has none.
     */
    std::optional<std::string> status;

    /**
     * @brief Where Operations stands on the trade: the state of its authorisation request,
     * not_requested when a live actual trade has no request yet, or not_required for a test trade.
     */
    std::string authorisation_status;

    /**
     * @brief The date the trade was agreed. Absent when the source does not state it.
     */
    std::optional<std::chrono::year_month_day> trade_date;

    /**
     * @brief The number of trades the row stands for: one for a trade, the member count for a
     * structure.
     */
    int trade_count = 0;

    /**
     * @brief The structure that holds the trade, when one does: null for a trade no structure
     * holds, and for a structure row.
     */
    std::optional<boost::uuids::uuid> structure_id;

    /**
     * @brief Whether the row is a leg of a structure. A boolean, so a filter can ask for the rows
     * that are legs and for the rows that are not; a null id cannot say "none".
     */
    bool is_leg = false;

    /**
     * @brief Every identifier of the trade as one text, which the list searches.
     */
    std::string search_text;

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
    friend bool operator==(const trade_summary&, const trade_summary&) = default;
};

/**
 * @brief Dispatch-key identifier for trade_summary, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const trade_summary&) {
    return "ores.trading.trade_summary";
}

}

#endif
