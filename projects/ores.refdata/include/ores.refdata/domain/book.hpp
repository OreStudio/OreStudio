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
#ifndef ORES_REFDATA_DOMAIN_BOOK_HPP
#define ORES_REFDATA_DOMAIN_BOOK_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::refdata::domain {

/**
 * @brief Operational ledger leaf that holds trades.
 *
 * Operational ledger leaves. The only entity that holds trades.
 * Serves as the basis for accounting, ownership, and regulatory
 * capital treatment. Must belong to exactly one portfolio.
 */
struct book final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief UUID uniquely identifying this book.
     *
     * Surrogate key for the book record.
     */
    boost::uuids::uuid id;

    /**
     * @brief Party that owns this book.
     *
     * References the parties table.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Book name, unique within party.
     *
     * e.g., 'FXO_EUR_VOL_01'.
     */
    std::string name;

    /**
     * @brief Optional free-text description of the book.
     */
    std::string description;

    /**
     * @brief Links to exactly one portfolio.
     *
     * Mandatory: Every book must belong to a portfolio.
     */
    boost::uuids::uuid parent_portfolio_id;

    /**
     * @brief Functional/accounting currency.
     *
     * ISO 4217 currency code.
     */
    std::string ledger_ccy;

    /**
     * @brief Reference to external General Ledger.
     *
     * e.g., 'GL-10150-FXO'. Nullable if not integrated.
     */
    std::string gl_account_ref;

    /**
     * @brief Internal finance code for P&L attribution.
     *
     * Links to cost center in finance system.
     */
    std::string cost_center;

    /**
     * @brief Lifecycle status of the book.
     *
     * References book_statuses lookup (Active, Closed, Frozen).
     */
    std::string book_status;

    /**
     * @brief Basel III/IV classification.
     *
     * 1 = Trading Book, 0 = Banking Book.
     */
    int is_trading_book;

    /**
     * @brief Username of the person who last modified this book.
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

}

#endif
