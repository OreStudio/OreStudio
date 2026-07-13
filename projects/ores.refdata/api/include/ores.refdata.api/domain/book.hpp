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
#ifndef ORES_REFDATA_API_DOMAIN_BOOK_HPP
#define ORES_REFDATA_API_DOMAIN_BOOK_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

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
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Workspace this record belongs to.
     *
     * Defaults to the Live workspace sentinel.
     */
    boost::uuids::uuid workspace_id = utility::uuid::live_workspace_id();

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
     * @brief Business unit that owns this book.
     *
     * Optional FK to business_units. Should reference a unit that is an owner_unit_id in the book's
     * portfolio ancestry chain.
     */
    std::optional<boost::uuids::uuid> owner_unit_id;

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
     * References book_statuses lookup (Active, Closed, Frozen). Defaults to Active so a
     * freshly-constructed book (the Add dialog, before the Status combo's async populate completes)
     * always carries a value the FK-validation trigger accepts.
     */
    std::string book_status = "Active";

    /**
     * @brief Basel III/IV FRTB trading book / banking book classification.
     *
     * References regulatory_book_types lookup (Trading, Banking). Defaults to Trading for the same
     * reason book_status defaults to Active.
     */
    std::string regulatory_book_type = "Trading";

    /**
     * @brief Whether this book is eligible for spot-sweep transfers to the designated Sweep target
     * book -- independent of ledger_feed_type and book_purpose_type; see
     * [[id:74AA46EB-64ED-4FD7-B212-AEC164648B84][Book classification]].
     */
    bool is_sweepable = false;

    /**
     * @brief Rates centre determining which revaluation market data snapshot this book uses at
     * end-of-day; see [[id:D18DF500-2C6C-42FF-BBAE-D5A46D410910][Book groups and rates centres]].
     *
     * Soft FK to business_centres by code (e.g. "GBLO", "USNY"), reusing the same location
     * reference already used by counterparty/party/business_unit rather than modeling a dedicated
     * rates-centre entity. Defaults to WRLD (the global sentinel business centre, seeded for every
     * tenant) for the same reason book_status defaults to Active -- a freshly-constructed book
     * always carries a value the FK-validation trigger accepts.
     */
    std::string rates_centre_code = "WRLD";

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

/**
 * @brief Dispatch-key identifier for book, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const book&) {
    return "ores.refdata.book";
}

}

#endif
