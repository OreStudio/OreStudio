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
#ifndef ORES_REFDATA_API_DOMAIN_BOOK_CHANGE_HPP
#define ORES_REFDATA_API_DOMAIN_BOOK_CHANGE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One proposed write to a book, held for its request.
 *
 * Do not edit. Change ores.refdata.book and run
 * derive_pending_models.py.
 */
struct book_change final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID identifying this line.
     *
     * One proposed write inside a request.
     */
    boost::uuids::uuid id;

    /**
     * @brief The approval request this line belongs to.
     *
     * References the approval requests table.
     */
    boost::uuids::uuid request_id;

    /**
     * @brief The place of this line in its request, from one.
     */
    int line_no = 0;

    /**
     * @brief What the line proposes: put or delete.
     */
    std::string operation;

    /**
     * @brief The version of the live row the maker read, or zero for a new row.
     */
    int base_version = 0;

    /**
     * @brief Whether the apply has written this line. A line is applied once.
     */
    bool applied = false;

    /**
     * @brief UUID uniquely identifying this book.
     *
     * Surrogate key for the book record.
     */
    boost::uuids::uuid entity_id;

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
     * @brief Functional/accounting currency, designated by the ledger.
     *
     * ISO 4217 currency code.
     */
    std::string functional_currency;

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
     * @brief The risk role this book plays.
     *
     * References book_purpose_types lookup (Trading, Reserve, Funding, Wash, WriteOff, Test, Sales,
     * SweepTarget, RemittanceTarget). Defaults to Trading, which that lookup's own seed describes
     * as the purpose for a book that plays none of the specialised roles, for the same reason
     * book_status defaults to Active.
     */
    std::string book_purpose_type = "Trading";

    /**
     * @brief How this book's ledger balance is fed.
     *
     * References ledger_feed_types lookup (None, Automatic, Manual). Defaults to None, which that
     * lookup's own seed describes as the value for a book with no ledger connection.
     */
    std::string ledger_feed_type = "None";

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
     * @brief The sandbox the book belongs to; absent for an official book.
     *
     * A book in a sandbox is a /virtual book/ (see
     * [[id:4AB0BC63-D73A-4FC3-B9AF-16C1BB90653F][Sandbox]]): the trading system creates it, it
     * never maps to the ledger, and nothing official reads it. It sits in its sandbox's portfolio
     * tree, so its parent portfolio belongs to the same sandbox, and an official book never sits
     * under a sandbox portfolio. It carries no ledger reference and is not a sweep target. Fixed
     * for the book's life.
     */
    std::optional<boost::uuids::uuid> sandbox_id;

    /**
     * @brief Username of the person who last modified this book change.
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
    friend bool operator==(const book_change&, const book_change&) = default;
};

/**
 * @brief Dispatch-key identifier for book_change, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const book_change&) {
    return "ores.refdata.book_change";
}

}

#endif
