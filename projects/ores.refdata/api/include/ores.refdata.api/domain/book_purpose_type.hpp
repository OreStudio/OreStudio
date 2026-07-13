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
#ifndef ORES_REFDATA_API_DOMAIN_BOOK_PURPOSE_TYPE_HPP
#define ORES_REFDATA_API_DOMAIN_BOOK_PURPOSE_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The risk role a book plays: what kind of trading/operational activity happens in it.
 *
 * Reference data table defining the risk role a book plays — what kind
 * of trading/operational activity happens in it, as distinct from its
 * regulatory type (see regulatory_book_type) or how its ledger balance
 * is fed. Examples: 'Trading', 'Reserve', 'Funding', 'Wash', 'Write-off',
 * 'Test', 'Sales', 'Sweep target', 'Remittance target'.
 *
 * Book purpose types are mutually exclusive — a book carries exactly one
 * at a time — and managed by the system tenant. See
 * [[id:74AA46EB-64ED-4FD7-B212-AEC164648B84][Book classification]] for the full analysis of why
 * this is a lookup entity rather than independent boolean flags, and why the values are mutually
 * exclusive with each other but independent of regulatory type and ledger feed.
 */
struct book_purpose_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique book purpose type code.
     *
     * Examples: 'Trading', 'Reserve', 'Funding', 'Wash', 'WriteOff', 'Test', 'Sales',
     * 'SweepTarget', 'RemittanceTarget'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the book purpose type.
     */
    std::string name;

    /**
     * @brief Detailed description of the book purpose type and the process it relates to.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this book purpose type.
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
 * @brief Dispatch-key identifier for book_purpose_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const book_purpose_type&) {
    return "ores.refdata.book_purpose_type";
}

}

#endif
