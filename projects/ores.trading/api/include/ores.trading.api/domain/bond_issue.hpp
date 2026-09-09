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
#ifndef ORES_TRADING_API_DOMAIN_BOND_ISSUE_HPP
#define ORES_TRADING_API_DOMAIN_BOND_ISSUE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief The bond issue: the terms of one ISIN (security_id), the stable row every instrument of
 * the family references.
 *
 * One row per bond issue (ISIN), the stable row every instrument of the
 * family references. The columns map one to one from bondData
 * (instruments.xsd lines 382-403): security_id, issuer, currency,
 * face_value, coupon_rate, coupon_frequency_code, day_count_code,
 * issue_date, maturity_date, settlement_days and the free description;
 * the coupon terms come from the coupon leg of the issue's LegData.
 */
struct bond_issue final {
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
     * @brief UUID uniquely identifying this bond issue.
     *
     * Surrogate key of the issue row. Instrument rows and the issue's own child rows reference it;
     * the row is stable while instruments open against it are amended.
     */
    boost::uuids::uuid issue_id;

    /**
     * @brief ISIN or other security identifier of the issue.
     *
     * Unique among the current rows of a tenant (the partial unique index below). The deduplication
     * key of the migration: one issue row per ISIN, shared by every trade of it.
     */
    std::string security_id;

    /**
     * @brief Issuer of the bond.
     */
    std::string issuer;

    /**
     * @brief ISO 4217 currency code of the bond.
     */
    std::string currency;

    /**
     * @brief Face value per unit of the bond.
     */
    double face_value = 0.0;

    /**
     * @brief Coupon rate of the bond, as a decimal.
     */
    double coupon_rate = 0.0;

    /**
     * @brief Coupon payment frequency (Annual, SemiAnnual, Quarterly).
     */
    std::string coupon_frequency_code;

    /**
     * @brief Day count convention of the bond (30/360, Actual/360, Actual/Actual).
     */
    std::string day_count_code;

    /**
     * @brief Issue date of the bond (ISO 8601 date string).
     */
    std::string issue_date;

    /**
     * @brief Maturity date of the bond (ISO 8601 date string).
     *
     * Must be after issue_date.
     */
    std::string maturity_date;

    /**
     * @brief Settlement days of the bond, a market convention of the issue.
     */
    int settlement_days = 0;

    /**
     * @brief Optional free-text description of the issue.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this bond issue.
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
 * @brief Dispatch-key identifier for bond_issue, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_issue&) {
    return "ores.trading.bond_issue";
}

}

#endif
