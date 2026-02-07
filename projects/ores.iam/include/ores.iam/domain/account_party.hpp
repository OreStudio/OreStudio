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
#ifndef ORES_IAM_DOMAIN_ACCOUNT_PARTY_HPP
#define ORES_IAM_DOMAIN_ACCOUNT_PARTY_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::iam::domain {

/**
 * @brief Links an IAM account to a party.
 *
 * Junction table linking IAM accounts to parties. Each account can be
 * associated with one or more parties, controlling which parties a user
 * can act on behalf of.
 */
struct account_party final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief ID of the IAM account.
     *
     * References ores_iam_accounts_tbl.id (soft FK).
     */
    boost::uuids::uuid account_id;

    /**
     * @brief ID of the party this account is associated with.
     *
     * References ores_refdata_parties_tbl.id (soft FK).
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Username of the person who last modified this account party.
     */
    std::string recorded_by;

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
