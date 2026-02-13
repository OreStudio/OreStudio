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
#ifndef ORES_REFDATA_DOMAIN_COUNTERPARTY_HPP
#define ORES_REFDATA_DOMAIN_COUNTERPARTY_HPP

#include <chrono>
#include <string>
#include <optional>
#include <boost/uuid/uuid.hpp>

namespace ores::refdata::domain {

/**
 * @brief An external trading partner participating in financial transactions.
 *
 * External trading partners and counterparties that participate in financial
 * transactions with the organisation. Counterparties form a hierarchy through
 * parent_counterparty_id for group structures.
 */
struct counterparty final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief UUID uniquely identifying this counterparty.
     *
     * Surrogate key for the counterparty record.
     */
    boost::uuids::uuid id;

    /**
     * @brief Full legal name of the counterparty.
     *
     * The official registered name of the external entity.
     */
    std::string full_name;

    /**
     * @brief Short code for quick reference.
     *
     * A brief mnemonic code used in trading systems.
     */
    std::string short_code;

    /**
     * @brief ASCII transliteration of the entity name.
     *
     * Populated from GLEIF data for entities with non-Latin names
     * (CJK, Cyrillic, Arabic, etc.). Null for entities already in Latin script.
     */
    std::optional<std::string> transliterated_name;

    /**
     * @brief Classification of this counterparty.
     *
     * References the party_type lookup table.
     */
    std::string party_type;

    /**
     * @brief Parent counterparty for hierarchy.
     *
     * References the parent counterparty record for group structures. Null for root counterparties.
     */
    std::optional<boost::uuids::uuid> parent_counterparty_id;

    /**
     * @brief Business center location code.
     *
     * FpML business center code indicating primary location.
     */
    std::string business_center_code;

    /**
     * @brief Current lifecycle status.
     *
     * References the party_status lookup table.
     */
    std::string status;

    /**
     * @brief Username of the person who last modified this counterparty.
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
