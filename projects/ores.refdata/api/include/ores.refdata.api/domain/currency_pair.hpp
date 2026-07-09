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
#ifndef ORES_REFDATA_API_DOMAIN_CURRENCY_PAIR_HPP
#define ORES_REFDATA_API_DOMAIN_CURRENCY_PAIR_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>

namespace ores::refdata::domain {

/**
 * @brief Currency pair identity (base/quote legs, deliverability, classification).
 *
 * Currency pair identity: base/quote legs, deliverability, classification,
 * and fixing source. Conventions (pip factor, tick size, calendars,
 * business day convention) live 1:1 in
 * [[id:1B88215B-1FE0-4CAF-B6AB-53F471963CA6][ores.refdata.currency_pair_convention]], matching the
 * codebase's existing *_convention entity family. spot_days, calendars, and G11 membership are
 * *derived* at read time from the two legs, not stored here — see
 * [[id:04A121FA-00D6-43EB-9B21-04EDC1FA493D][Currency pair support in reference data]] for the full
 * design rationale.
 */
struct currency_pair final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Canonical base/quote pair code, e.g. "EUR/USD", always stored in canonical
     * base-currency-precedence order.
     */
    std::string pair_code;

    /**
     * @brief Base (first) leg of the pair. Soft FK to currency.iso_code.
     */
    std::string base_currency;

    /**
     * @brief Quote (second) leg of the pair. Soft FK to currency.iso_code.
     */
    std::string quote_currency;

    /**
     * @brief Liquidity classification; soft FK to ores_refdata_currency_pair_classifications_tbl
     * (e.g. "major", "minor", "exotic"). Free-text pending a working dynamic-combo mechanism for
     * aux-type reference data (tracked separately; out of scope for this entity — see
     * [[id:04A121FA-00D6-43EB-9B21-04EDC1FA493D][Currency pair support in reference data]]).
     */
    std::string classification;

    /**
     * @brief Username of the person who last modified this currency pair.
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
