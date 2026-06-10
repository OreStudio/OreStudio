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
#ifndef ORES_TRADING_API_DOMAIN_RPA_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_RPA_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <chrono>
#include <string>

namespace ores::trading::domain {

/**
 * @brief Risk Participation Agreement (RPA) instrument.
 *
 * Represents a Risk Participation Agreement where one bank (protection
 * buyer) pays a fee to another (protection seller) to assume a portion of
 * credit risk on a reference counterparty.
 */
struct rpa_instrument final {
    instrument_identity identity;

    /**
     * @brief Agreement effective start date.
     *
     * ISO 8601 date string (YYYY-MM-DD).
     */
    std::string start_date;

    /**
     * @brief Agreement maturity date.
     *
     * Must be after start_date.
     */
    std::string maturity_date;

    /**
     * @brief Identifier of the reference counterparty whose credit risk is shared.
     *
     * e.g., LEI code or name of the reference entity.
     */
    std::string reference_counterparty;

    /**
     * @brief Participation rate (0, 1] representing the fraction of risk transferred.
     *
     * Expressed as a decimal fraction (e.g., 0.5 for 50%).
     */
    double participation_rate = 0.0;

    /**
     * @brief Optional protection fee paid by the buyer.
     *
     * Expressed as a decimal fraction. Must be non-negative if set.
     */
    std::optional<double> protection_fee = std::nullopt;

    /**
     * @brief Optional free-text description.
     *
     * Human-readable notes about this instrument.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
};

}

#endif
