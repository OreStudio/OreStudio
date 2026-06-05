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
#ifndef ORES_TRADING_DOMAIN_CREDIT_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_CREDIT_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <optional>
#include <string>

namespace ores::trading::domain {

/**
 * @brief Economic terms of a credit instrument.
 *
 * Extracted as a plain nested sub-struct to keep each rfl::Literal below the
 * MSVC C1202 threshold. See doc/investigations/msvc_c1202_rfl_complexity.org.
 */
struct credit_terms final {
    /**
     * @brief Name or identifier of the reference entity.
     */
    std::string reference_entity;

    /**
     * @brief ISO 4217 currency code (e.g. USD).
     */
    std::string currency;

    /**
     * @brief Notional amount of the credit instrument.
     */
    double notional = 0.0;

    /**
     * @brief Credit spread in basis points (e.g. 100.0 for 100 bps).
     */
    double spread = 0.0;

    /**
     * @brief Recovery rate as a decimal (e.g. 0.4 for 40%).
     */
    double recovery_rate = 0.0;

    /**
     * @brief Optional seniority (e.g. "Senior", "Subordinated"). Empty if not
     * applicable.
     */
    std::string seniority;

    /**
     * @brief Optional restructuring clause (e.g. "MM", "MR", "CR", "XR").
     * Empty if not applicable.
     */
    std::string restructuring;

    /**
     * @brief Reference asset code for CreditLinkedSwap. Empty otherwise.
     */
    std::string linked_asset_code;
};

/**
 * @brief Schedule and date conventions of a credit instrument.
 *
 * Extracted as a plain nested sub-struct to keep each rfl::Literal below the
 * MSVC C1202 threshold. See doc/investigations/msvc_c1202_rfl_complexity.org.
 */
struct credit_schedule final {
    /**
     * @brief Tenor of the instrument (e.g. "5Y", "3Y").
     */
    std::string tenor;

    /**
     * @brief Start date (ISO 8601 date string, e.g. 2026-01-15).
     */
    std::string start_date;

    /**
     * @brief Maturity date (ISO 8601 date string, e.g. 2031-01-15).
     */
    std::string maturity_date;

    /**
     * @brief Day count convention code (e.g. Actual365Fixed, Thirty360).
     */
    std::string day_count_code;

    /**
     * @brief Payment frequency code (e.g. Quarterly, SemiAnnual).
     */
    std::string payment_frequency_code;
};

/**
 * @brief Index identification for CDSIndex-style credit instruments.
 *
 * Extracted as a plain nested sub-struct to keep each rfl::Literal below the
 * MSVC C1202 threshold. See doc/investigations/msvc_c1202_rfl_complexity.org.
 */
struct credit_index final {
    /**
     * @brief Optional index name for CDSIndex trades (e.g. "CDX.NA.IG").
     * Empty for non-index products.
     */
    std::string index_name;

    /**
     * @brief Optional index series number for CDSIndex trades. Zero means not
     * specified.
     */
    int index_series = 0;
};

/**
 * @brief Optionality of CreditDefaultSwapOption-style credit instruments.
 *
 * Extracted as a plain nested sub-struct to keep each rfl::Literal below the
 * MSVC C1202 threshold. See doc/investigations/msvc_c1202_rfl_complexity.org.
 */
struct credit_option final {
    /**
     * @brief Option type: "Call" or "Put" — CreditDefaultSwapOption.
     * Empty otherwise.
     */
    std::string option_type;

    /**
     * @brief Option expiry date (ISO 8601) for CDS options. Empty otherwise.
     */
    std::string option_expiry_date;

    /**
     * @brief Option strike spread in bps for CDS options. Null when not set.
     */
    std::optional<double> option_strike;
};

/**
 * @brief Tranche attachment/detachment for CBO and SyntheticCDO instruments.
 *
 * Extracted as a plain nested sub-struct to keep each rfl::Literal below the
 * MSVC C1202 threshold. See doc/investigations/msvc_c1202_rfl_complexity.org.
 */
struct credit_tranche final {
    /**
     * @brief CBO tranche attachment point as decimal. Null when not set.
     */
    std::optional<double> tranche_attachment;

    /**
     * @brief CBO tranche detachment point as decimal. Null when not set.
     */
    std::optional<double> tranche_detachment;
};

/**
 * @brief Credit instrument economics for CreditDefaultSwap, CDSIndex,
 * SyntheticCDO, CreditDefaultSwapOption, IndexCreditDefaultSwapOption,
 * CreditLinkedSwap, and CBO trades.
 *
 * Discriminated by trade_type_code. Optional fields are empty/zero for
 * non-applicable product types.
 */
struct credit_instrument final {
    instrument_identity identity;
    credit_terms terms;
    credit_schedule schedule;
    credit_index index;
    credit_option option;
    credit_tranche tranche;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
};

}

#endif
