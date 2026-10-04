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
#ifndef ORES_TRADING_API_DOMAIN_CREDIT_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_CREDIT_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Credit instrument.
 *
 * Represents the credit product types ORE states. trade_type_code
 * discriminates the exact product, and the optional field blocks are null
 * when the sub-type does not state them: the index block for CDSIndex
 * products, the option block for CDS option products, the tranche block for
 * CBO and SyntheticCDO products, and the linked-asset code for
 * CreditLinkedSwap.
 *
 * The table is a flat instrument sub-type, so it binds
 * :profile: trading-instrument. Two table features justify that binding:
 * the table is tenant-scoped through tenant_id and the tenant isolation
 * policy, its insert
 * trigger stamps party_id from the session variable app.current_party_id
 * rather than taking it from the client. The profile also fixes the identity
 * and audit field groups, the batch read and the generator facet, and leaves
 * the table with no UI surface -- the per-instrument forms were hand-crafted
 * in the removed desktop client and consumed the generated messaging
 * protocol.
 */
struct credit_instrument final {
    instrument_identity identity;

    /**
     * @brief Name or identifier of the reference entity.
     */
    std::string reference_entity;

    /**
     * @brief ISO 4217 currency code (e.g. USD).
     *
     * Soft FK to ores_refdata_currencies_tbl: ISO 4217 currency codes belong to ores.refdata, so
     * the dependency is recorded rather than copied. PR 4 tightens the soft reference into a real
     * foreign key.
     */
    std::string currency;

    /**
     * @brief Notional amount of the credit instrument. Must be positive.
     */
    ores::utility::decimal::decimal notional;

    /**
     * @brief Credit spread in basis points (e.g. 100.0 for 100 bps).
     */
    double spread = 0.0;

    /**
     * @brief Recovery rate as a decimal (e.g. 0.4 for 40%).
     */
    double recovery_rate = 0.0;

    /**
     * @brief Tenor of the instrument (e.g. "5Y", "3Y").
     */
    std::string tenor;

    /**
     * @brief Start date (ISO 8601 date string, e.g. 2026-01-15).
     */
    std::chrono::year_month_day start_date;

    /**
     * @brief Maturity date (ISO 8601 date string, e.g. 2031-01-15).
     */
    std::chrono::year_month_day maturity_date;

    /**
     * @brief Day count convention code (e.g. Actual365Fixed, Thirty360).
     *
     * Soft FK to ores_refdata_day_count_fraction_types_tbl: day count conventions belong to
     * ores.refdata, so the dependency is recorded rather than copied. CreditInstrument stores ORE
     * dayCounter spellings (Actual365Fixed, Thirty360) while the reference table keys on ISDA short
     * codes (A365F, 30/360), so PR 4 needs a conversion helper in the shape of
     * payment_frequency_conversion.hpp. PR 4 tightens the soft reference into a real foreign key.
     */
    std::string day_count_fraction_code;

    /**
     * @brief Payment frequency code (e.g. Quarterly, SemiAnnual).
     *
     * Soft FK to ores_refdata_payment_frequencies_tbl: payment frequencies belong to ores.refdata,
     * so the dependency is recorded rather than copied. PR 4 tightens the soft reference into a
     * real foreign key.
     */
    std::string payment_frequency_code;

    /**
     * @brief Optional index name for CDSIndex trades (e.g. "CDX.NA.IG").
     *
     * Empty for non-index products.
     */
    std::string index_name;

    /**
     * @brief Optional index series number for CDSIndex trades.
     *
     * Null when the trade states no series.
     */
    std::optional<int> index_series = std::nullopt;

    /**
     * @brief Optional seniority (e.g. "Senior", "Subordinated").
     *
     * Empty when not applicable.
     */
    std::string seniority;

    /**
     * @brief Optional restructuring clause (e.g. "MM", "MR", "CR", "XR").
     *
     * Empty when not applicable.
     */
    std::string restructuring;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    /**
     * @brief Call or Put for CreditDefaultSwapOption; empty otherwise.
     *
     * Soft FK to ores_trading_option_types_tbl: the values are the closed ORE optionType set (Call,
     * Put). PR 4 tightens the soft reference into a real foreign key.
     */
    std::string option_type;

    /**
     * @brief Option expiry date (ISO 8601) for CDS options. Empty otherwise.
     */
    std::optional<std::chrono::year_month_day> option_expiry_date;

    /**
     * @brief Option strike spread in bps for CDS options. Null when not set.
     */
    std::optional<double> option_strike;

    /**
     * @brief Reference asset code for CreditLinkedSwap. Empty otherwise.
     */
    std::string linked_asset_code;

    /**
     * @brief CBO tranche attachment point as decimal. Null when not set.
     */
    std::optional<double> tranche_attachment;

    /**
     * @brief CBO tranche detachment point as decimal. Null when not set.
     */
    std::optional<double> tranche_detachment;

    ores::dq::domain::audit_record audit;
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
    friend bool operator==(const credit_instrument&, const credit_instrument&) = default;
};

/**
 * @brief Dispatch-key identifier for credit_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const credit_instrument&) {
    return "ores.trading.credit_instrument";
}

}

#endif
