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
#ifndef ORES_TRADING_API_DOMAIN_FX_ASIAN_FORWARD_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_FX_ASIAN_FORWARD_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief FX Asian Forward instrument.
 *
 * Routes ORE product types: FxAverageForward, FxTaRF. fx_index
 * captures Underlying.Name for the fixing source. The
 * reference_currency/reference_notional/settlement_currency/
 * settlement_notional/payment_date/long_short fields are
 * FxAverageForward-specific; currency/fixing_amount/target_amount/
 * strike are FxTaRF-specific (empty/absent for FxAverageForward).
 * Complex observation schedules and range bounds are not modelled in
 * Phase 2.
 */
struct fx_asian_forward_instrument final {
    instrument_identity identity;

    /**
     * @brief Fixing source index (Underlying.Name from ORE XML).
     */
    std::string fx_index;

    /**
     * @brief Currency the average is computed on (FxAverageForward).
     */
    std::string reference_currency;

    /**
     * @brief Notional in reference_currency (FxAverageForward).
     */
    std::optional<ores::utility::decimal::decimal> reference_notional;

    /**
     * @brief Currency of the settlement payment (FxAverageForward).
     */
    std::string settlement_currency;

    /**
     * @brief Notional in settlement_currency (FxAverageForward).
     */
    std::optional<ores::utility::decimal::decimal> settlement_notional;

    /**
     * @brief Settlement payment date (ISO 8601 date string).
     */
    std::optional<std::chrono::year_month_day> payment_date;

    /**
     * @brief Position direction: Long or Short. Hardcoded to Long by the mapper.
     */
    std::string long_short;

    /**
     * @brief Domestic currency (FxTaRF-specific). Empty for FxAverageForward.
     *
     * Soft FK to ores_refdata_currencies_tbl: ISO 4217 currency codes belong to ores.refdata, so
     * the dependency is recorded rather than copied. PR 4 tightens the soft reference into a real
     * foreign key.
     */
    std::string currency;

    /**
     * @brief Per-fixing target amount (FxTaRF-specific). Absent for FxAverageForward.
     */
    std::optional<ores::utility::decimal::decimal> fixing_amount;

    /**
     * @brief Total target amount (FxTaRF-specific). Absent for FxAverageForward.
     */
    std::optional<ores::utility::decimal::decimal> target_amount;

    /**
     * @brief Target strike level (FxTaRF-specific). Absent for FxAverageForward.
     */
    std::optional<double> strike;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

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
    friend bool operator==(const fx_asian_forward_instrument&,
                           const fx_asian_forward_instrument&) = default;
};

/**
 * @brief Dispatch-key identifier for fx_asian_forward_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_asian_forward_instrument&) {
    return "ores.trading.fx_asian_forward_instrument";
}

}

#endif
