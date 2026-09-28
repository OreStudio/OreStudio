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
#ifndef ORES_TRADING_API_DOMAIN_FX_ACCUMULATOR_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_FX_ACCUMULATOR_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief FX Accumulator instrument.
 *
 * Routes ORE product type: FxAccumulator. knock_out_barrier captures
 * the primary UpAndOut barrier. Multiple barriers and complex fixing
 * schedules are a Phase 2 coverage gap.
 */
struct fx_accumulator_instrument final {
    instrument_identity identity;

    /**
     * @brief Settlement currency (domestic side).
     *
     * Soft FK to ores_refdata_currencies_tbl: ISO 4217 currency codes belong to ores.refdata, so
     * the dependency is recorded rather than copied. PR 4 tightens the soft reference into a real
     * foreign key.
     */
    std::string currency;

    /**
     * @brief Per-fixing notional amount. Must be positive.
     */
    ores::utility::decimal::decimal fixing_amount;

    /**
     * @brief Fixed strike rate. Must be positive.
     */
    double strike = 0.0;

    /**
     * @brief FX pair or index identifier (e.g. TR20H-EUR-JPY).
     */
    std::string underlying_code;

    /**
     * @brief Position direction: Long or Short.
     *
     * Soft FK to ores_trading_long_short_types_tbl: the values are the closed ORE longShort set
     * (Long, Short), which the SQL schema already states as a check. PR 4 tightens the soft
     * reference into a real foreign key.
     */
    std::string long_short;

    /**
     * @brief Accumulation start date (ISO 8601 date string).
     */
    std::chrono::year_month_day start_date;

    /**
     * @brief Primary UpAndOut knock-out barrier level. Absent when no barrier.
     */
    std::optional<double> knock_out_barrier;

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
    friend bool operator==(const fx_accumulator_instrument&,
                           const fx_accumulator_instrument&) = default;
};

/**
 * @brief Dispatch-key identifier for fx_accumulator_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_accumulator_instrument&) {
    return "ores.trading.fx_accumulator_instrument";
}

}

#endif
