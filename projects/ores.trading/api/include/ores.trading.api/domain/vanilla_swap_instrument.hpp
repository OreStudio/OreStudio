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
#ifndef ORES_TRADING_API_DOMAIN_VANILLA_SWAP_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_VANILLA_SWAP_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Vanilla interest rate swap or cross-currency swap instrument.
 *
 * Represents a plain vanilla interest rate swap or cross-currency swap
 * instrument linking two legs via the swap_legs table.
 */
struct vanilla_swap_instrument final {
    instrument_identity identity;

    /**
     * @brief Swap effective/start date.
     *
     * ISO 8601 date string (YYYY-MM-DD).
     */
    std::string start_date;

    /**
     * @brief Swap maturity/termination date.
     *
     * Must be after start_date.
     */
    std::string maturity_date;

    /**
     * @brief Optional settlement lag in business days.
     *
     * Number of business days between execution and effective date.
     */
    std::optional<int> settlement_lag;

    /**
     * @brief Optional netting set identifier.
     *
     * Groups instruments for netting purposes.
     */
    std::string netting_set_id;

    /**
     * @brief Optional free-text description.
     *
     * Human-readable notes about this instrument.
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
    friend bool operator==(const vanilla_swap_instrument&,
                           const vanilla_swap_instrument&) = default;
};

/**
 * @brief Dispatch-key identifier for vanilla_swap_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const vanilla_swap_instrument&) {
    return "ores.trading.vanilla_swap_instrument";
}

}

#endif
