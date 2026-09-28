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
#ifndef ORES_TRADING_API_DOMAIN_SWAP_LEG_HPP
#define ORES_TRADING_API_DOMAIN_SWAP_LEG_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/swap_leg_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One leg of a rates instrument.
 *
 * The shared legs table of the nine rates instrument families. Each row is one
 * leg of an FRA, vanilla swap, cap/floor, swaption, balance-guaranteed swap,
 * callable swap, knock-out swap, inflation swap or RPA instrument. A plain
 * interest rate swap has two rows, one fixed and one floating; a cross-currency
 * swap has two rows with different currencies.
 *
 * leg_type_code is the discriminator and the leg type is data, not a table, so
 * one model covers the shared table (story 0DC1BAC7, decision D10). The fields a
 * leg type does not state are null: fixed_rate is null for a floating leg and
 * floating_index_code is null for a fixed leg.
 *
 * The row keeps its own id surrogate and names the parent instrument through
 * instrument_id, which is a soft foreign key to the instrument family rather
 * than to one table.
 *
 * It binds :profile: trading-instrument, like the nine instrument sub-types
 * whose legs it holds. Three table features justify the bind: the table is
 * tenant-scoped through tenant_id and the tenant isolation policy, it is
 * workspace-scoped through workspace_id, and its insert trigger stamps
 * party_id from the session variable app.current_party_id rather than taking
 * it from the client. The bind leaves the table with no UI surface -- the
 * per-instrument forms were hand-crafted in the removed desktop client and
 * consumed the generated messaging protocol. The identity and audit field
 * groups and the generator facet are the model's own flags, stated under C++
 * below, not profile assignments.
 *
 * The table is bi-temporal and audited, so the model takes the ordinary audited
 * shape and needs no shape flag.
 */
struct swap_leg final {
    swap_leg_identity identity;

    /**
     * @brief Leg type code (soft FK to ores_refdata_leg_types_tbl).
     *
     * Routes the leg economics: Fixed for a fixed leg, Floating for a floating leg, and the CMS,
     * CPI, OIS and other codes the rates families state.
     */
    std::string leg_type_code;

    /**
     * @brief Day count fraction code (soft FK to ores_refdata_day_count_fraction_types_tbl).
     */
    std::string day_count_fraction_code;

    /**
     * @brief Business day convention code (soft FK to
     * ores_refdata_business_day_convention_types_tbl).
     */
    std::string business_day_convention_code;

    /**
     * @brief Payment frequency code (soft FK to ores_refdata_payment_frequencies_tbl).
     */
    std::string payment_frequency_code;

    /**
     * @brief Floating index code of a floating leg (soft FK to
     * ores_refdata_floating_index_types_tbl).
     *
     * Empty for a fixed leg; validated only when stated.
     */
    std::string floating_index_code;

    /**
     * @brief Fixed rate of a fixed leg, as a decimal (e.g. 0.05 for 5%).
     *
     * Null for a floating leg.
     */
    double fixed_rate = 0.0;

    /**
     * @brief Spread over the floating index, as a decimal.
     *
     * Null for a fixed leg.
     */
    double spread = 0.0;

    /**
     * @brief Notional amount of the leg. Must be positive.
     */
    ores::utility::decimal::decimal notional;

    /**
     * @brief ISO 4217 currency code of the leg (e.g. USD).
     */
    std::string currency;

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
    friend bool operator==(const swap_leg&, const swap_leg&) = default;
};

/**
 * @brief Dispatch-key identifier for swap_leg, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const swap_leg&) {
    return "ores.trading.swap_leg";
}

}

#endif
