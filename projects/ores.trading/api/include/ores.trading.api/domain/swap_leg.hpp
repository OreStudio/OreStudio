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
#include <chrono>
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
 * leg type does not state are null: floating_index_code is null for a fixed
 * leg, for instance.
 *
 * The leg's economics are rows beside it, not columns on it: its notionals are
 * swap_leg_amount rows and its rates or spreads are swap_leg_rate rows,
 * because both are lists the document may state one entry at a time or one per
 * step. A single column cannot hold a schedule, and the two arms of a stepping
 * leg were flattened into one until these children landed.
 *
 * The row is keyed by the trade and the leg's ordinal, (trade_id, leg_number),
 * and names its parent through trade_id. Nothing mints a surrogate: the import
 * reads the parent instrument it was given, and the trade plus the ordinal
 * already identify the row. The instrument is keyed by its trade, so all nine
 * rates families name the one parent the trades table holds rather than a table
 * per family.
 *
 * It binds :profile: trading-instrument, like the nine instrument sub-types
 * whose legs it holds. Two table features justify the bind: the table is
 * tenant-scoped through tenant_id and the tenant isolation policy, its insert trigger stamps
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
     * @brief True when the leg's payer is the trade's counterparty rather than the party.
     *
     * ORE's legData states Payer as a required member, so a leg without it has lost which side
     * pays. It is nullable because a leg a caller writes directly may not state it.
     */
    std::optional<bool> payer;

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
     * @brief ISO 4217 currency code of the leg (e.g. USD).
     *
     * Soft FK to ores_refdata_currencies_tbl: ISO 4217 currency codes belong to ores.refdata, so
     * the dependency is recorded rather than copied. PR 4 tightens the soft reference into a real
     * foreign key.
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
