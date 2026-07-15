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
#ifndef ORES_REFDATA_API_DOMAIN_INSTRUMENT_CODE_HPP
#define ORES_REFDATA_API_DOMAIN_INSTRUMENT_CODE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Full product/trade-type catalogue, keyed by a short idiomatic code, cross-referenced to
 * ORE's oreTradeType.
 *
 * General-purpose product catalogue: one row per instrument/product type
 * the system knows about, covering every trade type in ORE's own
 * authoritative oreTradeType enumeration
 * (external/ore/xsd/instruments.xsd), plus one ORE Studio-specific
 * addition, DEPO (ORE has no distinct money-market-deposit trade type
 * — it models a deposit as a single-period Swap — so DEPO is added
 * purely to give the IR Curve Template a distinct short-end label; its
 * pricing still derives from the same par-rate formula a single-period
 * swap would use). The code column is deliberately not ORE's own
 * oreTradeType spelling: traders need something they can actually type
 * quickly, and ORE's names (FxDoubleBarrierOption,
 * EquityStrikeResettableOption) don't serve that. code is instead a
 * short, FIX-inspired mnemonic (FRA, IRS, CDS, FXBAR, EQVS...),
 * unique and typically under 10 characters. The literal ORE
 * oreTradeType string, where one exists, is preserved separately in
 * ore_trade_type (nullable — null only for DEPO) for future ORE
 * trade-file interop; it is not itself validated or FK'd anywhere, only
 * carried as a reference. Each row is tagged with the asset_class_code
 * it belongs to. Not scoped to any single consumer: the IR Curve
 * Template (which consumes the DEPO/FRA/IRS rates entries for its
 * tenor roles) is the first consumer, but this catalogue exists
 * independently for any future feature needing to classify or enumerate
 * the instrument types the system supports. Managed by the system
 * tenant, like other shared code tables.
 */
struct instrument_code final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique short, FIX-inspired mnemonic code for the instrument type -- not ORE's own
     * oreTradeType spelling (see ore_trade_type below for that). Chosen so a trader can type it
     * quickly.
     *
     * Examples: 'IRS', 'FRA', 'CDS', 'FXBAR', 'EQVS'.
     */
    std::string code;

    /**
     * @brief Human-readable full name for the instrument code (e.g. "Forward Rate Agreement" for
     * FRA).
     */
    std::string name;

    /**
     * @brief Detailed description of the instrument code.
     */
    std::string description;

    /**
     * @brief Asset class this instrument code belongs to (references asset_class_code.code; column
     * named asset_class, not asset_class_code, to avoid a C++ name lookup collision between the
     * member and the asset_class_code domain type itself in generated Qt code).
     */
    std::string asset_class;

    /**
     * @brief Literal ORE oreTradeType string (external/ore/xsd/instruments.xsd) this code
     * corresponds to, e.g. 'ForwardRateAgreement' for FRA. Null only for DEPO, the one ORE
     * Studio-specific addition with no ORE trade type of its own. Preserved for future ORE
     * trade-file interop; not itself validated or referenced by any FK.
     */
    std::optional<std::string> ore_trade_type;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this instrument code.
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

/**
 * @brief Dispatch-key identifier for instrument_code, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const instrument_code&) {
    return "ores.refdata.instrument_code";
}

}

#endif
