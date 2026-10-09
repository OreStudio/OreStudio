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
#ifndef ORES_TRADING_API_DOMAIN_RATE_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_RATE_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief The rates family header: one row per interest-rates trade, keyed by trade_id, owning the
 * family's facts, legs and children.
 *
 * One row per interest-rates trade, the family's own header. It holds what
 * every product in the family states the same way: the trade it belongs to,
 * the trade type code, the party, the activity that wrote the version, the
 * instrument's start and maturity dates and its description. It holds no
 * product economics.
 *
 * The header exists for two reasons. It is the family's *coverage*: the
 * trade_type_code check that the routed-instrument template generates from
 * ores.trading.trade_type_catalogue lists exactly the codes routed here, so
 * the family's codes sit in one table and no product table repeats the
 * identity columns. And it is the family's *owner*: the * Delete cascade
 * section below names every table the family writes, so the delete rule closes
 * the legs, leg children and facts with the header instead of leaving them
 * behind (finding R6 of the family's design).
 *
 * The products' own fields stay in their fact tables, keyed by the same trade
 * and joined to this row by the trade id, exactly as bond_instruments joins
 * its facts. A fact table carries no party_id and no trade_type_code: the
 * header is the family's party boundary and its routing boundary, so a fact
 * row reaches its tenant and its code through the header (design decision D3).
 *
 * The dates are the family's *common dates*. Each product used to state them
 * under its own name — start_date, maturity_date, expiry_date,
 * end_date — so nothing could read one date column across the family
 * (finding R9). The two columns here are the one set of names; a product that
 * means something else by a date keeps that date in its own fact table.
 *
 * InflationSwap is a member until an inflation family is scaffolded. The
 * taxonomy already calls it an inflation product, and its columns move here
 * with the rest of the family's so that its rows are owned and routed like
 * every other rates row. It leaves with its own family's rework.
 */
struct rate_instrument final {
    instrument_identity identity;

    /**
     * @brief The instrument's start date.
     *
     * Null when the product states none: a swaption states the underlying swap's dates only when
     * the document carries them.
     *
     * ISO 8601 date string (YYYY-MM-DD).
     */
    std::optional<std::chrono::year_month_day> start_date;

    /**
     * @brief The instrument's maturity date. A forward rate agreement's end date is this column:
     * the family states one name for the date, and the product's own spelling is not carried.
     *
     * Null with start_date, and for the same reason.
     */
    std::optional<std::chrono::year_month_day> maturity_date;

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
    friend bool operator==(const rate_instrument&, const rate_instrument&) = default;
};

/**
 * @brief Dispatch-key identifier for rate_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const rate_instrument&) {
    return "ores.trading.rate_instrument";
}

}

#endif
