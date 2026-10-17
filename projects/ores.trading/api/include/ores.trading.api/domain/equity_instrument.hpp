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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>

namespace ores::trading::domain {

/**
 * @brief The equity family header: one row per equity trade, keyed by trade_id, owning the family's
 * facts and children.
 *
 * One row per equity trade, the family's own header. It holds only what every
 * equity product states the same way: the trade it belongs to, the trade type
 * code, the party, the activity that wrote the version, the instrument's start
 * and maturity dates and its free text. It holds no product economics.
 *
 * No economic field is common to all the routed codes. The underlying is one
 * name for most products and a basket for the worst-of swap, so it stays on the
 * facts and in equity_basket_constituents rather than on the header. A header
 * column has to hold for every code, and the underlying does not.
 *
 * The products' own fields stay in their fact tables, keyed by the same trade
 * and joined to this row by the trade id. A fact table carries no party_id and
 * no trade_type_code: the header is the family's party boundary and its
 * routing boundary.
 *
 * The dates are the family's common dates, the one set of names for the start
 * and the maturity of every equity product, so the blotter reads one column for
 * each across the whole family.
 */
struct equity_instrument final {
    instrument_identity identity;

    /**
     * @brief The instrument's start date, one of the family's common dates.
     *
     * No equity product states both dates, so a code maps the one it has and the blotter's
     * common-date view reads the other as null.
     *
     * Null when the product states none.
     */
    std::optional<std::chrono::year_month_day> start_date;

    /**
     * @brief The instrument's maturity date, the family's other common date.
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
    friend bool operator==(const equity_instrument&, const equity_instrument&) = default;
};

/**
 * @brief The members of equity_instrument the customer agreed to.
 *
 * Declared by the model's :economic: property on each column.
 */
inline constexpr std::string_view equity_instrument_economic_fields[] = {
    "start_date",
    "maturity_date",
};

/**
 * @brief Names the members of equity_instrument that are economic.
 *
 * The economic digest reads this at compile time and decides nothing by a
 * field's name. A type without this overload cannot be digested.
 */
[[nodiscard]] constexpr std::span<const std::string_view>
economic_fields_of(std::type_identity<equity_instrument>) {
    return equity_instrument_economic_fields;
}

/**
 * @brief Dispatch-key identifier for equity_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const equity_instrument&) {
    return "ores.trading.equity_instrument";
}

}

#endif
