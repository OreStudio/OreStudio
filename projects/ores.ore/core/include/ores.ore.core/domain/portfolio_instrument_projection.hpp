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
#ifndef ORES_ORE_CORE_DOMAIN_PORTFOLIO_INSTRUMENT_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_PORTFOLIO_INSTRUMENT_PROJECTION_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/xml/record.hpp"
#include "ores.trading.api/domain/instrument_batch.hpp"
#include "ores.trading.api/domain/trade_instrument.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief The ORE name a trade's reference curve is written under.
 *
 * A bond names the curve it is discounted on by the market series' own URI, and
 * ORE resolves that name through the market configuration the run prices under.
 * The mapping belongs to the run, not to the trade.
 */
using portfolio_reference_curves = std::function<std::string(const std::string&)>;

/**
 * @brief An exact decimal member as the double the trading domain stores.
 *
 * The table keeps the digits the document wrote, and the domain column is a
 * double. The conversion happens here, once, at the boundary.
 */
inline auto decimal_as_double(const xml::record_reader& r,
                              std::string_view name) -> std::optional<double> {
    if (const auto value = r.decimal(name))
        return value->to_double();
    return std::nullopt;
}

/// A double as the exact decimal the table writes.
inline auto decimal_from_double(double value) -> ores::utility::decimal::decimal {
    if (auto parsed = ores::utility::decimal::decimal::from_double(value))
        return *parsed;
    return ores::utility::decimal::decimal{};
}

/// States a decimal member from a double the domain holds.
inline void set_decimal_from_double(xml::record_writer& w, std::string_view name, double value) {
    w.set_decimal(name, decimal_from_double(value));
}

/**
 * @brief States one rule of a schedule, and hands the rule back.
 *
 * The schema requires a start date, a tenor and a convention, and a row that
 * holds no date states the element empty rather than dropping it. The
 * convention is the one every mapper in the tree falls back to.
 */
inline auto write_schedule_rule(xml::record_writer schedule,
                                std::string_view start_date,
                                std::string_view tenor) -> xml::record_writer {
    auto rule = schedule.append("Rules");
    rule.set_text("StartDate", start_date);
    rule.set_text("Tenor", tenor);
    rule.set_spelling("Convention", "MF", "business day convention");
    return rule;
}

/**
 * @brief One instrument family's two directions.
 *
 * The reading returns nothing when the trade type is not the family's, and the
 * writing returns false, so the portfolio projection asks each family in turn
 * and the order is the one the old dispatcher used.
 */
struct instrument_projection final {
    std::string_view family;
    std::optional<trading::domain::trade_instrument> (*read)(xml::record_reader trade,
                                                             std::string_view trade_type);
    bool (*write)(xml::record_writer& trade,
                  std::string_view trade_type,
                  const trading::domain::trade_instrument& instrument,
                  const portfolio_reference_curves& reference_curves);
};

/// The families the projection knows, in the order the old dispatcher tried.
ORES_ORE_CORE_EXPORT auto instrument_projections() -> const std::vector<instrument_projection>&;

/// The scripted family, whose products carry their parameters as JSON.
ORES_ORE_CORE_EXPORT auto read_scripted_instrument(xml::record_reader trade,
                                                   std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument>;

ORES_ORE_CORE_EXPORT auto
write_scripted_instrument(xml::record_writer& trade,
                          std::string_view trade_type,
                          const trading::domain::trade_instrument& instrument,
                          const portfolio_reference_curves& reference_curves) -> bool;

/**
 * @brief The rates family: a swap and the products built on one.
 *
 * The family states one header row, the product's own fact row and the legs
 * and shared children its product needs. Both directions dispatch on the ORE
 * trade type, because a product the schema states as its own element is a
 * different product rather than another spelling of one.
 */
ORES_ORE_CORE_EXPORT auto read_rate_instrument(xml::record_reader trade,
                                               std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument>;

ORES_ORE_CORE_EXPORT auto
write_rate_instrument(xml::record_writer& trade,
                      std::string_view trade_type,
                      const trading::domain::trade_instrument& instrument,
                      const portfolio_reference_curves& reference_curves) -> bool;

/**
 * @brief The bond family: an issue row shared by the products built on it.
 *
 * The issue row carries the bond terms and its own children, the instrument
 * header carries the notional, and each product adds the fact row and the legs
 * it states. The writing names the run's curve for the issue's market series,
 * which is the mapping the market configuration owns rather than the trade.
 */
ORES_ORE_CORE_EXPORT auto read_bond_instrument(xml::record_reader trade,
                                               std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument>;

ORES_ORE_CORE_EXPORT auto
write_bond_instrument(xml::record_writer& trade,
                      std::string_view trade_type,
                      const trading::domain::trade_instrument& instrument,
                      const portfolio_reference_curves& reference_curves) -> bool;

/**
 * @brief The credit family: a routed header, one fact row per product and legs
 * grouped by the payment they state.
 *
 * An index swap, an index option and a synthetic CDO state a basket of names
 * beside the instrument, and a risk participation agreement states its own legs
 * as the rows of two groups. A product's legs are keyed by the trade, so the
 * group is the column that tells one product's legs from another's.
 */
ORES_ORE_CORE_EXPORT auto read_credit_instrument(xml::record_reader trade,
                                                 std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument>;

ORES_ORE_CORE_EXPORT auto
write_credit_instrument(xml::record_writer& trade,
                        std::string_view trade_type,
                        const trading::domain::trade_instrument& instrument,
                        const portfolio_reference_curves& reference_curves) -> bool;

/**
 * @brief The equity family: one product row, plus the entries a position owns.
 *
 * Every equity product is one row of its own table, and the trade type states
 * which of them the document holds. A position states its entries as child
 * rows, which travel in the carrier beside the row rather than inside it, and
 * an accumulator keeps its range bounds as JSON text because the table has no
 * child table for them.
 */
ORES_ORE_CORE_EXPORT auto read_equity_instrument(xml::record_reader trade,
                                                 std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument>;

ORES_ORE_CORE_EXPORT auto
write_equity_instrument(xml::record_writer& trade,
                        std::string_view trade_type,
                        const trading::domain::trade_instrument& instrument,
                        const portfolio_reference_curves& reference_curves) -> bool;

/**
 * @brief The commodity family: one product row, plus the entries a basket owns.
 *
 * Every commodity product is one row of its own table, and the trade type
 * states which of them the document holds. A swap and a swaption state their
 * legs in the document but keep only the terms the first commodity floating leg
 * and, for a swaption, the fixed leg's first price state. A basket states its
 * members as child rows, which travel in the carrier beside the row rather than
 * inside it.
 */
ORES_ORE_CORE_EXPORT auto read_commodity_instrument(xml::record_reader trade,
                                                    std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument>;

ORES_ORE_CORE_EXPORT auto
write_commodity_instrument(xml::record_writer& trade,
                           std::string_view trade_type,
                           const trading::domain::trade_instrument& instrument,
                           const portfolio_reference_curves& reference_curves) -> bool;

/**
 * @brief The FX family: one carrier holding a row per product, and no header.
 *
 * The carrier states a product by the array its row sits in, so the reading
 * pushes the row the trade type names and the writing takes the row the carrier
 * holds and asks the trade type which element carries it. An accumulator states
 * its range bounds beside its own row; no other product states a child.
 */
ORES_ORE_CORE_EXPORT auto read_fx_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument>;

ORES_ORE_CORE_EXPORT auto
write_fx_instrument(xml::record_writer& trade,
                    std::string_view trade_type,
                    const trading::domain::trade_instrument& instrument,
                    const portfolio_reference_curves& reference_curves) -> bool;

/**
 * @brief The composite family: a basket of constituent trades.
 *
 * A composite trade states its constituents as one row per component, and each
 * row is either a whole trade or its nested-only variant. The table keeps the
 * two kinds as separate indexed members, so the reading walks one arena and
 * then the other, and a document never mixes the kinds under one element. The
 * writing states the basket name and the currency the schema requires, because
 * the old mapper rebuilt no constituent.
 */
ORES_ORE_CORE_EXPORT auto read_composite_instrument(xml::record_reader trade,
                                                    std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument>;

ORES_ORE_CORE_EXPORT auto
write_composite_instrument(xml::record_writer& trade,
                           std::string_view trade_type,
                           const trading::domain::trade_instrument& instrument,
                           const portfolio_reference_curves& reference_curves) -> bool;

}

#endif
