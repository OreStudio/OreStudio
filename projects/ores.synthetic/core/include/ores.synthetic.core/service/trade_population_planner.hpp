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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_TRADE_POPULATION_PLANNER_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_TRADE_POPULATION_PLANNER_HPP

#include "ores.synthetic.api/domain/trade_population_plan.hpp"
#include "ores.synthetic.core/export.hpp"
#include <cstdint>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Thrown when a scope cannot state a population: a band whose ends do
 * not order, a party with no book or no counterparty, a family the generator
 * does not cover.
 *
 * A population that invents a reference is worse than one that fails, so every
 * unresolvable input is this exception rather than a default. The message
 * names the input, because the caller's next move is to fix the config row.
 */
class ORES_SYNTHETIC_CORE_EXPORT trade_population_error : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

/**
 * @brief Names the trade type code the bond family states for a plain bond.
 */
inline constexpr std::string_view bond_trade_type_code = "Bond";

/**
 * @brief Builds the population one asset class states.
 *
 * A planner is pure and deterministic: the answer is a function of the scope
 * alone, so the same scope states the same trades on every run, and a test can
 * compare two plans byte for byte without a database. Every identifier, date
 * and amount is derived and none is drawn from the clock.
 *
 * One planner serves one asset class, and the class it serves is the key the
 * registry answers it by.
 */
class ORES_SYNTHETIC_CORE_EXPORT trade_planner {
public:
    virtual ~trade_planner() = default;

    /**
     * @brief States the population the scope describes.
     *
     * @throws trade_population_error when the scope cannot state a
     * population.
     */
    [[nodiscard]] virtual domain::trade_population_plan
    plan(const domain::trade_generation_scope& scope) const = 0;

    /// The asset class this planner answers for.
    [[nodiscard]] virtual std::string_view asset_class_code() const = 0;
};

/**
 * @brief The planner for a bond population.
 *
 * The first implementation of the interface, and the one the sprint 28
 * population was written as a free function. It states a plain bond for every
 * slot.
 */
class ORES_SYNTHETIC_CORE_EXPORT bond_trade_planner final : public trade_planner {
public:
    [[nodiscard]] domain::trade_population_plan
    plan(const domain::trade_generation_scope& scope) const override;

    [[nodiscard]] std::string_view asset_class_code() const override;
};

/**
 * @brief The planner for a commodity population.
 *
 * The catalogue routes twenty-five commodity codes to one table, and the
 * ORE importer reads eight shapes out of it. The planner draws a shape per
 * slot and states the fields that shape needs, so every routed code books
 * and the eight the importer maps are the ones an export can carry back.
 *
 * It refuses when the tenant's catalogue names no commodity, because a
 * population that invented one would name something the engine cannot
 * resolve.
 */
class ORES_SYNTHETIC_CORE_EXPORT commodity_trade_planner final : public trade_planner {
public:
    [[nodiscard]] domain::trade_population_plan
    plan(const domain::trade_generation_scope& scope) const override;

    [[nodiscard]] std::string_view asset_class_code() const override;
};

/**
 * @brief The planner for an interest rate population.
 *
 * The routing table sends ten trade types to @c rate_instrument, and every one
 * of them reduces to a fixed leg against a floating leg: the type decides the
 * exercise features the specialised tables hold, and the legs are what all ten
 * share. The planner states those legs, drawing the index from the tenant's
 * benchmark catalogue and the conventions from the reference data's own codes.
 *
 * It refuses when the tenant holds no benchmark rate, because a floating leg
 * that named an index the store cannot resolve would book and then fail to
 * price.
 */
class ORES_SYNTHETIC_CORE_EXPORT rate_trade_planner final : public trade_planner {
public:
    [[nodiscard]] domain::trade_population_plan
    plan(const domain::trade_generation_scope& scope) const override;

    [[nodiscard]] std::string_view asset_class_code() const override;
};

/**
 * @brief The planner registered for an asset class.
 *
 * A class with no planner is refused by name rather than falling back to one,
 * because a population of the wrong family is worse than none: the reports
 * would compute over trades the config never asked for.
 *
 * @throws trade_population_error when no planner serves the class.
 */
ORES_SYNTHETIC_CORE_EXPORT const trade_planner& planner_for(const std::string& asset_class_code);

/// The asset classes that have a planner, in the order the registry holds them.
ORES_SYNTHETIC_CORE_EXPORT std::vector<std::string_view> planned_asset_classes();

/**
 * @brief The checks every family makes before it states a slot.
 *
 * A book, a counterparty and a trader every trade needs, and the notional and
 * tenor bands must order, so these are checked once for every planner rather
 * than repeated in each one.
 *
 * @throws trade_population_error naming the input that does not hold.
 */
ORES_SYNTHETIC_CORE_EXPORT void validate_scope(const domain::trade_generation_scope& scope);

/**
 * @brief The number of slots a scope states.
 *
 * The config states a count per book, so a scope over four books states four
 * times that count. It is a function of the scope alone.
 */
ORES_SYNTHETIC_CORE_EXPORT std::uint32_t slot_count(const domain::trade_generation_scope& scope);

/**
 * @brief States the envelope every family shares for one slot.
 *
 * The book, the counterparty, the trader, the identifiers, the trade date and
 * the netting set are the same for all of them, so a planner states what its
 * family adds and nothing else. A planner that computed these itself would be
 * a copy, and the copies drift.
 */
ORES_SYNTHETIC_CORE_EXPORT domain::trade_population_slot
common_slot(const domain::trade_generation_scope& scope, std::uint32_t slot);

/**
 * @brief Derives the id a trade slot carries.
 *
 * A name-based UUID over the scope's own identifiers and the slot, so a slot
 * is a name and not a draw. Re-running the same scope names the same slots and
 * an existing trade is found rather than a second one inserted.
 */
ORES_SYNTHETIC_CORE_EXPORT boost::uuids::uuid
trade_id_for(const domain::trade_generation_scope& scope, std::uint32_t slot);

/**
 * @brief Derives the id a bond issue carries from its security identifier.
 *
 * The issue is shared by every trade of the same security, so it is named by
 * the security and by nothing else: two trades of one ISIN name one issue.
 */
ORES_SYNTHETIC_CORE_EXPORT boost::uuids::uuid issue_id_for_security(const std::string& security_id);

/**
 * @brief The Internal identifier value a slot carries.
 *
 * Deterministic and human-readable, so a reviewer reading a trade's identifier
 * can name the slot it came from without a second lookup.
 */
ORES_SYNTHETIC_CORE_EXPORT std::string
internal_identifier_for(const domain::trade_generation_scope& scope, std::uint32_t slot);

/**
 * @brief The security identifier (ISIN-shaped) of the security with the given
 * original term.
 *
 * The population draws its trades from a pool of securities, one per currency
 * and whole year of the tenor band. A security is a fact about its issuer and
 * its terms and never about a book or a trade, so any book that keeps its
 * currency may hold it and every trade of it states the same maturity.
 */
ORES_SYNTHETIC_CORE_EXPORT std::string security_id_for(const domain::trade_generation_scope& scope,
                                                       std::uint32_t tenor_years);

/**
 * @brief The counterparty that issued the security with the given original
 * term, drawn from the tenant's counterparties in id order. The draw ignores
 * the config, so every config of a tenant names the same issuer for a security.
 */
ORES_SYNTHETIC_CORE_EXPORT boost::uuids::uuid
issuer_id_for(const domain::trade_generation_scope& scope, std::uint32_t tenor_years);

/**
 * @brief The id a config carries, named by the party it books for and by its
 * own name.
 *
 * A scope is a named recipe rather than a draw, so a provisioner that states
 * the same recipe twice reaches the same config, and the trade identifiers
 * that name the config are stable from one environment to the next.
 */
ORES_SYNTHETIC_CORE_EXPORT boost::uuids::uuid config_id_for(const boost::uuids::uuid& party_id,
                                                            const std::string& name);

/// The id one book of a config's scope carries.
ORES_SYNTHETIC_CORE_EXPORT boost::uuids::uuid book_scope_id_for(const boost::uuids::uuid& config_id,
                                                                const boost::uuids::uuid& book_id);

/// The id one counterparty of a config's scope carries.
ORES_SYNTHETIC_CORE_EXPORT boost::uuids::uuid
counterparty_scope_id_for(const boost::uuids::uuid& config_id,
                          const boost::uuids::uuid& counterparty_id);

}

#endif
