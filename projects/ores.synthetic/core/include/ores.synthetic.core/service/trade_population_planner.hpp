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
 * @brief How many trades one book receives per generation.
 *
 * The count is a property of the generator and not of the config, because the
 * config states the shape of the population and not its size: a reviewer reads
 * the bands and the scope, and the size is a constant the acceptance can name.
 */
inline constexpr std::uint32_t trades_per_book = 8;

/**
 * @brief Builds the population a scope states.
 *
 * Pure and deterministic: the answer is a function of the scope alone, so the
 * same scope states the same trades on every run, and a test can compare two
 * plans byte for byte without a database. Every identifier, date and amount is
 * derived and none is drawn from the clock.
 *
 * @throws trade_population_error when the scope cannot state a population.
 */
ORES_SYNTHETIC_CORE_EXPORT domain::trade_population_plan
plan_population(const domain::trade_generation_scope& scope);

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
 * term, drawn from the scope's counterparties.
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
