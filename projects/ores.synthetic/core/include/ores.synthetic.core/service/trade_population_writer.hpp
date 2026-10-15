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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_TRADE_POPULATION_WRITER_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_TRADE_POPULATION_WRITER_HPP

#include "ores.synthetic.api/domain/trade_population_plan.hpp"
#include "ores.synthetic.core/export.hpp"
#include <cstdint>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Thrown when a reference the population names cannot be resolved, or
 * when the store refuses a write.
 *
 * The generator never invents a book, a counterparty or a trader, so a
 * reference that does not resolve stops the run and names itself. A population
 * that quietly wrote a fixture would be worse than no population, because the
 * reports would compute over a book that does not exist.
 */
class ORES_SYNTHETIC_CORE_EXPORT trade_population_write_error : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

/**
 * @brief One trade's face, as the store answers it.
 *
 * Only what the run has to decide on: whether the slot has landed already, and
 * what version the trade stands at. The rest of the trade is the store's.
 */
struct trade_population_stored final {
    std::uint32_t version = 0;
    std::uint32_t external_version = 0;
};

/**
 * @brief What a run is told to do about one planned trade.
 */
struct trade_population_write final {
    ores::synthetic::domain::trade_population_slot slot;
    /**
     * @brief The instrument the slot trades, which its book names. Several
     * trades of one book share it, and its legs are written once.
     */
    std::string security_id;
    boost::uuids::uuid issue_id;
};

/**
 * @brief What a run reports for one trade.
 */
struct trade_population_outcome final {
    boost::uuids::uuid trade_id;
    std::string internal_identifier;
    boost::uuids::uuid book_id;
    boost::uuids::uuid counterparty_id;
    std::string trade_type_code;
    /**
     * @brief True when the trade was already on the books, so the run wrote
     * nothing and the version stands still.
     */
    bool already_present = false;
    std::uint32_t version = 0;
    std::uint32_t external_version = 0;
};

/**
 * @brief The bridge between the population and the store that owns trades.
 *
 * The generator holds no trading dependency of its own, so this is the seam:
 * a run reads its references, asks whether a trade has landed, and writes
 * through the operations the trading component serves. Every method answers
 * with the stub's own types, so a test drives the whole run without a broker
 * and a production implementation is the only thing that speaks the wire.
 */
class ORES_SYNTHETIC_CORE_EXPORT trade_population_writer {
public:
    virtual ~trade_population_writer() = default;

    /**
     * @brief Reads the generation config and everything it needs to state a
     * population: its book scope, its counterparty scope, the party's active
     * books and the party's trading staff.
     *
     * @throws trade_population_write_error when a reference the config names
     * cannot be resolved.
     */
    [[nodiscard]] virtual domain::trade_generation_scope
    resolve_scope(const boost::uuids::uuid& config_id) = 0;

    /**
     * @brief The trade as the store holds it, or nothing when it holds none.
     */
    [[nodiscard]] virtual std::optional<trade_population_stored>
    read_trade(const boost::uuids::uuid& trade_id) = 0;

    /**
     * @brief Books the trade: its anchor, its booking and its state, and its
     * Internal identifier in the same run.
     */
    virtual void write_trade(const trade_population_write& write,
                             const domain::trade_generation_scope& scope) = 0;

    /**
     * @brief Writes the trade's instrument and, the first time the issue is
     * seen in a run, the issue's own rows.
     */
    virtual void write_instrument(const trade_population_write& write,
                                  const domain::trade_generation_scope& scope) = 0;
};

}

#endif
