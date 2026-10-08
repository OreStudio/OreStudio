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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_BATCHER_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_BATCHER_HPP

#include "ores.trading.core/export.hpp"
#include <chrono>
#include <cstddef>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file trade_batcher.hpp
 * @brief Partitioning a report's trade population into the sets a run fans out
 * over.
 *
 * A report covers hundreds to thousands of books, and each needs its own trades
 * archive, its own market data and its own portfolio. The unit the run
 * addresses is a trade *set*, named by a batch key, rather than a book: a book
 * is one way to slice a population and the first way, not the shape of the
 * system.
 *
 * A set whose context is a book is derived — its members are the trades whose
 * current booking names that book — so this machinery reads a key the trade
 * already carries and stores nothing. A set a run *read* is recorded, because a
 * repeat has to reconstruct the input it read rather than today's; that is a
 * trade group, and recording these sets against a run is that work rather than
 * this.
 */
namespace ores::trading::service {

/**
 * @brief One trade's place in the population, as a strategy sees it.
 *
 * A trade appears once, and the book is the one its current booking names.
 */
struct population_trade {
    /** The trade's identity. */
    std::string trade_id;
    /** The book the trade's current booking names. */
    std::string book_id;
};

/**
 * @brief What a report asks to be partitioned.
 *
 * The report's name and the as-of travel into every batch key, so a set names
 * the reading it came from and not only the trades in it.
 */
struct trade_population {
    /** The report whose run reads the population. */
    std::string report_name;
    /** The date the population was read at. */
    std::chrono::year_month_day as_of;
    /** Every trade in scope, each named once. */
    std::vector<population_trade> trades;
};

/**
 * @brief One set of trades: the unit the run fans out over.
 *
 * The name is the batch key, and it is *opaque* downstream: a producer reads it
 * to name its artifact and never splits it, so a strategy may choose any
 * string. =REPORT_NAME:BOOK_ID:ASOF= is a convention for the book strategy, not
 * a grammar.
 */
struct trade_set {
    /** The batch key. */
    std::string name;
    /** The date the set was read at, which travels with it. */
    std::chrono::year_month_day as_of;
    /** The trades in the set. */
    std::vector<std::string> trade_ids;
};

/**
 * @brief How a population is split into sets.
 *
 * The boundary is here rather than a switch in the caller because how work is
 * best split depends on what the grid is doing, and that changes while the
 * system runs. A switch would put grid knowledge in the report path; a strategy
 * keeps it where the population is known.
 */
class ORES_TRADING_CORE_EXPORT trade_batching_strategy {
public:
    virtual ~trade_batching_strategy() = default;

    /**
     * @brief Splits each of @p sets further.
     *
     * A set a strategy cannot split comes back unchanged, so a strategy that
     * does not apply is not an error. The trades a set holds are read from
     * @p population, which is where a strategy finds the attributes it
     * partitions on.
     */
    [[nodiscard]] virtual std::vector<trade_set>
    split(const trade_population& population, const std::vector<trade_set>& sets) const = 0;

    /**
     * @brief The strategy's name, so a run can record how it was split.
     */
    [[nodiscard]] virtual std::string_view name() const = 0;
};

/**
 * @brief Splits a set into one set per book its trades are booked in.
 *
 * This is the derived set: the members are read from the booking, and nothing
 * is stored for the set itself. It is the first strategy, and it replaces the
 * seed set rather than refining it, because it is the one that names the books.
 */
class ORES_TRADING_CORE_EXPORT batch_by_book final : public trade_batching_strategy {
public:
    [[nodiscard]] std::vector<trade_set> split(const trade_population& population,
                                               const std::vector<trade_set>& sets) const override;

    [[nodiscard]] std::string_view name() const override;
};

/**
 * @brief Splits a set holding more than a limit into sets of at most that many.
 *
 * A grid does better with work of a bounded size, so this is the second half of
 * the first real batching: batch by book, then bound each set.
 */
class ORES_TRADING_CORE_EXPORT at_most_trades final : public trade_batching_strategy {
public:
    /**
     * @brief A strategy that splits at @p limit trades.
     *
     * Throws when @p limit is zero, because a limit that admits no trade cannot
     * partition anything.
     */
    explicit at_most_trades(std::size_t limit);

    [[nodiscard]] std::vector<trade_set> split(const trade_population& population,
                                               const std::vector<trade_set>& sets) const override;

    [[nodiscard]] std::string_view name() const override;

private:
    std::size_t limit_;
};

/**
 * @brief Applies its strategies in order, each splitting what the last one
 * produced.
 *
 * Composition is what makes a report's batching a statement rather than a
 * special case: "batch by book and at most a thousand trades" is two strategies
 * composed, not a third one that reimplements both.
 */
class ORES_TRADING_CORE_EXPORT composite_strategy final : public trade_batching_strategy {
public:
    explicit composite_strategy(
        std::vector<std::shared_ptr<const trade_batching_strategy>> strategies);

    [[nodiscard]] std::vector<trade_set> split(const trade_population& population,
                                               const std::vector<trade_set>& sets) const override;

    [[nodiscard]] std::string_view name() const override;

private:
    std::vector<std::shared_ptr<const trade_batching_strategy>> strategies_;
    std::string name_;
};

/**
 * @brief The batch key a set is addressed by.
 *
 * The convention the book strategy uses, and the one place it is spelled.
 */
[[nodiscard]] ORES_TRADING_CORE_EXPORT std::string batch_key(const std::string& report_name,
                                                             const std::string& book_id,
                                                             std::chrono::year_month_day as_of);

/**
 * @brief Partitions a report's population by running a strategy over it.
 */
class ORES_TRADING_CORE_EXPORT trade_batcher {
public:
    explicit trade_batcher(std::shared_ptr<const trade_batching_strategy> strategy);

    /**
     * @brief The sets the population partitions into.
     *
     * A partition covers the population exactly once. A strategy that leaves a
     * trade out, puts one in two sets, or gives two sets the same batch key
     * would price a population nobody asked for, so the batcher refuses it
     * rather than handing it on.
     */
    [[nodiscard]] std::vector<trade_set> partition(const trade_population& population) const;

private:
    std::shared_ptr<const trade_batching_strategy> strategy_;
};

}

#endif
