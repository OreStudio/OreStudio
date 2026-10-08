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
#include "ores.trading.core/service/trade_batcher.hpp"
#include <algorithm>
#include <cstddef>
#include <format>
#include <map>
#include <set>
#include <stdexcept>
#include <unordered_map>

namespace ores::trading::service {

namespace {

/**
 * @brief The date as the batch key spells it.
 *
 * Spelled field by field rather than through the chrono formatter, so the key
 * depends on nothing but integer formatting.
 */
std::string iso_date(std::chrono::year_month_day as_of) {
    return std::format("{:04}-{:02}-{:02}",
                       static_cast<int>(as_of.year()),
                       static_cast<unsigned>(as_of.month()),
                       static_cast<unsigned>(as_of.day()));
}

/**
 * @brief The set the strategies start from: the whole population, under the
 * report's own name.
 *
 * A strategy that names the sets replaces this one; a strategy that only bounds
 * their size refines it.
 */
trade_set seed(const trade_population& population) {
    trade_set whole;
    whole.name = population.report_name;
    whole.as_of = population.as_of;
    whole.trade_ids.reserve(population.trades.size());
    for (const auto& trade : population.trades)
        whole.trade_ids.push_back(trade.trade_id);
    return whole;
}

/** The book the population names for a trade. */
const std::string& book_of_trade(const std::unordered_map<std::string, std::string>& book_of,
                                 const std::string& trade_id) {
    const auto it = book_of.find(trade_id);
    if (it == book_of.end())
        throw std::invalid_argument("The population holds no trade " + trade_id + ".");
    return it->second;
}

}

std::string batch_key(const std::string& report_name,
                      const std::string& book_id,
                      std::chrono::year_month_day as_of) {
    return std::format("{}:{}:{}", report_name, book_id, iso_date(as_of));
}

std::vector<trade_set> batch_by_book::split(const trade_population& population,
                                            const std::vector<trade_set>& sets) const {
    std::unordered_map<std::string, std::string> book_of;
    book_of.reserve(population.trades.size());
    for (const auto& trade : population.trades)
        book_of.emplace(trade.trade_id, trade.book_id);

    std::vector<trade_set> out;
    for (const auto& set : sets) {
        // An ordered map, so the sets come out in a stated order whatever order
        // the trades arrived in.
        std::map<std::string, std::vector<std::string>> by_book;
        for (const auto& trade_id : set.trade_ids)
            by_book[book_of_trade(book_of, trade_id)].push_back(trade_id);

        for (auto& [book_id, trade_ids] : by_book) {
            trade_set child;
            child.name = batch_key(population.report_name, book_id, population.as_of);
            child.as_of = population.as_of;
            child.trade_ids = std::move(trade_ids);
            out.push_back(std::move(child));
        }
    }
    return out;
}

std::string_view batch_by_book::name() const {
    return "batch-by-book";
}

at_most_trades::at_most_trades(std::size_t limit)
    : limit_(limit) {
    if (limit_ == 0)
        throw std::invalid_argument("A batch holds at least one trade.");
}

std::vector<trade_set> at_most_trades::split(const trade_population&,
                                             const std::vector<trade_set>& sets) const {
    std::vector<trade_set> out;
    for (const auto& set : sets) {
        if (set.trade_ids.size() <= limit_) {
            out.push_back(set);
            continue;
        }

        // The parts carry the set's own key and a suffix, so a producer still
        // names one artifact per set and the key stays opaque to it.
        std::size_t part = 0;
        for (std::size_t first = 0; first < set.trade_ids.size(); first += limit_) {
            ++part;
            const auto last = std::min(first + limit_, set.trade_ids.size());
            trade_set child;
            child.name = std::format("{}#{}", set.name, part);
            child.as_of = set.as_of;
            child.trade_ids.assign(set.trade_ids.begin() + static_cast<std::ptrdiff_t>(first),
                                   set.trade_ids.begin() + static_cast<std::ptrdiff_t>(last));
            out.push_back(std::move(child));
        }
    }
    return out;
}

std::string_view at_most_trades::name() const {
    return "at-most-trades";
}

composite_strategy::composite_strategy(
    std::vector<std::shared_ptr<const trade_batching_strategy>> strategies)
    : strategies_(std::move(strategies)) {
    if (strategies_.empty())
        throw std::invalid_argument("A composite strategy holds at least one strategy.");

    for (const auto& strategy : strategies_) {
        if (!name_.empty())
            name_ += ",";
        name_ += strategy->name();
    }
}

std::vector<trade_set> composite_strategy::split(const trade_population& population,
                                                 const std::vector<trade_set>& sets) const {
    std::vector<trade_set> out = sets;
    for (const auto& strategy : strategies_)
        out = strategy->split(population, out);
    return out;
}

std::string_view composite_strategy::name() const {
    return name_;
}

trade_batcher::trade_batcher(std::shared_ptr<const trade_batching_strategy> strategy)
    : strategy_(std::move(strategy)) {
    if (!strategy_)
        throw std::invalid_argument("A batcher holds a strategy.");
}

std::vector<trade_set> trade_batcher::partition(const trade_population& population) const {
    auto sets = strategy_->split(population, {seed(population)});

    // The population is the whole of what a run may price, so the sets have to
    // cover it exactly once and be addressable one from another.
    std::set<std::string> covered;
    std::set<std::string> names;
    for (const auto& set : sets) {
        if (!names.insert(set.name).second)
            throw std::runtime_error("Two sets share the batch key " + set.name + ".");
        for (const auto& trade_id : set.trade_ids)
            if (!covered.insert(trade_id).second)
                throw std::runtime_error("Trade " + trade_id + " is in more than one set.");
    }
    for (const auto& trade : population.trades)
        if (!covered.contains(trade.trade_id))
            throw std::runtime_error("Trade " + trade.trade_id + " is in no set.");

    return sets;
}

}
