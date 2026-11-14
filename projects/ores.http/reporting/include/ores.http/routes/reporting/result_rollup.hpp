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
#ifndef ORES_HTTP_ROUTES_REPORTING_RESULT_ROLLUP_HPP
#define ORES_HTTP_ROUTES_REPORTING_RESULT_ROLLUP_HPP

#include <cstddef>
#include <functional>
#include <map>
#include <string>
#include <vector>

namespace ores::http::routes::reporting::rollup {

/**
 * @brief A figure in the currency it is stated in.
 *
 * A share of no figure is not a figure, so one that is not stated carries no
 * text rather than a figure of zero.
 */
struct money final {
    std::string currency;
    double total = 0;
    bool stated = true;
};

/// One figure of a run, as the engine stated it, at the trade it is about.
struct priced_trade final {
    std::string trade_id;
    double npv = 0;
    std::string npv_currency;
    double npv_base = 0;
    bool has_npv_base = false;
    double notional = 0;
    std::string notional_currency;
    std::string counterparty;
    std::string counterparty_name;
    std::string book_id;
    std::string book_name;
};

/// One row of an answer at the book or counterparty level.
struct aggregate_row final {
    std::string key;
    std::string name;
    std::size_t trades = 0;
    std::vector<money> totals;
    std::vector<money> notional;
    std::string group;
    bool is_group = false;
    std::vector<money> previous;
    std::vector<money> change;
    std::vector<money> change_pct;
    bool is_removed = false;
    int depth = 0;
};

/// The change between two sets of figures, and the change as a share.
struct change_set final {
    std::vector<money> change;
    std::vector<money> pct;
};

/// A row's figure, so the largest reads first and a large loss does not.
double figure_of(const aggregate_row& row);

/// The per-currency total of the run's net present values.
std::vector<money> npv_totals(const std::vector<priced_trade>& rows);

/// The change between two sets of figures, each currency apart.
change_set change_between(const std::vector<money>& current,
                          const std::vector<money>& previous);

/// The rows rolled up by the key they share, largest first.
std::vector<aggregate_row>
aggregate_rows(const std::vector<priced_trade>& rows,
               const std::function<std::string(const priced_trade&)>& key_of,
               const std::map<std::string, std::string>& names);

/**
 * @brief The books as a tree: each portfolio above the books it holds.
 *
 * Portfolios nest, so a portfolio's figure is its own books plus the portfolios
 * under it. A book in no portfolio, or in one whose name was not read, stands at
 * the top rather than under a guess.
 */
std::vector<aggregate_row>
with_portfolio_tree(const std::vector<aggregate_row>& books,
                    const std::map<std::string, std::string>& book_portfolio,
                    const std::map<std::string, std::string>& portfolio_name,
                    const std::map<std::string, std::string>& portfolio_parent);

/**
 * @brief The rows with a group row above the members that share a parent.
 *
 * A parent that also trades in the run is one of its own group's members, so its
 * trades are counted once and its row is stated under the group it leads.
 */
std::vector<aggregate_row> with_groups(const std::vector<aggregate_row>& rows,
                                       const std::map<std::string, std::string>& parents,
                                       const std::map<std::string, std::string>& names);

/**
 * @brief The rows stated against an earlier run of the same report.
 *
 * A row only the earlier run had is kept and marked, so a book that stopped
 * appearing is read as gone rather than silently dropped.
 */
std::vector<aggregate_row> compared_with(const std::vector<aggregate_row>& current,
                                         const std::vector<aggregate_row>& earlier);

}

#endif
