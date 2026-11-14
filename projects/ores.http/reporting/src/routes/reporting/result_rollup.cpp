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
#include "ores.http/routes/reporting/result_rollup.hpp"
#include <algorithm>
#include <cmath>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace ores::http::routes::reporting::rollup {

namespace {

/// The figure a currency states in a set of rows, or zero when it states none.
double figure_in(const std::vector<money>& figures, const std::string& currency) {
    for (const auto& one : figures)
        if (one.currency == currency)
            return std::isfinite(one.total) ? one.total : 0.0;
    return 0.0;
}

/// One figure of a set of rows, each currency added apart.
std::vector<money>
money_of(const std::vector<aggregate_row>& rows,
         const std::function<const std::vector<money>&(const aggregate_row&)>& figure_of_row) {
    std::map<std::string, double> totals;
    for (const auto& row : rows)
        for (const auto& one : figure_of_row(row))
            if (std::isfinite(one.total))
                totals[one.currency] += one.total;
    std::vector<money> out;
    out.reserve(totals.size());
    for (const auto& [currency, total] : totals)
        out.push_back(money{currency, total});
    return out;
}

/// One figure of a set of trades, each currency added apart.
std::vector<money>
sum_of(const std::vector<priced_trade>& rows,
       const std::function<double(const priced_trade&)>& amount_of,
       const std::function<const std::string&(const priced_trade&)>& currency_of) {
    std::map<std::string, double> totals;
    for (const auto& row : rows) {
        const auto amount = amount_of(row);
        if (std::isfinite(amount))
            totals[currency_of(row)] += amount;
    }
    std::vector<money> out;
    out.reserve(totals.size());
    for (const auto& [currency, total] : totals)
        out.push_back(money{currency, total});
    return out;
}

}

/// A row's figure, so the largest reads first and a large loss does not.
double figure_of(const aggregate_row& row) {
    double total = 0;
    for (const auto& one : row.totals)
        if (std::isfinite(one.total))
            total += one.total;
    return total;
}

/// The per-currency total of the run's net present values.
std::vector<money> npv_totals(const std::vector<priced_trade>& rows) {
    return sum_of(
        rows,
        [](const priced_trade& row) { return row.npv; },
        [](const priced_trade& row) -> const std::string& { return row.npv_currency; });
}

/// The change between two sets of figures, each currency apart.
change_set change_between(const std::vector<money>& current, const std::vector<money>& previous) {
    std::set<std::string> currencies;
    for (const auto& one : current)
        currencies.insert(one.currency);
    for (const auto& one : previous)
        currencies.insert(one.currency);

    change_set out;
    for (const auto& currency : currencies) {
        const auto now = figure_in(current, currency);
        const auto before = figure_in(previous, currency);
        out.change.push_back(money{currency, now - before});
        // A change stated as a share of nothing is not a figure, so it is left
        // unstated rather than drawn as no movement at all.
        out.pct.push_back(money{currency,
                                before == 0 ? 0.0 : ((now - before) / std::abs(before)) * 100.0,
                                before != 0});
    }
    return out;
}

/// The rows rolled up by the key they share.
std::vector<aggregate_row>
aggregate_rows(const std::vector<priced_trade>& rows,
               const std::function<std::string(const priced_trade&)>& key_of,
               const std::map<std::string, std::string>& names) {
    std::map<std::string, std::vector<priced_trade>> groups;
    for (const auto& row : rows)
        groups[key_of(row)].push_back(row);

    std::vector<aggregate_row> out;
    out.reserve(groups.size());
    for (const auto& [key, members] : groups) {
        const auto found = names.find(key);
        out.push_back(aggregate_row{
            key,
            found == names.end() ? std::string() : found->second,
            members.size(),
            sum_of(
                members,
                [](const priced_trade& row) { return row.npv; },
                [](const priced_trade& row) -> const std::string& { return row.npv_currency; }),
            sum_of(
                members,
                [](const priced_trade& row) { return row.notional; },
                [](const priced_trade& row) -> const std::string& {
                    return row.notional_currency;
                }),
            std::string()});
    }
    std::ranges::sort(out, [](const aggregate_row& left, const aggregate_row& right) {
        return figure_of(left) > figure_of(right);
    });
    return out;
}

/// A group row that sums its members, at the given depth.
aggregate_row node_of(const std::string& key,
                      const std::string& name,
                      const std::vector<aggregate_row>& children,
                      int depth) {
    aggregate_row node;
    node.key = key;
    node.name = name;
    for (const auto& child : children)
        node.trades += child.trades;
    node.totals =
        money_of(children, [](const aggregate_row& row) -> const auto& { return row.totals; });
    node.notional =
        money_of(children, [](const aggregate_row& row) -> const auto& { return row.notional; });
    node.is_group = true;
    node.depth = depth;
    return node;
}

/**
 * @brief The books as a tree: each portfolio above the books and portfolios it holds.
 *
 * Portfolios nest, so a portfolio's figure is its own books plus the portfolios
 * under it, and the depth lets the screen indent each row by its place. A book
 * in no portfolio, or in one whose name was not read, stands at the top rather
 * than under a guess.
 */
std::vector<aggregate_row>
with_portfolio_tree(const std::vector<aggregate_row>& books,
                    const std::map<std::string, std::string>& book_portfolio,
                    const std::map<std::string, std::string>& portfolio_name,
                    const std::map<std::string, std::string>& portfolio_parent) {
    const auto lookup = [](const std::map<std::string, std::string>& from, const std::string& key) {
        const auto found = from.find(key);
        return found == from.end() ? std::string() : found->second;
    };

    std::map<std::string, std::vector<aggregate_row>> books_under;
    std::vector<aggregate_row> standing;
    for (const auto& book : books) {
        const auto portfolio = lookup(book_portfolio, book.key);
        if (portfolio.empty() || lookup(portfolio_name, portfolio).empty()) {
            standing.push_back(book);
            continue;
        }
        books_under[portfolio].push_back(book);
    }

    // A portfolio that holds a book is a node, and so is every named portfolio
    // above it, so a nested holding is drawn under the portfolio that holds it.
    std::set<std::string> node_ids;
    for (const auto& [portfolio, held] : books_under)
        node_ids.insert(portfolio);
    for (const auto& start : node_ids) {
        auto parent = lookup(portfolio_parent, start);
        while (!parent.empty() && !node_ids.contains(parent)) {
            if (lookup(portfolio_name, parent).empty())
                break;
            node_ids.insert(parent);
            parent = lookup(portfolio_parent, parent);
        }
    }

    std::map<std::string, std::vector<std::string>> children_of;
    for (const auto& id : node_ids) {
        const auto parent = lookup(portfolio_parent, id);
        const auto key = !parent.empty() && node_ids.contains(parent) ? parent : std::string();
        children_of[key].push_back(id);
    }

    std::function<std::vector<aggregate_row>(const std::string&, int)> build;
    build = [&](const std::string& id, int depth) {
        auto own = books_under.contains(id) ? books_under[id] : std::vector<aggregate_row>{};
        std::ranges::sort(own, [](const aggregate_row& left, const aggregate_row& right) {
            return figure_of(left) > figure_of(right);
        });

        std::vector<std::pair<aggregate_row, std::vector<aggregate_row>>> children;
        for (const auto& child :
             children_of.contains(id) ? children_of[id] : std::vector<std::string>{}) {
            auto built = build(child, depth + 1);
            aggregate_row child_node = built.front();
            built.erase(built.begin());
            children.emplace_back(std::move(child_node), std::move(built));
        }
        std::ranges::sort(children, [](const auto& left, const auto& right) {
            return figure_of(left.first) > figure_of(right.first);
        });

        std::vector<aggregate_row> summed;
        for (const auto& book : own)
            summed.push_back(book);
        for (const auto& child : children)
            summed.push_back(child.first);
        const auto name = lookup(portfolio_name, id);
        auto node = node_of(id, name.empty() ? id : name, summed, depth);

        std::vector<aggregate_row> rows;
        rows.push_back(node);
        for (auto book : own) {
            book.group = node.name;
            book.depth = depth + 1;
            rows.push_back(std::move(book));
        }
        // A nested portfolio is drawn under the one that holds it, and its own
        // rows follow it.
        for (const auto& child : children) {
            rows.push_back(child.first);
            for (const auto& row : child.second)
                rows.push_back(row);
        }
        return rows;
    };

    // A portfolio whose parents lead in a circle would never be reached from the
    // top, so one that is not reached is drawn at the top rather than lost.
    std::set<std::string> reachable;
    std::function<void(const std::string&)> walk;
    walk = [&](const std::string& id) {
        if (!reachable.insert(id).second)
            return;
        for (const auto& child :
             children_of.contains(id) ? children_of[id] : std::vector<std::string>{})
            walk(child);
    };
    walk(std::string());
    for (const auto& id : node_ids) {
        if (reachable.contains(id))
            continue;
        const auto parent = lookup(portfolio_parent, id);
        if (!parent.empty() && children_of.contains(parent)) {
            auto& where = children_of[parent];
            std::erase(where, id);
        }
        children_of[std::string()].push_back(id);
        reachable.insert(id);
    }

    std::vector<std::pair<aggregate_row, std::vector<aggregate_row>>> tops;
    for (const auto& top : children_of[std::string()]) {
        auto built = build(top, 0);
        aggregate_row node = built.front();
        built.erase(built.begin());
        tops.emplace_back(std::move(node), std::move(built));
    }
    std::ranges::sort(tops, [](const auto& left, const auto& right) {
        return figure_of(left.first) > figure_of(right.first);
    });

    std::vector<aggregate_row> rows;
    for (const auto& top : tops) {
        rows.push_back(top.first);
        for (const auto& row : top.second)
            rows.push_back(row);
    }
    std::ranges::sort(standing, [](const aggregate_row& left, const aggregate_row& right) {
        return figure_of(left) > figure_of(right);
    });
    for (auto book : standing) {
        book.depth = 0;
        rows.push_back(std::move(book));
    }
    return rows;
}

/**
 * @brief The rows with a group row above the members that share a parent.
 *
 * One helper serves both levels: books group under their portfolios, and
 * counterparties under their parent counterparty. The two id spaces never mix,
 * because each call is given one parent map.
 */
std::vector<aggregate_row> with_groups(const std::vector<aggregate_row>& rows,
                                       const std::map<std::string, std::string>& parents,
                                       const std::map<std::string, std::string>& names) {
    const auto lookup = [](const std::map<std::string, std::string>& from, const std::string& key) {
        const auto found = from.find(key);
        return found == from.end() ? std::string() : found->second;
    };

    // Only a parent that has at least one child row in this run leads a group; a
    // parent whose children did not trade would otherwise repeat its own row.
    std::set<std::string> leading;
    for (const auto& row : rows) {
        const auto parent = lookup(parents, row.key);
        if (!parent.empty())
            leading.insert(parent);
    }

    std::map<std::string, std::vector<aggregate_row>> members;
    std::vector<aggregate_row> standing;
    for (const auto& row : rows) {
        const auto parent = lookup(parents, row.key);
        const auto group = !parent.empty()           ? parent :
                           leading.contains(row.key) ? row.key :
                                                       std::string();
        const auto group_name = lookup(names, group);
        if (group.empty() || group_name.empty()) {
            standing.push_back(row);
            continue;
        }
        auto member = row;
        member.group = group_name;
        member.depth = 1;
        members[group].push_back(std::move(member));
    }

    std::vector<std::pair<aggregate_row, std::vector<aggregate_row>>> groups;
    for (auto& [parent, children] : members) {
        std::ranges::sort(children, [](const aggregate_row& left, const aggregate_row& right) {
            return figure_of(left) > figure_of(right);
        });
        aggregate_row group;
        group.key = parent;
        group.name = lookup(names, parent);
        for (const auto& child : children)
            group.trades += child.trades;
        group.totals =
            money_of(children, [](const aggregate_row& row) -> const auto& { return row.totals; });
        group.notional = money_of(
            children, [](const aggregate_row& row) -> const auto& { return row.notional; });
        group.is_group = true;
        groups.emplace_back(std::move(group), std::move(children));
    }
    std::ranges::sort(groups, [](const auto& left, const auto& right) {
        return figure_of(left.first) > figure_of(right.first);
    });

    std::vector<aggregate_row> out;
    for (const auto& entry : groups) {
        out.push_back(entry.first);
        for (const auto& member : entry.second)
            out.push_back(member);
    }
    std::ranges::sort(standing, [](const aggregate_row& left, const aggregate_row& right) {
        return figure_of(left) > figure_of(right);
    });
    for (const auto& row : standing)
        out.push_back(row);
    return out;
}

/**
 * @brief The rows stated against an earlier run of the same report.
 *
 * A row the earlier run did not have carries no earlier figure, and a row only
 * the earlier run had is kept and marked, so a book or a name that stopped
 * appearing is read as gone rather than silently dropped.
 */
std::vector<aggregate_row> compared_with(const std::vector<aggregate_row>& current,
                                         const std::vector<aggregate_row>& earlier) {
    std::map<std::string, aggregate_row> before;
    for (const auto& row : earlier)
        before.emplace(row.key, row);

    std::vector<aggregate_row> rows;
    for (const auto& row : current) {
        auto stated = row;
        const auto found = before.find(row.key);
        if (found != before.end())
            stated.previous = found->second.totals;
        auto changed = change_between(stated.totals, stated.previous);
        stated.change = std::move(changed.change);
        stated.change_pct = std::move(changed.pct);
        rows.push_back(std::move(stated));
    }

    std::set<std::string> keys;
    for (const auto& row : current)
        keys.insert(row.key);

    for (const auto& row : earlier) {
        if (keys.contains(row.key))
            continue;
        auto removed = row;
        auto changed = change_between({}, row.totals);
        // A removed row states what it was, not figures that look current, so it
        // cannot be read as a book that still holds trades.
        removed.trades = 0;
        removed.previous = row.totals;
        removed.change = std::move(changed.change);
        removed.change_pct = std::move(changed.pct);
        removed.totals.clear();
        removed.notional.clear();
        removed.is_removed = true;

        // A removed row stays with its group rather than trailing the table, so a
        // group and the names under it keep reading together.
        auto anchor = std::optional<std::size_t>{};
        if (!removed.group.empty()) {
            for (std::size_t i = rows.size(); i > 0; --i)
                if (rows[i - 1].group == removed.group) {
                    anchor = i - 1;
                    break;
                }
        } else if (removed.is_group) {
            for (std::size_t i = rows.size(); i > 0; --i)
                if (rows[i - 1].key == removed.key) {
                    anchor = i - 1;
                    break;
                }
        }
        if (anchor)
            rows.insert(rows.begin() + static_cast<std::ptrdiff_t>(*anchor + 1), removed);
        else
            rows.push_back(std::move(removed));
    }
    return rows;
}


}
