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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_DATABASE_PQ_WHERE_HPP
#define ORES_DATABASE_PQ_WHERE_HPP

#include "ores.database/pq/statement.hpp"
#include "ores.platform/time/datetime.hpp"
#include <algorithm>
#include <chrono>
#include <functional>
#include <initializer_list>
#include <optional>
#include <string>
#include <utility>
#include <vector>

/*
 * The conditions and orders of a list read, built on a dynamic_statement.
 * A condition binds its values into the statement and returns its SQL, so no
 * value is ever written into the text. Column names are code: the generator
 * writes them, and a requested order is checked against the entity's sortable
 * fields before it reaches here.
 */
namespace ores::database::pq::where {

/**
 * @brief A condition: given the statement it joins, binds its values and
 * returns its SQL.
 */
using condition = std::function<std::string(dynamic_statement&)>;

template <typename T>
condition equals(std::string column, T value) {
    return [column = std::move(column), value = std::move(value)](dynamic_statement& s) {
        return column + " = " + s.bind(value);
    };
}

template <typename T>
condition at_least(std::string column, T value) {
    return [column = std::move(column), value = std::move(value)](dynamic_statement& s) {
        return column + " >= " + s.bind(value);
    };
}

template <typename T>
condition at_most(std::string column, T value) {
    return [column = std::move(column), value = std::move(value)](dynamic_statement& s) {
        return column + " <= " + s.bind(value);
    };
}

inline condition is_null(std::string column) {
    return [column = std::move(column)](dynamic_statement&) {
        return column + " is null";
    };
}

/**
 * @brief The column holds one of the values. No values holds for no row.
 */
template <typename T>
condition one_of(std::string column, std::vector<T> values) {
    return [column = std::move(column), values = std::move(values)](dynamic_statement& s) {
        if (values.empty())
            return std::string("false");
        return column + " = any(" + s.bind(values) + ")";
    };
}

/**
 * @brief Any of the columns contains the text, ignoring case.
 */
inline condition contains_any(std::initializer_list<std::string> columns, std::string text) {
    return [columns = std::vector<std::string>(columns),
            text = std::move(text)](dynamic_statement& s) {
        const auto needle = s.bind(text);
        std::string sql = "(";
        for (std::size_t i = 0; i < columns.size(); ++i) {
            if (i > 0)
                sql += " or ";
            sql += "strpos(lower(" + columns[i] + "), lower(" + needle + ")) > 0";
        }
        return sql + ")";
    };
}

/**
 * @brief The row was current at the instant, or is current now when there is
 * none.
 */
inline condition valid_at(std::optional<std::chrono::system_clock::time_point> as_of) {
    return [as_of](dynamic_statement& s) {
        if (!as_of)
            return std::string("valid_to = ores_utility_infinity_timestamp_fn()");
        const auto at = s.bind(*as_of);
        return "valid_from <= " + at + " and valid_to > " + at;
    };
}

/**
 * @brief The same, for an instant stated as the ISO 8601 text a list request
 * carries; parse_as_of has already refused a malformed one.
 */
inline condition valid_at(const std::optional<std::string>& as_of) {
    if (!as_of)
        return valid_at(std::optional<std::chrono::system_clock::time_point>());
    return valid_at(std::optional(ores::platform::time::datetime::from_iso8601_utc(*as_of)));
}

/**
 * @brief Every condition holds. None holds for every row.
 */
inline condition all_of(std::vector<condition> conditions) {
    return [conditions = std::move(conditions)](dynamic_statement& s) {
        if (conditions.empty())
            return std::string("true");
        std::string sql;
        for (std::size_t i = 0; i < conditions.size(); ++i) {
            if (i > 0)
                sql += " and ";
            sql += "(" + conditions[i](s) + ")";
        }
        return sql;
    };
}

/**
 * @brief A condition narrowed by another, when there is one.
 */
inline condition narrowed(condition base, std::optional<condition> filter) {
    if (!filter)
        return base;
    return all_of({std::move(base), std::move(*filter)});
}

/**
 * @brief An order: the stated columns, then the key columns the stated ones
 * leave out, so a page is never ambiguous.
 */
inline std::string order_by(std::initializer_list<std::string> ordered,
                            bool descending,
                            std::initializer_list<std::string> key) {
    std::string sql;
    const auto add = [&](const std::string& column, bool desc) {
        sql += sql.empty() ? " order by " : ", ";
        sql += column;
        if (desc)
            sql += " desc";
    };
    for (const auto& c : ordered)
        add(c, descending);
    for (const auto& c : key)
        if (std::ranges::find(ordered, c) == ordered.end())
            add(c, false);
    return sql;
}

}

#endif
