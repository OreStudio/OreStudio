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
#ifndef ORES_DATABASE_REPOSITORY_LIST_FILTER_HPP
#define ORES_DATABASE_REPOSITORY_LIST_FILTER_HPP

#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <concepts>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <utility>
#include <vector>

/*
 * The conditions a list's filter record sets, built at run time because which
 * members a request sets is only known when it arrives. Each member narrows
 * the rows on its own; the members a request sets must all hold.
 */
namespace ores::database::repository {

inline sqlgen::dynamic::Value filter_value(const std::string& v) {
    return {.val = sqlgen::dynamic::String{.val = v}};
}

inline sqlgen::dynamic::Value filter_value(const boost::uuids::uuid& v) {
    return {.val = sqlgen::dynamic::String{.val = boost::uuids::to_string(v)}};
}

inline sqlgen::dynamic::Value filter_value(bool v) {
    return {.val = sqlgen::dynamic::Boolean{.val = v}};
}

template <std::integral T>
    requires(!std::same_as<T, bool>)
sqlgen::dynamic::Value filter_value(T v) {
    return {.val = sqlgen::dynamic::Integer{.val = static_cast<std::int64_t>(v)}};
}

namespace detail {

inline sqlgen::dynamic::Operation column(const std::string& name) {
    return {.val = sqlgen::dynamic::Column{.name = name}};
}

inline sqlgen::dynamic::Operation value(sqlgen::dynamic::Value v) {
    return {.val = std::move(v)};
}

template <typename T>
sqlgen::Ref<sqlgen::dynamic::Operation> ref(T op) {
    return sqlgen::Ref<sqlgen::dynamic::Operation>::make(
        sqlgen::dynamic::Operation{.val = std::move(op)});
}

inline sqlgen::Ref<sqlgen::dynamic::Operation> ref(sqlgen::dynamic::Operation op) {
    return sqlgen::Ref<sqlgen::dynamic::Operation>::make(std::move(op));
}

inline sqlgen::dynamic::Condition both(sqlgen::dynamic::Condition a, sqlgen::dynamic::Condition b) {
    return {.val = sqlgen::dynamic::Condition::And{
                .cond1 = sqlgen::Ref<sqlgen::dynamic::Condition>::make(std::move(a)),
                .cond2 = sqlgen::Ref<sqlgen::dynamic::Condition>::make(std::move(b))}};
}

inline sqlgen::dynamic::Condition either(sqlgen::dynamic::Condition a,
                                         sqlgen::dynamic::Condition b) {
    return {.val = sqlgen::dynamic::Condition::Or{
                .cond1 = sqlgen::Ref<sqlgen::dynamic::Condition>::make(std::move(a)),
                .cond2 = sqlgen::Ref<sqlgen::dynamic::Condition>::make(std::move(b))}};
}

}

/**
 * @brief A row matches when its column equals the value.
 */
inline sqlgen::dynamic::Condition equals(const std::string& column, sqlgen::dynamic::Value v) {
    return {.val = sqlgen::dynamic::Condition::Equal{.op1 = detail::column(column),
                                                     .op2 = detail::value(std::move(v))}};
}

/**
 * @brief A row matches when its column is null.
 */
inline sqlgen::dynamic::Condition is_null(const std::string& column) {
    return {.val = sqlgen::dynamic::Condition::IsNull{.op = detail::column(column)}};
}

/**
 * @brief A row matches when its column equals any of the values.
 *
 * An empty list matches no row. It is stated as a condition that never holds,
 * because an empty IN list is not SQL.
 */
inline sqlgen::dynamic::Condition one_of(const std::string& column,
                                         std::vector<sqlgen::dynamic::Value> values) {
    if (values.empty())
        return {.val = sqlgen::dynamic::Condition::Equal{.op1 = detail::value(filter_value(1)),
                                                         .op2 = detail::value(filter_value(0))}};
    return {.val = sqlgen::dynamic::Condition::In{.op = detail::column(column),
                                                  .patterns = std::move(values)}};
}

/**
 * @brief A row matches when any of the columns contains the text, ignoring case.
 *
 * Both sides are folded by the database, so letters outside ASCII fold as
 * the database folds them, and the text is matched literally: removing every
 * occurrence of it shortens the column exactly when the column contains it,
 * so no character in it is a wildcard. A null column contains nothing.
 */
inline sqlgen::dynamic::Condition contains_any(std::initializer_list<std::string> columns,
                                               const std::string& text) {
    using sqlgen::dynamic::Operation;
    std::optional<sqlgen::dynamic::Condition> r;
    for (const auto& name : columns) {
        const auto folded = Operation::Lower{.op1 = detail::ref(detail::column(name))};
        const auto needle = Operation::Lower{.op1 = detail::ref(detail::value(filter_value(text)))};
        const auto removed =
            Operation::Replace{.op1 = detail::ref(folded),
                               .op2 = detail::ref(needle),
                               .op3 = detail::ref(detail::value(filter_value(std::string())))};
        sqlgen::dynamic::Condition c{
            .val = sqlgen::dynamic::Condition::LesserThan{
                .op1 = Operation{.val = Operation::Length{.op1 = detail::ref(removed)}},
                .op2 = Operation{.val = Operation::Length{.op1 = detail::ref(folded)}}}};
        r = r ? detail::either(std::move(*r), std::move(c)) : std::move(c);
    }
    return r ? std::move(*r) : one_of(std::string(), {});
}

/**
 * @brief The conditions that must all hold, or none when there are none.
 */
inline std::optional<sqlgen::dynamic::Condition>
all_of(std::vector<sqlgen::dynamic::Condition> conditions) {
    std::optional<sqlgen::dynamic::Condition> r;
    for (auto& c : conditions)
        r = r ? detail::both(std::move(*r), std::move(c)) : std::move(c);
    return r;
}

/**
 * @brief A query's own condition narrowed by a filter, when there is one.
 */
inline std::optional<sqlgen::dynamic::Condition>
narrowed(std::optional<sqlgen::dynamic::Condition> own,
         std::optional<sqlgen::dynamic::Condition> filter) {
    if (!filter)
        return own;
    if (!own)
        return filter;
    return detail::both(std::move(*own), std::move(*filter));
}

}

#endif
