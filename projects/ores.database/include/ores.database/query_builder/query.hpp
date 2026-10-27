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
#ifndef ORES_DATABASE_QUERY_BUILDER_QUERY_HPP
#define ORES_DATABASE_QUERY_BUILDER_QUERY_HPP

#include "ores.database/export.hpp"
#include "ores.database/pq/text.hpp"
#include "ores.database/query_builder/columns.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <concepts>
#include <cstdint>
#include <cstdio>
#include <initializer_list>
#include <optional>
#include <string>
#include <type_traits>
#include <vector>

/*
 * The queries a generated repository states, run on the libpq layer. The
 * names are the ones the repository templates used with sqlgen, so a
 * repository moves by changing its namespace, and the SQL is the SQL sqlgen
 * wrote: every value a text parameter the server types from where it is
 * used, as a literal was. A condition holds its SQL with '?' for each
 * parameter, and the statement numbers them when it is run.
 */
namespace ores::database::query_builder {

/**
 * @brief A condition: its SQL, with '?' for each parameter, and the values.
 *
 * A condition built from a list whose length varies has a varying text, so
 * the statement it joins runs unnamed rather than being prepared.
 */
struct condition {
    std::string sql;
    std::vector<pq::param> params;
    bool variadic = false;
};

inline condition operator&&(condition a, condition b) {
    condition r{
        "(" + a.sql + ") and (" + b.sql + ")", std::move(a.params), a.variadic || b.variadic};
    r.params.insert(r.params.end(), b.params.begin(), b.params.end());
    return r;
}

inline condition operator||(condition a, condition b) {
    condition r{
        "(" + a.sql + ") or (" + b.sql + ")", std::move(a.params), a.variadic || b.variadic};
    r.params.insert(r.params.end(), b.params.begin(), b.params.end());
    return r;
}

struct order_item {
    std::string column;
    bool desc = false;
};

/**
 * @brief The columns a page is ordered by.
 */
using order = std::vector<order_item>;

/**
 * @brief A value bound as a text parameter, as a list filter states it.
 */
using value = pq::param;

namespace detail {

inline condition compare(const std::string& column, std::string_view op, pq::param v) {
    return {column + " " + std::string(op) + " ?", {std::move(v)}};
}

}

struct column_ref {
    std::string name;

    [[nodiscard]] order_item desc() const {
        return {name, true};
    }

    operator order_item() const {
        return {name, false};
    }

    template <typename T>
    [[nodiscard]] condition in(const std::vector<T>& values) const {
        if (values.empty())
            return {"false", {}, true};
        condition r{name + " in (", {}, true};
        for (std::size_t i = 0; i < values.size(); ++i) {
            r.sql += i == 0 ? "?" : ", ?";
            r.params.push_back(pq::to_text(values[i]));
        }
        r.sql += ")";
        return r;
    }
};

inline pq::param to_param(const char* v) {
    return pq::text_param(v);
}

template <typename T>
pq::param to_param(const T& v) {
    return pq::to_text(v);
}

template <typename T>
condition operator==(const column_ref& c, const T& v) {
    return detail::compare(c.name, "=", to_param(v));
}

template <typename T>
condition operator!=(const column_ref& c, const T& v) {
    return detail::compare(c.name, "<>", to_param(v));
}

template <typename T>
condition operator<(const column_ref& c, const T& v) {
    return detail::compare(c.name, "<", to_param(v));
}

template <typename T>
condition operator<=(const column_ref& c, const T& v) {
    return detail::compare(c.name, "<=", to_param(v));
}

template <typename T>
condition operator>(const column_ref& c, const T& v) {
    return detail::compare(c.name, ">", to_param(v));
}

template <typename T>
condition operator>=(const column_ref& c, const T& v) {
    return detail::compare(c.name, ">=", to_param(v));
}

namespace literals {

inline column_ref operator""_c(const char* name, std::size_t size) {
    return {std::string(name, size)};
}

}

struct where_clause {
    condition c;
};

inline where_clause where(condition c) {
    return {std::move(c)};
}

struct order_clause {
    order items;
};

template <typename... Items>
order_clause order_by(const Items&... items) {
    return {{order_item(items)...}};
}

struct limit_clause {
    std::uint32_t n;
};

inline limit_clause limit(std::uint32_t n) {
    return {n};
}

struct offset_clause {
    std::uint32_t n;
};

inline offset_clause offset(std::uint32_t n) {
    return {n};
}

/**
 * @brief A read of the rows of one entity.
 */
template <typename Entity>
struct read_query {
    std::optional<condition> filter;
    order sorting;
    std::optional<std::uint32_t> max_rows;
    std::optional<std::uint32_t> skip;
};

template <typename Vector>
inline const read_query<typename Vector::value_type> read{};

template <typename Entity>
read_query<Entity> operator|(read_query<Entity> q, where_clause w) {
    q.filter = std::move(w.c);
    return q;
}

template <typename Entity>
read_query<Entity> operator|(read_query<Entity> q, order_clause o) {
    q.sorting = std::move(o.items);
    return q;
}

template <typename Entity>
read_query<Entity> operator|(read_query<Entity> q, limit_clause l) {
    q.max_rows = l.n;
    return q;
}

template <typename Entity>
read_query<Entity> operator|(read_query<Entity> q, offset_clause o) {
    q.skip = o.n;
    return q;
}

/**
 * @brief A delete of the rows of one entity that a condition names.
 */
template <typename Entity>
struct delete_query {
    std::optional<condition> filter;
};

template <typename Entity>
inline const delete_query<Entity> delete_from{};

template <typename Entity>
delete_query<Entity> operator|(delete_query<Entity> q, where_clause w) {
    q.filter = std::move(w.c);
    return q;
}

/**
 * @brief A write of rows: an insert, or an insert that replaces the row of the
 * same key, as a current-state table keeps one row a key.
 */
template <typename Entity>
struct write_query {
    std::vector<Entity> rows;
    bool replace = false;
};

template <typename Entity>
write_query<Entity> insert(Entity row) {
    return {{std::move(row)}, false};
}

template <typename Entity>
write_query<Entity> insert(std::vector<Entity> rows) {
    return {std::move(rows), false};
}

template <typename Entity>
write_query<Entity> insert_or_replace(Entity row) {
    return {{std::move(row)}, true};
}

template <typename Entity>
write_query<Entity> insert_or_replace(std::vector<Entity> rows) {
    return {std::move(rows), true};
}

/*
 * The list-filter vocabulary of the repository templates, as conditions.
 */

inline value filter_value(const std::string& v) {
    return pq::text_param(v);
}

inline value filter_value(const boost::uuids::uuid& v) {
    return pq::text_param(boost::uuids::to_string(v));
}

inline value filter_value(bool v) {
    return pq::text_param(v ? "true" : "false");
}

template <std::integral T>
    requires(!std::same_as<T, bool>)
value filter_value(T v) {
    return pq::text_param(std::to_string(v));
}

inline value filter_value(const std::chrono::year_month_day& v) {
    char text[16];
    std::snprintf(text,
                  sizeof text,
                  "%04d-%02u-%02u",
                  static_cast<int>(v.year()),
                  static_cast<unsigned>(v.month()),
                  static_cast<unsigned>(v.day()));
    return pq::text_param(text);
}

inline value filter_value(const std::chrono::system_clock::time_point& v) {
    return pq::text_param(ores::platform::time::datetime::to_db_string(v));
}

template <typename E>
    requires std::is_enum_v<E>
value filter_value(E v) {
    return pq::text_param(std::string(to_string(v)));
}

inline condition equals(const std::string& column, value v) {
    return detail::compare(column, "=", std::move(v));
}

inline condition at_least(const std::string& column, value v) {
    return detail::compare(column, ">=", std::move(v));
}

inline condition at_most(const std::string& column, value v) {
    return detail::compare(column, "<=", std::move(v));
}

inline condition is_null(const std::string& column) {
    return {column + " is null", {}};
}

/**
 * @brief The column holds one of the values. No values holds for no row.
 */
inline condition one_of(const std::string& column, std::vector<value> values) {
    if (values.empty())
        return {"1 = 0", {}, true};
    condition r{column + " in (", std::move(values), true};
    for (std::size_t i = 0; i < r.params.size(); ++i)
        r.sql += i == 0 ? "?" : ", ?";
    r.sql += ")";
    return r;
}

/**
 * @brief Any of the columns contains the text, ignoring case.
 */
inline condition contains_any(std::initializer_list<std::string> columns, const std::string& text) {
    condition r{"(", {}};
    bool first = true;
    for (const auto& c : columns) {
        if (!first)
            r.sql += " or ";
        first = false;
        r.sql += "strpos(lower(" + c + "), lower(?)) > 0";
        r.params.push_back(pq::text_param(text));
    }
    r.sql += ")";
    return r;
}

/**
 * @brief Every condition holds. None holds for every row.
 */
inline condition all_of(std::vector<condition> conditions) {
    if (conditions.empty())
        return {"true", {}};
    condition r = std::move(conditions.front());
    for (std::size_t i = 1; i < conditions.size(); ++i)
        r = std::move(r) && std::move(conditions[i]);
    return r;
}

/**
 * @brief The row current now, or the row valid at an instant.
 */
ORES_DATABASE_EXPORT condition valid_at(const std::optional<std::string>& as_of);

/**
 * @brief A condition narrowed by another, when there is one.
 */
inline condition narrowed(condition base, std::optional<condition> filter) {
    if (!filter)
        return base;
    return std::move(base) && std::move(*filter);
}

/**
 * @brief The stated columns in the stated direction, then the key columns the
 * stated ones leave out, ascending, so a page is never ambiguous.
 */
inline order make_order(std::initializer_list<std::string> ordered,
                        bool descending,
                        std::initializer_list<std::string> key) {
    order r;
    for (const auto& name : ordered)
        r.push_back({name, descending});
    for (const auto& name : key) {
        bool stated = false;
        for (const auto& o : ordered)
            stated = stated || o == name;
        if (!stated)
            r.push_back({name, false});
    }
    return r;
}

}

#endif
