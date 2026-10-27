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
#ifndef ORES_DATABASE_PQ_STATEMENT_HPP
#define ORES_DATABASE_PQ_STATEMENT_HPP

#include "ores.database/pq/codec.hpp"
#include <array>
#include <atomic>
#include <cstdint>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

namespace ores::database::pq {

/**
 * @brief How often a statement was prepared and run, across connections.
 *
 * A statement is prepared once on each connection that runs it, so in a
 * steady state the prepares stop growing while the executions go on.
 */
struct statement_counters final {
    mutable std::atomic<std::uint64_t> prepares{0};
    mutable std::atomic<std::uint64_t> executions{0};
};

/**
 * @brief Reads every row of a result into the given column types.
 */
template <typename... Columns>
std::vector<std::tuple<Columns...>> read_rows(const result& r) {
    std::vector<std::tuple<Columns...>> out;
    out.reserve(static_cast<std::size_t>(r.rows()));
    for (int i = 0; i < r.rows(); ++i) {
        [&]<std::size_t... I>(std::index_sequence<I...>) {
            out.emplace_back(read<Columns>(r, i, static_cast<int>(I))...);
        }(std::index_sequence_for<Columns...>{});
    }
    return out;
}

/**
 * @brief A statement declared once whose parameters and results are text, as
 * a repository that fills a text entity runs. The server types each parameter
 * from where it is used, so the statement names no types; it is prepared once
 * per connection all the same.
 */
class text_statement final {
public:
    text_statement(std::string_view name, std::string_view sql)
        : name_(name)
        , sql_(sql) {}

    text_statement(const text_statement&) = delete;
    text_statement& operator=(const text_statement&) = delete;

    [[nodiscard]] std::string_view name() const noexcept {
        return name_;
    }

    [[nodiscard]] std::string_view sql() const noexcept {
        return sql_;
    }

    [[nodiscard]] const statement_counters& counters() const noexcept {
        return counters_;
    }

private:
    std::string_view name_;
    std::string_view sql_;
    statement_counters counters_;
};

/**
 * @brief A statement whose text is built at run time, such as a list read
 * whose filter and order the request states.
 *
 * Every value is bound, never written into the text: bind() keeps the value
 * as a typed parameter and returns its placeholder. Only code, such as a
 * column a whitelist allowed, is appended as text. It runs unnamed, so it is
 * parsed on each run and never prepared.
 */
class dynamic_statement final {
public:
    explicit dynamic_statement(std::string sql = {})
        : sql_(std::move(sql)) {}

    dynamic_statement& append(std::string_view text) {
        sql_ += text;
        return *this;
    }

    template <typename T>
    std::string bind(const T& v) {
        params_.push_back(encode_param<T>(v));
        oids_.push_back(params_.back().oid);
        return "$" + std::to_string(params_.size());
    }

    /**
     * @brief Adds a parameter already encoded, for a statement whose text
     * already holds its placeholder.
     */
    void add(param p) {
        oids_.push_back(p.oid);
        params_.push_back(std::move(p));
    }

    [[nodiscard]] const std::string& sql() const noexcept {
        return sql_;
    }

    [[nodiscard]] const std::vector<param>& params() const noexcept {
        return params_;
    }

    [[nodiscard]] const std::vector<Oid>& oids() const noexcept {
        return oids_;
    }

private:
    std::string sql_;
    std::vector<param> params_;
    std::vector<Oid> oids_;
};

/**
 * @brief A statement declared once: its name, its SQL, the types of its
 * parameters and the types of the columns it returns.
 *
 * Declare it as an =inline const= at namespace scope. The parameter types
 * become the OIDs it is prepared with, and the column types decide how each
 * row is read, so a statement whose arguments or reader disagree with its
 * declaration does not compile.
 */
template <typename Columns, typename... Params>
class statement;

template <typename... Columns, typename... Params>
class statement<std::tuple<Columns...>, Params...> {
public:
    using row = std::tuple<Columns...>;
    static constexpr std::size_t arity = sizeof...(Params);
    static constexpr std::array<Oid, arity> param_oids{oid_of<Params>()...};

    statement(std::string_view name, std::string_view sql)
        : name_(name)
        , sql_(sql) {}

    statement(const statement&) = delete;
    statement& operator=(const statement&) = delete;

    [[nodiscard]] std::string_view name() const noexcept {
        return name_;
    }

    [[nodiscard]] std::string_view sql() const noexcept {
        return sql_;
    }

    [[nodiscard]] const statement_counters& counters() const noexcept {
        return counters_;
    }

    /**
     * @brief Encodes the arguments, in the binary format, in declared order.
     */
    [[nodiscard]] std::array<param, arity> encode(const Params&... args) const {
        return {encode_param<Params>(args)...};
    }

    /**
     * @brief Reads every row of a result into the declared column types.
     */
    [[nodiscard]] std::vector<row> rows(const result& r) const {
        return read_rows<Columns...>(r);
    }

private:
    std::string_view name_;
    std::string_view sql_;
    statement_counters counters_;
};

}

#endif
