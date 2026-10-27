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
        std::vector<row> out;
        out.reserve(static_cast<std::size_t>(r.rows()));
        for (int i = 0; i < r.rows(); ++i)
            out.push_back(read_row(r, i, std::index_sequence_for<Columns...>{}));
        return out;
    }

private:
    template <std::size_t... I>
    static row read_row(const result& r, int i, std::index_sequence<I...>) {
        return row{read<Columns>(r, i, static_cast<int>(I))...};
    }

    std::string_view name_;
    std::string_view sql_;
    statement_counters counters_;
};

}

#endif
