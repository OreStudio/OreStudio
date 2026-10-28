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
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include <map>
#include <memory>
#include <mutex>

namespace ores::database::query_builder {

namespace {

/*
 * The longest a statement may take before its timing is logged as a warning.
 */
constexpr long long slow_statement_ms = 250;

struct prepared_entry {
    std::string name;
    std::string sql;
    std::unique_ptr<pq::text_statement> statement;
};

}

condition valid_at(const std::optional<std::string>& as_of) {
    if (!as_of)
        return {"valid_to = ?", {pq::text_param(repository::MAX_TIMESTAMP)}};
    return {"valid_from <= ? and valid_to > ?", {pq::text_param(*as_of), pq::text_param(*as_of)}};
}

std::string detail::numbered(const std::string& sql) {
    std::string out;
    out.reserve(sql.size() + 16);
    int n = 0;
    for (const char c : sql) {
        if (c == '?')
            out += "$" + std::to_string(++n);
        else
            out += c;
    }
    return out;
}

const pq::text_statement& detail::prepared(const std::string& sql) {
    static std::mutex mutex;
    static std::map<std::string, std::unique_ptr<prepared_entry>, std::less<>> statements;
    std::scoped_lock lock(mutex);
    auto& entry = statements[sql];
    if (!entry) {
        entry = std::make_unique<prepared_entry>();
        entry->name = "ores_q_" + std::to_string(statements.size());
        entry->sql = sql;
        entry->statement = std::make_unique<pq::text_statement>(entry->name, entry->sql);
    }
    return *entry->statement;
}

pq::result detail::run(pq::transaction& tx,
                       const std::string& sql,
                       const std::vector<pq::param>& params,
                       bool variadic) {
    if (!variadic)
        return tx.run(prepared(sql), params);
    pq::dynamic_statement s(sql);
    for (const auto& p : params)
        s.add(p);
    return tx.run_text(s);
}

void detail::refused(const pq::store_error& e, logging::logger_t& lg) {
    using namespace ores::logging;
    BOOST_LOG_SEV(lg, error) << e.what();
    BOOST_THROW_EXCEPTION(
        repository::repository_exception(std::format("Repository error: {}", e.what())));
}

void detail::finished(logging::logger_t& lg,
                      const std::string& what,
                      std::size_t rows,
                      std::chrono::steady_clock::time_point began) {
    using namespace ores::logging;
    const auto took = std::chrono::duration_cast<std::chrono::milliseconds>(
                          std::chrono::steady_clock::now() - began)
                          .count();
    BOOST_LOG_SEV(lg, took >= slow_statement_ms ? warn : debug)
        << what << ". Total: " << rows << " in " << took << " ms";
}

std::vector<std::vector<std::optional<std::string>>>
execute_parameterized_multi_column_query(context ctx,
                                         const std::string& sql,
                                         const std::vector<std::optional<std::string>>& params,
                                         logging::logger_t& lg,
                                         const std::string& operation_desc) {
    using namespace ores::logging;
    BOOST_LOG_SEV(lg, debug) << operation_desc << ".";
    const auto began = std::chrono::steady_clock::now();
    std::vector<pq::param> encoded;
    encoded.reserve(params.size());
    for (const auto& v : params)
        encoded.push_back(pq::to_text(v));
    try {
        auto rows = database::transact(ctx, [&](pq::transaction& tx) {
            const auto r = tx.run(detail::prepared(sql), encoded);
            std::vector<std::vector<std::optional<std::string>>> out(
                static_cast<std::size_t>(r.rows()));
            for (int row = 0; row < r.rows(); ++row)
                for (int column = 0; column < r.columns(); ++column)
                    out[static_cast<std::size_t>(row)].push_back(
                        pq::from_text<std::optional<std::string>>(r, row, column));
            return out;
        });
        detail::finished(lg, operation_desc, rows.size(), began);
        return rows;
    } catch (const pq::store_error& e) {
        detail::refused(e, lg);
    }
}

std::vector<std::vector<std::optional<std::string>>> execute_raw_multi_column_query(
    context ctx, const std::string& sql, logging::logger_t& lg, const std::string& operation_desc) {
    return execute_parameterized_multi_column_query(ctx, sql, {}, lg, operation_desc);
}

std::vector<std::string> execute_parameterized_string_query(context ctx,
                                                            const std::string& sql,
                                                            const std::vector<std::string>& params,
                                                            logging::logger_t& lg,
                                                            const std::string& operation_desc) {
    std::vector<std::optional<std::string>> values(params.begin(), params.end());
    std::vector<std::string> out;
    for (const auto& row :
         execute_parameterized_multi_column_query(ctx, sql, values, lg, operation_desc))
        out.push_back(row.empty() ? std::string() : row.front().value_or(std::string()));
    return out;
}

}
