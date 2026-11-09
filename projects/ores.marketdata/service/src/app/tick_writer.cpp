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
#include "ores.marketdata.service/app/tick_writer.hpp"
#include <format>
#include <libpq-fe.h>
#include <stdexcept>
#include <utility>

namespace ores::marketdata::service::app {

namespace {

constexpr std::size_t chunk_bytes = 1 << 20;

/// A connection string value, quoted so that a space or a quote in a password reaches libpq
/// unchanged.
std::string conninfo_value(const std::string& value) {
    std::string out = "'";
    for (const char c : value) {
        if (c == '\'' || c == '\\')
            out += '\\';
        out += c;
    }
    out += '\'';
    return out;
}

struct result_guard {
    PGresult* result;
    explicit result_guard(PGresult* r)
        : result(r) {}
    ~result_guard() {
        if (result)
            PQclear(result);
    }
    result_guard(const result_guard&) = delete;
    result_guard& operator=(const result_guard&) = delete;
};

void execute(pg_conn* conn, const char* sql) {
    result_guard result(PQexec(conn, sql));
    if (PQresultStatus(result.result) != PGRES_COMMAND_OK)
        throw std::runtime_error(PQerrorMessage(conn));
}

}

std::string tick_copy_line(const tick_row& row) {
    const auto micros = std::chrono::floor<std::chrono::microseconds>(row.ts);
    return std::format("{},{},{:%F %T}+00,{},{}\n",
                       row.stream_id,
                       row.feed_id,
                       micros,
                       row.coord,
                       row.value);
}

tick_writer::tick_writer(tick_writer_options options)
    : options_(std::move(options)) {}

tick_writer::~tick_writer() {
    close();
}

void tick_writer::close() noexcept {
    if (conn_) {
        PQfinish(conn_);
        conn_ = nullptr;
    }
}

void tick_writer::write(std::span<const tick_row> rows) {
    if (rows.empty())
        return;

    const bool new_connection = conn_ == nullptr;
    if (new_connection) {
        const auto info = std::format("host={} port={} dbname={} user={} password={} "
                                      "options='-c timezone=UTC'",
                                      conninfo_value(options_.host),
                                      options_.port,
                                      conninfo_value(options_.database),
                                      conninfo_value(options_.user),
                                      conninfo_value(options_.password));
        conn_ = PQconnectdb(info.c_str());
        if (PQstatus(conn_) != CONNECTION_OK) {
            const std::string message = PQerrorMessage(conn_);
            close();
            throw std::runtime_error("The tick writer cannot connect: " + message);
        }
    }

    try {
        if (new_connection)
            execute(conn_,
                    "create temp table ores_marketdata_ticks_stage ("
                    "stream_id uuid, feed_id uuid, ts timestamptz, coord smallint, "
                    "value double precision) on commit delete rows");
        execute(conn_, "begin");
        {
            result_guard begin(
                PQexec(conn_,
                       "copy ores_marketdata_ticks_stage "
                       "(stream_id, feed_id, ts, coord, value) "
                       "from stdin with (format csv)"));
            if (PQresultStatus(begin.result) != PGRES_COPY_IN)
                throw std::runtime_error(PQerrorMessage(conn_));
        }

        std::string chunk;
        chunk.reserve(chunk_bytes + 256);
        const auto send = [&] {
            if (chunk.empty())
                return;
            if (PQputCopyData(conn_, chunk.data(), static_cast<int>(chunk.size())) != 1)
                throw std::runtime_error(PQerrorMessage(conn_));
            chunk.clear();
        };
        for (const auto& row : rows) {
            chunk += tick_copy_line(row);
            if (chunk.size() >= chunk_bytes)
                send();
        }
        send();

        if (PQputCopyEnd(conn_, nullptr) != 1)
            throw std::runtime_error(PQerrorMessage(conn_));
        result_guard done(PQgetResult(conn_));
        if (!done.result || PQresultStatus(done.result) != PGRES_COMMAND_OK)
            throw std::runtime_error(PQerrorMessage(conn_));
        while (auto* extra = PQgetResult(conn_))
            PQclear(extra);

        // No conflict target: naming the key would need SELECT, which the role does not hold.
        execute(conn_,
                "insert into ores_marketdata_ticks_tbl "
                "(stream_id, feed_id, ts, coord, value) "
                "select stream_id, feed_id, ts, coord, value from ores_marketdata_ticks_stage "
                "on conflict do nothing");
        execute(conn_, "commit");
    } catch (const std::exception& e) {
        const std::string message = e.what();
        close();
        throw std::runtime_error("The tick writer failed to copy " + std::to_string(rows.size()) +
                                 " row(s): " + message);
    }
}

}
