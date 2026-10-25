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
#ifndef ORES_MARKETDATA_SERVICE_APP_TICK_WRITER_HPP
#define ORES_MARKETDATA_SERVICE_APP_TICK_WRITER_HPP

#include "ores.marketdata.service/export.hpp"
#include <chrono>
#include <span>
#include <string>
#include <string_view>

struct pg_conn;

namespace ores::marketdata::service::app {

/**
 * @brief One row of the tick table: a value of one coordinate of one stream at a time.
 *
 * The ids are views, because a batch repeats the same few of them for every row. The text they
 * view must outlive the row.
 */
struct tick_row final {
    std::string_view stream_id;
    std::string_view source_id;
    std::chrono::system_clock::time_point ts;
    short coord = 0;
    double value = 0;
};

/**
 * @brief The CSV line that COPY reads for @p row, ending in a newline.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::string tick_copy_line(const tick_row& row);

/**
 * @brief Where the tick writer connects, and as whom.
 */
struct tick_writer_options final {
    std::string user;
    std::string password;
    std::string host = "localhost";
    std::string database;
    int port = 5432;
};

/**
 * @brief Writes rows of the tick table with COPY, as the tick writer role.
 *
 * The role has INSERT only, and it does not own the table or hold SELECT. The tick table carries
 * no row-level security, so a role with INSERT writes with COPY. The writer holds one connection
 * of its own and opens it on the first write. It is not safe for two threads, and the ingest
 * worker is its only caller. A write that fails closes the connection, so the next write opens a
 * new one.
 *
 * Every write is one COPY, so it is one transaction.
 */
class ORES_MARKETDATA_SERVICE_EXPORT tick_writer {
public:
    explicit tick_writer(tick_writer_options options);
    virtual ~tick_writer();
    tick_writer(const tick_writer&) = delete;
    tick_writer& operator=(const tick_writer&) = delete;

    /**
     * @brief Copies @p rows into the tick table.
     *
     * @throws std::runtime_error when the connection or the COPY fails.
     */
    virtual void write(std::span<const tick_row> rows);

private:
    void close() noexcept;

    tick_writer_options options_;
    pg_conn* conn_ = nullptr;
};

}

#endif
