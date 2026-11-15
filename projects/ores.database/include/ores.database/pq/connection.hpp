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
#ifndef ORES_DATABASE_PQ_CONNECTION_HPP
#define ORES_DATABASE_PQ_CONNECTION_HPP

#include "ores.database/export.hpp"
#include "ores.database/pq/options.hpp"
#include <libpq-fe.h>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_set>

namespace ores::database::pq {

/**
 * @brief One result of a statement, declared where it is passed and not read.
 */
class result;

/**
 * @brief Closes a connection the layer opened.
 */
struct connection_closer final {
    void operator()(PGconn* c) const noexcept {
        PQfinish(c);
    }
};

/**
 * @brief One long-lived connection and the statements prepared on it.
 *
 * Nothing is set on the connection when it opens: a transaction sets its own
 * context, and binary timestamps are UTC whatever the session's time zone.
 */
class ORES_DATABASE_EXPORT connection final {
public:
    /**
     * @brief Opens the connection; a failure to connect is retryable.
     */
    explicit connection(const connection_options& options);

    [[nodiscard]] PGconn* native() const noexcept {
        return handle_.get();
    }

    [[nodiscard]] bool healthy() const noexcept;

    /**
     * @brief Whether the connection is outside any transaction, as a
     * connection in the pool must be.
     */
    [[nodiscard]] bool idle() const noexcept;

    /**
     * @brief Reconnects a broken connection. A new server session holds no
     * prepared statement, so the record of them is cleared.
     */
    void reset();

    [[nodiscard]] bool is_prepared(std::string_view name) const;
    void mark_prepared(std::string_view name);

private:
    std::unique_ptr<PGconn, connection_closer> handle_;
    std::unordered_set<std::string> prepared_;
};

/**
 * @brief The one check every result goes through.
 *
 * A result that is not a success throws store_error, classed by its SQLSTATE.
 * A missing result, or one from a connection that broke, is retryable.
 */
ORES_DATABASE_EXPORT void check(const connection& c, const result& r, std::string_view what);

}

#endif
