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
#include <chrono>
#include <cstdint>
#include <libpq-fe.h>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <unordered_set>

namespace ores::database::pq {

/**
 * @brief Where and as whom a connection logs in.
 */
struct connection_options final {
    std::string host;
    std::uint16_t port = 5432;
    std::string dbname;
    std::string user;
    std::string password;
    std::string application_name;
    std::chrono::seconds connect_timeout{10};

    /**
     * @brief The keyword/value connection string, each value quoted.
     */
    [[nodiscard]] ORES_DATABASE_EXPORT std::string conninfo() const;
};

/**
 * @brief One result of a statement, cleared when it goes out of scope.
 *
 * Values are read in the format the statement asked for: binary for every
 * statement of the layer.
 */
class ORES_DATABASE_EXPORT result final {
public:
    result() = default;
    explicit result(PGresult* r)
        : handle_(r) {}

    [[nodiscard]] PGresult* native() const noexcept {
        return handle_.get();
    }

    [[nodiscard]] explicit operator bool() const noexcept {
        return handle_ != nullptr;
    }

    [[nodiscard]] ExecStatusType status() const noexcept;
    [[nodiscard]] int rows() const noexcept;
    [[nodiscard]] int columns() const noexcept;
    [[nodiscard]] bool is_null(int row, int column) const noexcept;
    [[nodiscard]] std::span<const std::byte> value(int row, int column) const noexcept;

    /**
     * @brief A value of a statement that asked for text results.
     */
    [[nodiscard]] std::string_view text(int row, int column) const noexcept;

    /**
     * @brief The rows a write touched, from the command tag.
     */
    [[nodiscard]] std::uint64_t affected() const;

private:
    struct deleter {
        void operator()(PGresult* r) const noexcept {
            PQclear(r);
        }
    };
    std::unique_ptr<PGresult, deleter> handle_;
};

/**
 * @brief Closes a connection the layer opened, and leaves a borrowed one open.
 */
struct connection_closer final {
    bool owned = true;
    void operator()(PGconn* c) const noexcept {
        if (owned)
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

    /**
     * @brief Wraps a connection that sqlgen holds open, and leaves it open.
     *
     * It never goes into a pool. It is the connection of a transaction that
     * joins an open sqlgen transaction, and it goes with sqlgen.
     */
    [[nodiscard]] static connection borrow(PGconn* c) noexcept;

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
    connection() = default;

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
