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
#include "ores.database/pq/connection.hpp"
#include "ores.database/pq/store_error.hpp"
#include <cassert>
#include <charconv>

namespace ores::database::pq {

namespace {

void append(std::string& out, std::string_view key, std::string_view value) {
    if (value.empty())
        return;
    if (!out.empty())
        out += ' ';
    out += key;
    out += "='";
    for (const char ch : value) {
        if (ch == '\'' || ch == '\\')
            out += '\\';
        out += ch;
    }
    out += '\'';
}

std::string message_of(const connection& c, std::string_view what) {
    std::string m(what);
    m += ": ";
    m += PQerrorMessage(c.native());
    while (!m.empty() && (m.back() == '\n' || m.back() == ' '))
        m.pop_back();
    return m;
}

}

std::string connection_options::conninfo() const {
    std::string out;
    append(out, "host", host);
    append(out, "port", std::to_string(port));
    append(out, "dbname", dbname);
    append(out, "user", user);
    append(out, "password", password);
    append(out, "application_name", application_name);
    append(out, "connect_timeout", std::to_string(connect_timeout.count()));
    return out;
}

ExecStatusType result::status() const noexcept {
    return PQresultStatus(handle_.get());
}

int result::rows() const noexcept {
    return PQntuples(handle_.get());
}

int result::columns() const noexcept {
    return PQnfields(handle_.get());
}

bool result::is_null(int row, int column) const noexcept {
    return PQgetisnull(handle_.get(), row, column) == 1;
}

std::span<const std::byte> result::value(int row, int column) const noexcept {
    const auto* data = reinterpret_cast<const std::byte*>(PQgetvalue(handle_.get(), row, column));
    return {data, static_cast<std::size_t>(PQgetlength(handle_.get(), row, column))};
}

std::string_view result::text(int row, int column) const noexcept {
    return {PQgetvalue(handle_.get(), row, column),
            static_cast<std::size_t>(PQgetlength(handle_.get(), row, column))};
}

std::uint64_t result::affected() const {
    const std::string_view tag = PQcmdTuples(handle_.get());
    std::uint64_t n = 0;
    std::from_chars(tag.data(), tag.data() + tag.size(), n);
    return n;
}

connection::connection(const connection_options& options)
    : handle_(PQconnectdb(options.conninfo().c_str())) {
    if (!handle_)
        throw store_error("Could not allocate a connection", "", error_kind::retryable);
    if (PQstatus(handle_.get()) != CONNECTION_OK)
        throw store_error(message_of(*this, "Could not connect"), "08001", error_kind::retryable);
}

connection connection::borrow(PGconn* c) noexcept {
    assert(c != nullptr);
    connection out;
    out.handle_ = std::unique_ptr<PGconn, connection_closer>(c, connection_closer{.owned = false});
    return out;
}

bool connection::healthy() const noexcept {
    return PQstatus(handle_.get()) == CONNECTION_OK;
}

bool connection::idle() const noexcept {
    return PQtransactionStatus(handle_.get()) == PQTRANS_IDLE;
}

void connection::reset() {
    prepared_.clear();
    PQreset(handle_.get());
    if (!healthy())
        throw store_error(message_of(*this, "Could not reconnect"), "08001", error_kind::retryable);
}

bool connection::is_prepared(std::string_view name) const {
    return prepared_.contains(std::string(name));
}

void connection::mark_prepared(std::string_view name) {
    prepared_.emplace(name);
}

void check(const connection& c, const result& r, std::string_view what) {
    if (!r) {
        throw store_error(message_of(c, what), c.healthy() ? "" : "08006", error_kind::retryable);
    }
    switch (r.status()) {
        case PGRES_COMMAND_OK:
        case PGRES_TUPLES_OK:
        case PGRES_SINGLE_TUPLE:
        case PGRES_PIPELINE_SYNC:
        case PGRES_COPY_IN:
            return;
        default:
            break;
    }
    const char* state = PQresultErrorField(r.native(), PG_DIAG_SQLSTATE);
    std::string sqlstate = state ? state : "";
    std::string message(what);
    message += ": ";
    if (const char* m = PQresultErrorMessage(r.native()); m && *m)
        message += m;
    else
        message += PQerrorMessage(c.native());
    while (!message.empty() && (message.back() == '\n' || message.back() == ' '))
        message.pop_back();
    if (sqlstate.empty() && !c.healthy())
        sqlstate = "08006";
    const auto kind = sqlstate.empty() && r.status() == PGRES_PIPELINE_ABORTED ?
                          error_kind::refused :
                          classify(sqlstate);
    throw store_error(std::move(message), std::move(sqlstate), kind);
}

}
