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
#ifndef ORES_DATABASE_PQ_RESULT_HPP
#define ORES_DATABASE_PQ_RESULT_HPP

#include "ores.database/export.hpp"
#include <cstddef>
#include <cstdint>
#include <libpq-fe.h>
#include <memory>
#include <span>
#include <string_view>

namespace ores::database::pq {

/**
 * @brief One result of a statement, cleared when it goes out of scope.
 *
 * Values are read in the format the statement asked for: binary for a
 * generated repository's row, text for the SQL an entity model does not
 * generate.
 *
 * A result holds a PGresult and no connection, so a statement that reads rows
 * includes this header and not the one that opens the connection.
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

}

#endif
