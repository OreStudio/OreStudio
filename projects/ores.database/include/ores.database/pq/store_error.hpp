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
#ifndef ORES_DATABASE_PQ_STORE_ERROR_HPP
#define ORES_DATABASE_PQ_STORE_ERROR_HPP

#include "ores.database/export.hpp"
#include <stdexcept>
#include <string>
#include <string_view>

namespace ores::database::pq {

/**
 * @brief Whether a failed store operation can succeed when it is tried again.
 *
 * A retryable failure is one the store or the network caused: a lost
 * connection, a serialization failure, a deadlock, a full server. A refused
 * failure is one the request caused: a constraint, a bad value, a missing
 * permission. Trying a refused request again fails again.
 */
enum class error_kind { retryable, refused };

/**
 * @brief The class of a SQLSTATE.
 *
 * Connection exceptions (08), transaction rollbacks (40), insufficient
 * resources (53) other than a full disk (53100), a server that is shutting
 * down or starting (57P01, 57P02, 57P03), lock not available (55P03) and a
 * read-only transaction after a failover (25006) are retryable. A cancelled
 * or timed-out statement (57014) is refused: the same statement times out
 * again. Every other state, and an empty one, is refused.
 */
ORES_DATABASE_EXPORT error_kind classify(std::string_view sqlstate);

/**
 * @brief A failed store operation, with its SQLSTATE and its class.
 */
class ORES_DATABASE_EXPORT store_error : public std::runtime_error {
public:
    store_error(std::string message, std::string sqlstate, error_kind kind);

    [[nodiscard]] const std::string& sqlstate() const noexcept {
        return sqlstate_;
    }

    [[nodiscard]] error_kind kind() const noexcept {
        return kind_;
    }

    [[nodiscard]] bool retryable() const noexcept {
        return kind_ == error_kind::retryable;
    }

private:
    std::string sqlstate_;
    error_kind kind_;
};

}

#endif
