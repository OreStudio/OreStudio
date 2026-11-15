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
#ifndef ORES_DATABASE_PQ_OPTIONS_HPP
#define ORES_DATABASE_PQ_OPTIONS_HPP

#include "ores.database/export.hpp"
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <string>

namespace ores::database::pq {

/**
 * @brief Where and as whom a connection logs in.
 *
 * Held by value in the context and in a pool, so it lives in its own header and
 * brings no libpq type with it. A header that only stores these options does
 * not have to parse =libpq-fe.h=.
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
 * @brief The shape of the waits between attempts to open a pool.
 */
enum class backoff { linear, exponential };

/**
 * @brief How hard a pool tries to open its connections before it gives up.
 *
 * A database that is restarting refuses connections for a while; the attempts
 * absorb that instead of failing the first request after it. Only a failure
 * to connect is retried.
 */
struct open_policy final {
    /// Maximum number of attempts.
    std::size_t attempts = 10;
    /// Base wait between attempts.
    std::chrono::seconds wait{1};
    /// Wait shape between attempts.
    backoff shape = backoff::exponential;
};

}

#endif
