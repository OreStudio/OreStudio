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
#ifndef ORES_HTTP_NET_CORS_HPP
#define ORES_HTTP_NET_CORS_HPP

#include "ores.http.api/export.hpp"
#include "ores.http.api/net/http_server_options.hpp"
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::net {

/**
 * @brief The origins a deployment admits, read once from its configuration.
 *
 * A header's case is not its meaning, and an origin is case-insensitive, so both the
 * configured origins and the one a request states are folded to lowercase here and in
 * the comparison. A deployment that names no origin admits every origin, without
 * credentials: enough for an anonymous caller, and never enough for a session, because
 * a browser refuses to send a cookie to an answer that names every origin.
 */
struct ORES_HTTP_API_EXPORT cors_policy final {
    /// Whether CORS is on at all.
    bool enabled{true};

    /// Whether the deployment names no origins, which admits them all without credentials.
    bool every_origin{true};

    /// The origins the deployment names, lowercased, in the order it names them.
    std::vector<std::string> origins;

    /**
     * @brief Whether @p origin earns an answer.
     *
     * A request that states no origin is not a cross-origin request at all, so it earns
     * nothing: a caller that is not a browser never sends one.
     */
    [[nodiscard]] bool admits(std::string_view origin) const;

    /**
     * @brief Whether the deployment names @p origin explicitly.
     *
     * A wildcard names none of them: it admits every origin to read, and no origin to act
     * with a session, because it cannot tell a page we serve from any other. An origin the
     * list names is one we serve, which is what makes it a trustworthy place for a write
     * to come from.
     */
    [[nodiscard]] bool names(std::string_view origin) const;
};

/** The policy a deployment's configuration states. */
ORES_HTTP_API_EXPORT cors_policy make_cors_policy(const http_server_options& options);

/**
 * @brief The headers an answer to a request that states @p origin carries.
 *
 * A wildcard deployment answers every origin without credentials. A deployment that names
 * its origins answers only those, and to those it allows credentials on the same answer,
 * so a session rides a call only to an origin the list names. A named origin is also the
 * only place a write may come from, besides this deployment's own.
 */
ORES_HTTP_API_EXPORT
std::vector<std::pair<std::string, std::string>> cors_headers(const cors_policy& policy,
                                                              std::string_view origin);

/** The same, with what a preflight also needs: the methods and the headers it may send. */
ORES_HTTP_API_EXPORT
std::vector<std::pair<std::string, std::string>> cors_preflight_headers(const cors_policy& policy,
                                                                        std::string_view origin);

}

#endif
