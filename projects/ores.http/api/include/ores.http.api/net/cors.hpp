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
 * @brief The headers an answer to a request that states @p origin carries.
 *
 * Two modes, and they are not interchangeable.
 *
 * A wildcard deployment answers every origin, without credentials. That is what an
 * anonymous caller needs -- a tool, a page that reads the rates -- and it is what a
 * session can never use, because a browser refuses to send a cookie to an answer that
 * names every origin.
 *
 * A deployment that lists its origins answers only those, and to those it allows
 * credentials on the same answer. So a session rides a call only to an origin the list
 * names, and no other page can read the API with the person's cookie.
 *
 * A request that states no origin is not a cross-origin request at all and earns
 * nothing: a caller that is not a browser never sends one.
 */
ORES_HTTP_API_EXPORT
std::vector<std::pair<std::string, std::string>> cors_headers(const http_server_options& options,
                                                             std::string_view origin);

/** The same, with what a preflight also needs: the methods and the headers it may send. */
ORES_HTTP_API_EXPORT
std::vector<std::pair<std::string, std::string>>
cors_preflight_headers(const http_server_options& options, std::string_view origin);

/**
 * @brief Whether the deployment names @p origin explicitly.
 *
 * A wildcard names none of them: it admits every origin to read, and no origin to act
 * with a session, because it cannot tell a page we serve from any other. An origin the
 * list names is one we serve, which is what makes it a trustworthy place for a write to
 * come from.
 */
ORES_HTTP_API_EXPORT
bool cors_names(const http_server_options& options, std::string_view origin);

}

#endif
