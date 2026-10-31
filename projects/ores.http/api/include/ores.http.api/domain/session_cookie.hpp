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
#ifndef ORES_HTTP_DOMAIN_SESSION_COOKIE_HPP
#define ORES_HTTP_DOMAIN_SESSION_COOKIE_HPP

#include "ores.http.api/export.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::http::domain {

/**
 * @brief How this deployment names and protects the cookie a session rides in.
 */
struct ORES_HTTP_API_EXPORT cookie_settings final {
    /**
     * @brief The name the browser stores it under.
     *
     * Carries the environment's suffix, so two deployments on one host cannot
     * overwrite each other's session.
     */
    std::string name = "ores_web_session";

    /// Whether this deployment answers over TLS, which decides =Secure=.
    bool secure = false;

    /// The path the cookie is scoped to.
    std::string path = "/";
};

/**
 * @brief The cookie that carries a session's identifier, and how to read it back.
 *
 * The identifier is the only thing in it: not the token, not the account. A script
 * on the page cannot read it (=HttpOnly=), a cross-site form does not send it
 * (=SameSite=Lax=), and over TLS it is =Secure= and named with the =__Host-= prefix
 * that the flag requires.
 */
class ORES_HTTP_API_EXPORT session_cookie final {
public:
    /**
     * @brief The =Set-Cookie= value that gives a browser a session.
     */
    [[nodiscard]] static std::string issue(const cookie_settings& settings,
                                           const std::string& identifier);

    /**
     * @brief The =Set-Cookie= value that takes a session away.
     */
    [[nodiscard]] static std::string clear(const cookie_settings& settings);

    /**
     * @brief The session identifier a request carries, when it carries one.
     *
     * @param header the request's =Cookie= header, as sent
     */
    [[nodiscard]] static std::optional<std::string> read(std::string_view header,
                                                         const cookie_settings& settings);

    /**
     * @brief The name the cookie is issued under.
     *
     * Over TLS the name takes the =__Host-= prefix, which a browser only accepts
     * with =Secure=, a =/= path and no domain attribute, which is what this builds.
     */
    [[nodiscard]] static std::string issue_name(const cookie_settings& settings);
};

}

#endif
