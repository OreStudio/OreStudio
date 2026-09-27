/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * @brief Asks for the HTTP server's base URL.
 *
 * It carries no fields, because the answer is the same for every caller.
 * Requires a valid session: the base URL is internal infrastructure and
 * is not exposed to unauthenticated callers.
 */
export interface GetHttpInfoRequest {
}

/**
 * @brief The HTTP server's externally reachable base URL.
 */
export interface GetHttpInfoResponse {
    /**
     * @brief The base URL, for example "http://localhost:51000".
     */
    base_url: string;
    /**
     * @brief Whether the base URL was produced.
     */
    success: boolean;
    /**
     * @brief Why it was not, when it was not.
     */
    message: string;
}

export const subjects = {
    get_http_info_request: "http.v1.info.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_http_info_request: true,
} as const;
