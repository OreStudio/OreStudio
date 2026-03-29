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
#ifndef ORES_SERVICE_ERROR_CODE_HPP
#define ORES_SERVICE_ERROR_CODE_HPP

namespace ores::service {

/**
 * @brief Error codes returned by service-layer request helpers.
 */
enum class error_code {
    /**
     * @brief The request did not carry a valid credential.
     *
     * No Authorization header, invalid format, or JWT signature failure.
     * The client should log in before retrying.
     */
    unauthorized,

    /**
     * @brief The JWT token has expired.
     *
     * The credential was well-formed but has passed its expiry time.
     * The client may attempt a token refresh (iam.v1.auth.refresh) and
     * then retry the original request.
     */
    token_expired,

    /**
     * @brief The caller is authenticated but lacks the required permission.
     *
     * The JWT was valid but the caller's permission list does not contain
     * the permission required by this handler.
     */
    forbidden
};

} // namespace ores::service

#endif
