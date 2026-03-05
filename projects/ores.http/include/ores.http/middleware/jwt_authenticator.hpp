/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_HTTP_MIDDLEWARE_JWT_AUTHENTICATOR_HPP
#define ORES_HTTP_MIDDLEWARE_JWT_AUTHENTICATOR_HPP

// JWT authenticator has moved to ores.security. This header is retained for
// backward compatibility.
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_error.hpp"

namespace ores::http::middleware {

using jwt_authenticator = ores::security::jwt::jwt_authenticator;
using jwt_error         = ores::security::jwt::jwt_error;
using ores::security::jwt::to_string;

}

#endif
