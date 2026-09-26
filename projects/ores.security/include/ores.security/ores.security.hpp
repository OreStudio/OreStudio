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
#ifndef ORES_SECURITY_HPP
#define ORES_SECURITY_HPP

/**
 * @brief Shared security primitives for ORE Studio.
 *
 * The component holds the security code the tree shares, in three facets:
 *
 * - @b crypto (ores::security::crypto): the scrypt password hash and its
 *   verification. A stored hash carries its own cost, production uses the
 *   OWASP-recommended ln=14, and verification refuses a hash weaker than the
 *   cost the running build produces.
 * - @b jwt (ores::security::jwt): JWT signing and validation over jwt-cpp.
 *   Both validation paths constrain the algorithm, the issuer, the audience
 *   and the expiry, and report which of them failed through jwt_error.
 * - @b validation (ores::security::validation): the OWASP password policy and
 *   the email shape, each returning a validation_result.
 *
 * Every service validates the tokens IAM issues, which is why the JWT
 * authenticator is the component's widest surface. Nothing here is generated,
 * and nothing but a signed token crosses a wire.
 */
namespace ores::security {}

#endif
