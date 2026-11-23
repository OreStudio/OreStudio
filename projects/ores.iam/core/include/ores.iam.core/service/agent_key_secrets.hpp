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
#ifndef ORES_IAM_CORE_SERVICE_AGENT_KEY_SECRETS_HPP
#define ORES_IAM_CORE_SERVICE_AGENT_KEY_SECRETS_HPP

#include "ores.iam.core/export.hpp"
#include <string>
#include <string_view>

namespace ores::iam::service {

/**
 * @brief A new key secret: 256 bits from the operating system's secure random
 * source, as sixty-four lower-case hexadecimal digits.
 *
 * Throws when the random source fails, so no key is ever made from less.
 */
ORES_IAM_CORE_EXPORT std::string generate_agent_key_secret();

/**
 * @brief The SHA-256 of a secret, in hexadecimal: the only form IAM stores.
 *
 * A fast hash suffices because the secret is a long random value, not a
 * password a person chose.
 */
ORES_IAM_CORE_EXPORT std::string hash_agent_key_secret(std::string_view secret);

/**
 * @brief Whether a presented secret hashes to the stored hash, compared in
 * constant time so the comparison reveals nothing of the stored value.
 */
ORES_IAM_CORE_EXPORT bool agent_key_secret_matches(std::string_view secret,
                                                   std::string_view stored_hash);

}

#endif
