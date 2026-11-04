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
#ifndef ORES_TRADING_API_DOMAIN_ECONOMIC_DIGEST_HPP
#define ORES_TRADING_API_DOMAIN_ECONOMIC_DIGEST_HPP

#include "ores.utility/crypto/sha256.hpp"
#include "ores.utility/digest/canonical_form.hpp"
#include <string>

namespace ores::trading::domain {

/**
 * @brief The SHA-256 digest of a value's economic fields, lowercase hex.
 *
 * Renders @p v canonically and hashes the result. A struct renders through
 * the =canonical_form= codegen writes beside it from the model's :economic:
 * property, so which fields count is a property of the model. Two values with
 * the same economics give the same digest; a change to a field the customer
 * did not agree to leaves the digest alone.
 *
 * @param v The instrument, trade or component value to digest.
 * @return The lowercase hex SHA-256 digest.
 */
template <typename T>
std::string economic_digest(const T& v) {
    return ores::utility::crypto::sha256::hex_digest(ores::utility::digest::canonical_value(v));
}

}

#endif
