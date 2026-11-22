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
#ifndef ORES_IAM_API_DOMAIN_AGENT_KEY_FORMAT_HPP
#define ORES_IAM_API_DOMAIN_AGENT_KEY_FORMAT_HPP

#include "ores.iam.api/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief The prefix every agent key begins with, so the HTTP edge can tell an
 * agent's key from a token before it reads anything else.
 */
inline constexpr std::string_view agent_key_prefix = "ores_ak_";

/**
 * @brief The three parts of an agent key.
 *
 * A key is ores_ak_<tenant>_<key id>_<secret>: the tenant and the key id are
 * thirty-two hexadecimal digits each and are not secret; the secret is sixty-four
 * hexadecimal digits, 256 random bits, and is the only part that proves anything.
 */
struct agent_key_parts final {
    boost::uuids::uuid tenant_id{};
    boost::uuids::uuid key_id{};
    std::string secret;
};

/**
 * @brief Whether a bearer value is an agent key rather than a token.
 */
ORES_IAM_API_EXPORT bool is_agent_key(std::string_view value);

/**
 * @brief The parts of a well-formed agent key, or nothing.
 *
 * Checks only the form. Whether the key exists, and whether its secret is
 * right, is IAM's to decide at the exchange.
 */
ORES_IAM_API_EXPORT std::optional<agent_key_parts> parse_agent_key(std::string_view value);

/**
 * @brief Writes a key from its parts.
 */
ORES_IAM_API_EXPORT std::string format_agent_key(const agent_key_parts& parts);

}

#endif
