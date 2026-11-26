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
#ifndef ORES_VAULT_API_MESSAGING_CUSTODY_CONTEXT_HPP
#define ORES_VAULT_API_MESSAGING_CUSTODY_CONTEXT_HPP

#include "ores.vault.api/export.hpp"
#include <string>

namespace ores::vault::messaging {

/**
 * @brief The contexts a sealed value is authenticated against.
 *
 * Both sides of a hop must build the same string, so it is stated once, in the
 * api part every caller already depends on: the vault's own service and the
 * gateway that seals on its behalf.
 */
struct ORES_VAULT_API_EXPORT custody_context {
    /**
     * @brief The context a caller seals a write under.
     *
     * It names the secret and its owner, so a captured write cannot be stored
     * under another name or another owner.
     */
    [[nodiscard]] static std::string write(const std::string& name, const std::string& owner);

    /**
     * @brief The context the vault seals a delivered value under.
     *
     * It names the consumer, the secret and the version, so a captured reply
     * replayed to another consumer or another secret does not open.
     */
    [[nodiscard]] static std::string
    delivery(const std::string& subject, const std::string& name, int key_version);
};

}

#endif
