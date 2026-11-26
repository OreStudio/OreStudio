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
#ifndef ORES_VAULT_CORE_MESSAGING_REGISTRAR_HPP
#define ORES_VAULT_CORE_MESSAGING_REGISTRAR_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.vault.core/export.hpp"
#include <optional>
#include <vector>

namespace ores::vault::messaging {

/**
 * @brief Registers every subject the vault answers.
 *
 * The entities' generated registrars carry their CRUD subjects. This one adds
 * the five custody operations beside them, because those are not CRUD verbs and
 * no model generates them. A service that starts without its keys does not
 * reach this point: the keys are loaded here and the load refuses an absent or
 * malformed credential.
 */
struct ORES_VAULT_CORE_EXPORT registrar {
    /**
     * @brief Registers the generated CRUD subjects and the custody five.
     *
     * @param nats The connected client.
     * @param ctx The service's own context, tenant-neutral.
     * @param verifier The token verifier the service signs with.
     * @return The subscriptions, which the caller keeps alive.
     *
     * @throws std::runtime_error When the unit holds no usable key material.
     */
    [[nodiscard]] static std::vector<ores::nats::service::subscription>
    register_handlers(ores::nats::service::client& nats,
                      const ores::database::context& ctx,
                      std::optional<ores::security::jwt::jwt_authenticator> verifier);
};

}

#endif
