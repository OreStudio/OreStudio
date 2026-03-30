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
#ifndef ORES_WORKFLOW_MESSAGING_REGISTRAR_HPP
#define ORES_WORKFLOW_MESSAGING_REGISTRAR_HPP

#include <vector>
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"

namespace ores::workflow::messaging {

/**
 * @brief Registers all workflow NATS message handlers.
 *
 * @param nats          Raw NATS client for subscriptions and replies.
 * @param ctx           Base database context (connection pool + service account).
 * @param signer        JWT authenticator for validating inbound bearer tokens.
 * @param outbound_nats Authenticated NATS client used for outgoing service calls.
 *                      Must be constructed with a service-level token_provider.
 */
class registrar {
public:
    static std::vector<ores::nats::service::subscription>
    register_handlers(ores::nats::service::client& nats,
        ores::database::context ctx,
        ores::security::jwt::jwt_authenticator signer,
        ores::nats::service::nats_client outbound_nats);
};

}

#endif
