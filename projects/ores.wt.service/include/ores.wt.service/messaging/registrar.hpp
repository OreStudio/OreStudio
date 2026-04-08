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
#ifndef ORES_WT_SERVICE_MESSAGING_REGISTRAR_HPP
#define ORES_WT_SERVICE_MESSAGING_REGISTRAR_HPP

#include <optional>
#include <vector>
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"

namespace ores::wt::service::messaging {

class registrar {
public:
    /**
     * @brief Register NATS handlers for the Wt web service.
     *
     * Currently empty — the Wt service has no NATS subjects of its own.
     * The verifier is available for future NATS service-to-service calls
     * once the Wt UI is implemented in terms of NATS messages.
     * When handlers need DB access, use application_context::instance().
     */
    static std::vector<ores::nats::service::subscription>
    register_handlers(ores::nats::service::client& nats,
        std::optional<ores::security::jwt::jwt_authenticator> verifier = std::nullopt);
};

}

#endif
