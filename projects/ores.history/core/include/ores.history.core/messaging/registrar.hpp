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
#ifndef ORES_HISTORY_MESSAGING_REGISTRAR_HPP
#define ORES_HISTORY_MESSAGING_REGISTRAR_HPP

#include "ores.database/domain/context.hpp"
#include "ores.history.core/export.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <optional>

namespace ores::history::messaging {

/**
 * @brief Subscribes the one generic history subject (history.v1.get) to a
 * history_handler backed by @p registry, under the given queue group.
 *
 * @p ctx and @p verifier are the same service-level context/JWT
 * verifier every other per-entity registrar in the composing service
 * receives — each request is resolved into a scoped context via
 * make_request_context() exactly like any other subject.
 *
 * Call once per service process, after registering that service's own
 * entities' providers on @p registry. Owns no state itself: registry must
 * outlive the returned subscription.
 */
ORES_HISTORY_CORE_EXPORT ores::nats::service::subscription
register_history_handlers(ores::nats::service::client& nats,
                          const service::dispatch_registry& registry,
                          std::string_view queue_group,
                          ores::database::context ctx,
                          std::optional<ores::security::jwt::jwt_authenticator> verifier);

}

#endif
