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
#ifndef ORES_SYNTHETIC_SERVICE_REGISTRAR_HPP
#define ORES_SYNTHETIC_SERVICE_REGISTRAR_HPP

#include "feed_controller.hpp"
#include "feed_kind_registry.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <memory>
#include <optional>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Registers NATS handlers for the synthetic feed control plane.
 *
 * Wires:
 *   synthetic.v1.ops.start_feed  — starts a feed by config_id, any registered kind (auth + RBAC)
 *   synthetic.v1.ops.stop_feed   — stops a running feed by config_id or source_name
 *   synthetic.v1.feed_configs.list   — lists running feed source_names, every kind
 *   one <kind>.simulate subject per registered kind — batch dry-run sample paths (auth + RBAC)
 *   synthetic.v1.ops.preview_ir_curve_shape  — stateless curve-shape preview (IR only; auth + RBAC)
 */
class registrar {
public:
    static std::vector<ores::nats::service::subscription>
    register_handlers(ores::nats::service::client& nats,
                      ores::nats::service::nats_client& auth_nats,
                      std::shared_ptr<feed_controller> ctrl,
                      ores::database::context ctx,
                      std::optional<ores::security::jwt::jwt_authenticator> verifier,
                      const feed_kind_registry& registry);
};

}

#endif
