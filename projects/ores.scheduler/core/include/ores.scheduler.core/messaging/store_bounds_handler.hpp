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
#ifndef ORES_SCHEDULER_CORE_MESSAGING_STORE_BOUNDS_HANDLER_HPP
#define ORES_SCHEDULER_CORE_MESSAGING_STORE_BOUNDS_HANDLER_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.scheduler.api/messaging/scheduling_operations_protocol.hpp"
#include "ores.scheduler.core/export.hpp"
#include "ores.scheduler.core/repository/store_bounds_repository.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <optional>

namespace ores::scheduler::messaging {

/**
 * @brief Serves the housekeeping reads: every store's bound, and a store's series.
 *
 * Both are computed from state three tables hold, which is why they are written
 * here rather than generated from an entity: the answers are the database's, and
 * the screen that reads them must not reimplement the rule that decides whether a
 * store is bounded.
 */
class ORES_SCHEDULER_CORE_EXPORT store_bounds_handler {
public:
    store_bounds_handler(ores::nats::service::client& nats,
                         const ores::database::context& ctx,
                         std::optional<ores::security::jwt::jwt_authenticator> verifier);

    void list(ores::nats::message msg);
    void samples(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    const ores::database::context& ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
