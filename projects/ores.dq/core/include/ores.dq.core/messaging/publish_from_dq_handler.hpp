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
#ifndef ORES_DQ_CORE_MESSAGING_PUBLISH_FROM_DQ_HANDLER_HPP
#define ORES_DQ_CORE_MESSAGING_PUBLISH_FROM_DQ_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"

namespace ores::dq::messaging {

/**
 * @brief Handles DQ-internal publish-from-dq workflow step commands.
 *
 * Handles dq.v1.ip2country.publish-from-dq,
 * dq.v1.coding-schemes.publish-from-dq,
 * dq.v1.badge-severities.publish-from-dq, and
 * dq.v1.badge-definitions.publish-from-dq, which write to DQ's own tables
 * rather than another service's.
 *
 * Subject-to-function mapping:
 *   dq.v1.ip2country.publish-from-dq        -> ores_dq_ip2country_publish_fn
 *   dq.v1.coding-schemes.publish-from-dq    -> ores_dq_coding_schemes_publish_fn
 *   dq.v1.badge-severities.publish-from-dq  -> ores_dq_badge_severities_publish_fn
 *   dq.v1.badge-definitions.publish-from-dq -> ores_dq_badge_definitions_publish_fn
 */
class ORES_DQ_CORE_EXPORT publish_from_dq_handler {
public:
    publish_from_dq_handler(ores::nats::service::client& nats, ores::database::context ctx);

    void handle(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
};

}

#endif
