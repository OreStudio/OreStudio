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
#ifndef ORES_INBOX_SERVICE_APP_NATS_APPLY_ENGINE_HPP
#define ORES_INBOX_SERVICE_APP_NATS_APPLY_ENGINE_HPP

#include "ores.inbox.core/service/approval_apply_retry.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"

namespace ores::inbox::service::app {

/**
 * @brief The workflow engine, reached over the wire with the service's own
 * token.
 *
 * The scheduled retry of an apply has no person behind it, so the run it starts
 * and the resume it asks for are the service's. The engine reaches every tenant's
 * run for a caller in the system tenant, which is where this service runs.
 */
class nats_apply_engine final : public ores::inbox::service::apply_engine {
public:
    nats_apply_engine(ores::nats::service::client& nats, ores::nats::service::nats_client svc_nats);

    void start(const ores::workflow::messaging::start_workflow_message& start) override;

    ores::workflow::messaging::retry_workflow_instance_response
    retry(const std::string& workflow_instance_id) override;

private:
    ores::nats::service::client& nats_;
    ores::nats::service::nats_client svc_nats_;
};

}

#endif
