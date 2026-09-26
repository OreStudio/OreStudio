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
#pragma once

#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.scheduler.core/export.hpp"
#include "ores.scheduler.core/service/action_handler.hpp"

namespace ores::scheduler::service {

/**
 * @brief Fires a NATS message on each job firing.
 *
 * Handles jobs with action_type == "nats_publish". The action_payload JSON
 * names the subject, and may name a report definition and its tenant.
 *
 * Two kinds of job use this action and they need different treatment.
 *
 * A job that names a report definition is asking the reporting service to run
 * a report, and that service answers. The trigger is sent as an authenticated
 * request, with the scheduler's own service account, so the reporting service
 * can name the caller and so a refusal comes back as a job failure rather than
 * being lost. The body is the report instance trigger request, and its field
 * order and types are the reporting operation's.
 *
 * Any other job is a fire-and-forget notification, such as the stale-result
 * reaper. It is published, with no reply read, because nothing answers it.
 */
class ORES_SCHEDULER_CORE_EXPORT nats_publish_action_handler final : public action_handler {
public:
    nats_publish_action_handler(ores::nats::service::client& nats,
                                ores::nats::service::nats_client& svc_nats);

    [[nodiscard]] std::string_view action_type() const noexcept override {
        return "nats_publish";
    }

    boost::asio::awaitable<std::expected<void, std::string>>
    execute(const action_context& ctx) override;

private:
    ores::nats::service::client& nats_;
    ores::nats::service::nats_client& svc_nats_;
};

} // namespace ores::scheduler::service
