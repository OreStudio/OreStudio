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
#ifndef ORES_SCHEDULER_CORE_SERVICE_NATS_REQUEST_ACTION_HANDLER_HPP
#define ORES_SCHEDULER_CORE_SERVICE_NATS_REQUEST_ACTION_HANDLER_HPP

#include "ores.nats/service/nats_client.hpp"
#include "ores.scheduler.core/export.hpp"
#include "ores.scheduler.core/service/action_handler.hpp"

namespace ores::scheduler::service {

/**
 * @brief Sends an authenticated request on each job firing and reads the answer.
 *
 * Handles jobs with action_type == "nats_request". The action_payload names the
 * subject and, optionally, the body the callee's own request type takes.
 *
 * Unlike nats_publish, the trigger is a request: it travels with the scheduler's
 * own service account, so the callee validates the caller and an envelope
 * refusal reaches the job record as a failure rather than being discarded. The
 * answer's body belongs to the callee, so this action reads only the envelope;
 * a subject whose per-item outcome must decide the job's result keeps its
 * outcomes in its own log, as the curve republish sweep does.
 */
class ORES_SCHEDULER_CORE_EXPORT nats_request_action_handler final : public action_handler {
public:
    explicit nats_request_action_handler(ores::nats::service::nats_client& svc_nats);

    [[nodiscard]] std::string_view action_type() const noexcept override {
        return "nats_request";
    }

    boost::asio::awaitable<std::expected<void, std::string>>
    execute(const action_context& ctx) override;

private:
    ores::nats::service::nats_client& svc_nats_;
};

}

#endif
