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
#include "ores.inbox.service/app/nats_apply_engine.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include <stdexcept>
#include <string>

namespace ores::inbox::service::app {

nats_apply_engine::nats_apply_engine(ores::nats::service::client& nats,
                                     ores::nats::service::nats_client svc_nats)
    : nats_(nats)
    , svc_nats_(std::move(svc_nats)) {}

void nats_apply_engine::start(const ores::workflow::messaging::start_workflow_message& start) {
    nats_.js_publish(ores::workflow::messaging::start_workflow_message::nats_subject,
                     ores::nats::default_wire_codec().encode(start),
                     ores::nats::headers::with_publish_identity(
                         ores::nats::service::forwarded_caller_headers(svc_nats_.bearer_token()),
                         start.instance_id));
}

ores::workflow::messaging::retry_workflow_instance_response
nats_apply_engine::retry(const std::string& workflow_instance_id) {
    ores::workflow::messaging::retry_workflow_instance_request request;
    request.workflow_instance_id = workflow_instance_id;

    const auto& codec = ores::nats::default_wire_codec();
    const auto reply = svc_nats_.authenticated_request(request.nats_subject, codec.encode(request));
    if (const auto it = reply.headers.find(std::string(ores::nats::headers::x_error));
        it != reply.headers.end())
        throw std::runtime_error("the engine refused the call: " + it->second);

    auto answer =
        codec.decode<ores::workflow::messaging::retry_workflow_instance_response>(reply.data);
    if (!answer)
        throw std::runtime_error("the engine's answer could not be read");
    return *answer;
}

std::string nats_apply_engine::run_status(const std::string& workflow_instance_id) {
    ores::workflow::messaging::get_workflow_steps_request request;
    request.workflow_instance_id = workflow_instance_id;

    const auto& codec = ores::nats::default_wire_codec();
    const auto reply = svc_nats_.authenticated_request(request.nats_subject, codec.encode(request));
    if (const auto it = reply.headers.find(std::string(ores::nats::headers::x_error));
        it != reply.headers.end())
        throw std::runtime_error("the engine refused the call: " + it->second);

    auto answer = codec.decode<ores::workflow::messaging::get_workflow_steps_response>(reply.data);
    if (!answer)
        throw std::runtime_error("the engine's answer could not be read");
    return answer->success ? answer->status : std::string{};
}

}
