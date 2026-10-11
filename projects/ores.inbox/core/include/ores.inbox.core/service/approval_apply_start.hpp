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
#ifndef ORES_INBOX_CORE_SERVICE_APPROVAL_APPLY_START_HPP
#define ORES_INBOX_CORE_SERVICE_APPROVAL_APPLY_START_HPP

#include "ores.inbox.api/domain/approval_kind.hpp"
#include "ores.inbox.api/domain/approval_request.hpp"
#include "ores.inbox.api/workflow/apply_step_command.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include <boost/uuid/name_generator_sha1.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <rfl/json.hpp>
#include <string>

namespace ores::inbox::service {

/**
 * @brief The message that starts the workflow applying an approved request, or
 * nothing when the request's kind names none.
 *
 * The request is the request's id, as every kind's apply workflow reads it. The
 * run's id comes from the request's id alone, so starting the same request twice
 * names the same run, and the engine refuses the second as a duplicate: a
 * request is applied by one run.
 */
inline std::optional<ores::workflow::messaging::start_workflow_message>
apply_start_of(const domain::approval_kind& kind,
               const domain::approval_request& request,
               const std::string& correlation_id) {
    if (!kind.apply_workflow_type || kind.apply_workflow_type->empty())
        return std::nullopt;

    // A fixed namespace, so the same request always names the same run.
    static const boost::uuids::uuid apply_namespace =
        boost::uuids::string_generator{}("4a7e0c52-9d1b-4f6a-8c33-1b5e7d90a264");
    const auto request_id = boost::uuids::to_string(request.id);

    ores::workflow::messaging::start_workflow_message start;
    start.type = *kind.apply_workflow_type;
    start.tenant_id = request.tenant_id.to_string();
    start.target_kind = "approval_request";
    start.target_id = request_id;
    start.request_json =
        rfl::json::write(ores::inbox::workflow::apply_step_command{.request_id = request_id});
    start.correlation_id = correlation_id;
    start.instance_id = boost::uuids::to_string(
        boost::uuids::name_generator_sha1{apply_namespace}("approval-apply:" + request_id));
    return start;
}

}

#endif
