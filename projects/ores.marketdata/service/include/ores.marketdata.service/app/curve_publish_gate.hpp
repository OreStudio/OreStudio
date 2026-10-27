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
#ifndef ORES_MARKETDATA_SERVICE_APP_CURVE_PUBLISH_GATE_HPP
#define ORES_MARKETDATA_SERVICE_APP_CURVE_PUBLISH_GATE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/domain/approval_request.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.marketdata.service/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>

namespace ores::marketdata::service::app {

/**
 * @brief The kind of approval request that permits a curve publish.
 *
 * A publish is a request and not a save: the trader who owns the curve asks,
 * a person holding the kind's decider permission — Market Risk — answers, and
 * only an approved request publishes. The kind is seeded in
 * projects/ores.sql/populate/inbox/inbox_approval_kinds_populate.sql.
 */
inline constexpr std::string_view curve_publish_kind_code = "marketdata.curve_publish";

/**
 * @brief What the approval record says about one publish.
 *
 * approved says the publish may go ahead. state_code is the request's state as
 * the record holds it, so a caller reads waiting or refused rather than
 * guessing, and it is empty when no request was named at all. message says why
 * a publish may not go ahead, in the words the person reads.
 */
struct publish_verdict {
    bool approved = false;
    std::string state_code;
    std::string message;
};

/**
 * @brief Asks the approval record whether a publish may go ahead.
 *
 * The request must be a @c marketdata.curve_publish request of the caller's
 * tenant, and it must be approved. Everything else is a refusal that names the
 * state the request is in, or the kind it is of, so a person who names the
 * wrong request is told which one to name.
 */
inline publish_verdict verdict_on_publish(const ores::database::context& ctx,
                                          const std::string& request_id) {
    if (request_id.empty()) {
        return {.approved = false,
                .state_code = {},
                .message = "A publish needs an approved request of the kind " +
                           std::string(curve_publish_kind_code) +
                           ". Raise one, have it decided, then publish with its id."};
    }

    std::optional<ores::inbox::domain::approval_request> request;
    try {
        ores::inbox::service::approval_lifecycle lifecycle(ctx);
        request = lifecycle.request(request_id);
    } catch (const std::exception&) {
        // An id that is not a uuid is not a request. The person named
        // something the record cannot hold, which is a refusal and not a
        // failure of the service.
        request = std::nullopt;
    }

    if (!request) {
        return {.approved = false,
                .state_code = {},
                .message = "No approval request has the id " + request_id + "."};
    }

    if (request->kind_code != curve_publish_kind_code) {
        return {.approved = false,
                .state_code = request->state_code,
                .message = "The request is a " + request->kind_code +
                           " request, and a publish is permitted by a " +
                           std::string(curve_publish_kind_code) + " request."};
    }

    if (request->state_code != "approved") {
        return {.approved = false,
                .state_code = request->state_code,
                .message = "The request is " + request->state_code +
                           ", and a publish waits until it is approved."};
    }

    return {.approved = true, .state_code = request->state_code, .message = {}};
}

/**
 * @brief Raises the request that a publish waits on, for the signed-in person.
 *
 * The person asking to publish is the person the record names as the asker, so
 * four-eyes holds at the record: whoever decides it must be somebody else.
 */
inline ores::inbox::domain::approval_request
raise_publish_request(const ores::database::context& ctx, const std::string& reason) {
    ores::inbox::service::approval_lifecycle lifecycle(ctx);
    const auto kind = lifecycle.kind(std::string(curve_publish_kind_code));
    if (!kind)
        throw std::runtime_error("No such kind of request: " +
                                 std::string(curve_publish_kind_code) + ".");
    const auto me = lifecycle.actor_account_id();
    if (!me)
        throw std::runtime_error("The signed-in account was not found.");
    return lifecycle.raise(*kind, reason, *me);
}

}

#endif
