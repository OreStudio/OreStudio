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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_INBOX_API_MESSAGING_APPROVAL_OPERATIONS_PROTOCOL_HPP
#define ORES_INBOX_API_MESSAGING_APPROVAL_OPERATIONS_PROTOCOL_HPP

#include "ores.inbox.api/domain/approval_request.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <string>
#include <vector>

namespace ores::inbox::messaging {

/**
 * @brief Asks for something that a person who may decide it must approve.
 *
 * The request is raised for the signed-in person, waiting, and expiring when
 * its kind says. The component that owns the kind writes the detail that names
 * what is asked for, keyed by the request's id.
 */
struct raise_approval_request_request {
    using response_type = struct raise_approval_request_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval-requests.raise";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The kind of request, such as iam.role_grant.
     */
    std::string kind_code;
    /**
     * @brief Why the person asks, in their words.
     */
    std::string reason;
};

struct raise_approval_request_response {
    ores::utility::domain::result result;
    /**
     * @brief The request as raised, when the outcome is ok.
     */
    ores::inbox::domain::approval_request request;
};

/**
 * @brief Takes back a request the signed-in person raised.
 *
 * Only an open request can be withdrawn, and only by the person who asked.
 */
struct withdraw_approval_request_request {
    using response_type = struct withdraw_approval_request_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval-requests.withdraw";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string request_id;
    /**
     * @brief The version of the request the person saw.
     */
    int version = 0;
    std::string comment;
};

struct withdraw_approval_request_response {
    ores::utility::domain::result result;
    ores::inbox::domain::approval_request request;
};

/**
 * @brief Approves, refuses, holds or resumes a request.
 *
 * The decider needs the permission the request's kind names, and is not the
 * person who asked. The request moves to the state the decisions reach: an
 * approval closes it only once the kind's approvals_required approvals stand.
 */
struct decide_approval_request_request {
    using response_type = struct decide_approval_request_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval-requests.decide";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string request_id;
    /**
     * @brief The version of the request the decider saw, so a second decision
     * on a request someone else has just decided is refused.
     */
    int version = 0;
    /**
     * @brief approve, refuse, hold or resume.
     */
    std::string decision_code;
    std::string comment;
};

struct decide_approval_request_response {
    ores::utility::domain::result result;
    /**
     * @brief The request after the decision, when the outcome is ok.
     */
    ores::inbox::domain::approval_request request;
};

/**
 * @brief Reads the open requests the signed-in person may decide.
 *
 * Open means waiting or held. A request is in the queue when the person holds
 * its kind's decide permission and did not raise it. Oldest first.
 */
struct list_approval_queue_request {
    using response_type = struct list_approval_queue_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval-requests.queue";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    int offset = 0;
    int limit = 100;
};

struct list_approval_queue_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::approval_request> requests;
    /**
     * @brief How many requests the whole queue holds, for paging.
     */
    int total = 0;
};

/**
 * @brief Reads the requests the signed-in person raised, newest first.
 */
struct list_my_approval_requests_request {
    using response_type = struct list_my_approval_requests_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval-requests.mine";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    int offset = 0;
    int limit = 100;
};

struct list_my_approval_requests_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::approval_request> requests;
    int total = 0;
};

}

#endif
