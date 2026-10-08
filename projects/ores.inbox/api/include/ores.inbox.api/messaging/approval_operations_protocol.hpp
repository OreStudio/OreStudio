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
#include <chrono>
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
    static constexpr std::string_view nats_subject = "inbox.v1.ops.raise_approval";
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
    static constexpr std::string_view nats_subject = "inbox.v1.ops.withdraw_approval";
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
    static constexpr std::string_view nats_subject = "inbox.v1.ops.decide_approval";
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
 * @brief Reads the open requests the signed-in person may decide, and the ones
 * answered within the installation's window.
 *
 * Open means waiting or held. A request is in the queue when the person holds
 * its kind's decide permission and did not raise it. Oldest first.
 *
 * A request leaves the queue the moment it is answered, which leaves the person
 * who answered it nothing to look at: the notice that says what happened links
 * to a request the queue no longer holds. So the answer carries the requests
 * answered recently beside the open ones, newest answer first, for as long as
 * the installation says. How long is the variability system setting
 * =inbox.approval_queue.answered_window_seconds=; the read falls back to an
 * empty tail when that setting cannot be read, because the open queue is the
 * read's reason to exist and it is not.
 */
struct list_approval_queue_request {
    using response_type = struct list_approval_queue_response;
    static constexpr std::string_view nats_subject = "inbox.v1.ops.list_approval_queue";
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
     * @brief How many open requests the whole queue holds, for paging. The answered
     * tail is bounded by the window and is not part of this count.
     */
    int total = 0;
    /**
     * @brief The requests answered within the window, newest answer first. Bounded,
     * and not paged: nobody acts on them.
     */
    std::vector<ores::inbox::domain::approval_request> answered;
};

/**
 * @brief Reads the requests the signed-in person raised, newest first.
 */
struct list_my_approval_requests_request {
    using response_type = struct list_my_approval_requests_response;
    static constexpr std::string_view nats_subject = "inbox.v1.ops.list_my_approval_requests";
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

/**
 * @brief Reads the one request an identifier names.
 *
 * A notice a person is given carries the request it is about, so the notice
 * has to be able to open it. The queue cannot answer that: it holds what is
 * waiting, and a notice is usually read after the request stopped waiting.
 * The request a person may open is therefore read on its own, and the caller
 * states which request they mean rather than reading a list to find it.
 *
 * What the caller may open is the store's to decide: the person who asked,
 * and whoever may decide a request of that kind. A stranger is answered as if
 * the request did not exist, so the read tells nobody that a request they may
 * not see is there.
 */
struct get_approval_request {
    using response_type = struct get_approval_response;
    static constexpr std::string_view nats_subject = "inbox.v1.ops.get_approval";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string request_id;
};

struct get_approval_response {
    ores::utility::domain::result result;
    /**
     * @brief The request, present when the caller may open it.
     *
     * Absent means one thing to the caller and two things to the store: no such
     * request, or a request this caller may not see. Telling them apart would
     * tell a stranger that a request exists.
     */
    std::optional<ores::inbox::domain::approval_request> request;
};

/**
 * @brief Closes every open request past its kind's deadline, across every
 * tenant.
 *
 * The scheduler fires this, so it acts as the service rather than as a person
 * and reaches requests no tenant-scoped caller could read. Each person who
 * asked is told; a request already closed is not open, so a repeated call
 * changes nothing.
 *
 * A scheduler firing is a plain publish with no token, so this carries no
 * session: it is trusted at the transport, as the compute reaper is. That is
 * affordable here because it takes no input and closes only what is already
 * past its deadline, which a caller could reach by waiting.
 */
struct expire_overdue_approvals_request {
    using response_type = struct expire_overdue_approvals_response;
    static constexpr std::string_view nats_subject = "inbox.v1.ops.expire_overdue_approvals";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
};

struct expire_overdue_approvals_response {
    ores::utility::domain::result result;
    /**
     * @brief The requests that closed, as UUID strings, oldest deadline first.
     */
    std::vector<std::string> expired;
};

/**
 * @brief Warns the deciders of every open request whose deadline is close.
 *
 * The other half of the deadline. The sweep that closes what nobody answered
 * keeps the queue moving, but a decider who never looked learns afterwards;
 * this gives them the chance the deadline was there to give.
 *
 * It has its own schedule and its own window, because warning somebody is a
 * nudge with its own timing and its own audience, while closing a queue that
 * has stopped moving is housekeeping. The window is the variability setting
 * =inbox.approval_expiry.reminder_window_seconds=.
 *
 * A request is warned about once, and the notice is what says so: a reminder
 * already raised names the request, so there is no second mark to keep in step
 * with the first. A request answered before its deadline passes stops matching,
 * and the one the sweep closed is left out by the close.
 *
 * A scheduler firing is a plain publish with no token, so this carries no
 * session: it takes no input, and it only reads requests that are already close
 * to a deadline their kind set.
 */
struct remind_expiring_approvals_request {
    using response_type = struct remind_expiring_approvals_response;
    static constexpr std::string_view nats_subject = "inbox.v1.ops.remind_expiring_approvals";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
};

struct remind_expiring_approvals_response {
    ores::utility::domain::result result;
    /**
     * @brief The requests complained about, as UUID strings, nearest deadline
     * first.
     */
    std::vector<std::string> reminded;
};

/**
 * @brief One field of a request at one version, as a screen names and draws it.
 *
 * The value is text whatever its type: the renderer that fills this in is the
 * same one the record screens' history panel reads, so a story and a panel
 * draw the same request the same way.
 */
struct approval_request_field {
    std::string name;
    std::string value;
};

/**
 * @brief One version of a request, with who wrote it, when, and every field it
 * held.
 *
 * The oldest version is the request as it was raised. Each later one is a
 * decision that moved it, so this is the request's own half of its story. The
 * screen pairs each version with the one before it to draw what changed.
 */
struct approval_request_version {
    int version = 0;
    /**
     * @brief The person or service that wrote this version.
     */
    std::string modified_by;
    std::string performed_by;
    /**
     * @brief When this version was written, which for an answered request is the
     * moment of the answer.
     */
    std::chrono::system_clock::time_point recorded_at;
    std::string change_reason_code;
    std::string change_commentary;
    std::vector<approval_request_field> fields;
};

/**
 * @brief Reads every version of one request, newest first.
 *
 * Answered when the caller raised the request, and when the caller may read
 * approval requests. Answered as not found otherwise, so a caller learns
 * nothing about a request they may not read.
 *
 * The entity read of approval requests and the generic history read both gate
 * on the administrator's permission, so neither can answer the person who
 * raised the request with their own request's versions. This is the read that
 * can, and it is why the story of a request is an operation rather than a
 * composition of the reads that already exist.
 */
struct get_approval_history_request {
    using response_type = struct get_approval_history_response;
    static constexpr std::string_view nats_subject = "inbox.v1.ops.get_approval_history";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string request_id;
};

struct get_approval_history_response {
    ores::utility::domain::result result;
    std::vector<approval_request_version> versions;
};

}

#endif
