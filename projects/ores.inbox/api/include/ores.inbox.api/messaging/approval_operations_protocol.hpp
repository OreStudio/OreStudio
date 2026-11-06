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
#include "ores.inbox.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <chrono>
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
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
    /**
     * @brief The parts whose approval the request needs.
     *
     * Empty for a kind that names one decider permission and a count. When parts
     * are named, the request is approved once every part has approved, in the
     * answer order the parts give.
     */
    std::vector<std::string> part_codes;

    friend bool operator==(const raise_approval_request_request&,
                           const raise_approval_request_request&) = default;
};

struct raise_approval_request_response {
    ores::utility::domain::result result;
    /**
     * @brief The request as raised, when the outcome is ok.
     */
    ores::inbox::domain::approval_request request;

    friend bool operator==(const raise_approval_request_response&,
                           const raise_approval_request_response&) = default;
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

    friend bool operator==(const withdraw_approval_request_request&,
                           const withdraw_approval_request_request&) = default;
};

struct withdraw_approval_request_response {
    ores::utility::domain::result result;
    ores::inbox::domain::approval_request request;

    friend bool operator==(const withdraw_approval_request_response&,
                           const withdraw_approval_request_response&) = default;
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
    /**
     * @brief The part the decider answers for.
     *
     * Required to approve or refuse a request that names parts. The decider needs
     * the permission that part names.
     */
    std::string part_code;
    /**
     * @brief Why the request is parked, when the decision is a hold.
     *
     * One of the codes the hold reason lookup holds. A hold that names none, or a
     * code the lookup does not hold, is refused with the codes it accepts, so a
     * held request always says what it waits for in words a report can group by.
     * Empty on every decision that is not a hold.
     */
    std::string hold_reason_code = "";

    friend bool operator==(const decide_approval_request_request&,
                           const decide_approval_request_request&) = default;
};

struct decide_approval_request_response {
    ores::utility::domain::result result;
    /**
     * @brief The request after the decision, when the outcome is ok.
     */
    ores::inbox::domain::approval_request request;

    friend bool operator==(const decide_approval_request_response&,
                           const decide_approval_request_response&) = default;
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

    friend bool operator==(const list_approval_queue_request&,
                           const list_approval_queue_request&) = default;
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

    friend bool operator==(const list_approval_queue_response&,
                           const list_approval_queue_response&) = default;
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

    friend bool operator==(const list_my_approval_requests_request&,
                           const list_my_approval_requests_request&) = default;
};

struct list_my_approval_requests_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::approval_request> requests;
    int total = 0;

    friend bool operator==(const list_my_approval_requests_response&,
                           const list_my_approval_requests_response&) = default;
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

    friend bool operator==(const get_approval_request&, const get_approval_request&) = default;
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

    friend bool operator==(const get_approval_response&, const get_approval_response&) = default;
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

    friend bool operator==(const expire_overdue_approvals_request&,
                           const expire_overdue_approvals_request&) = default;
};

struct expire_overdue_approvals_response {
    ores::utility::domain::result result;
    /**
     * @brief The requests that closed, as UUID strings, oldest deadline first.
     */
    std::vector<std::string> expired;

    friend bool operator==(const expire_overdue_approvals_response&,
                           const expire_overdue_approvals_response&) = default;
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

    friend bool operator==(const remind_expiring_approvals_request&,
                           const remind_expiring_approvals_request&) = default;
};

struct remind_expiring_approvals_response {
    ores::utility::domain::result result;
    /**
     * @brief The requests complained about, as UUID strings, nearest deadline
     * first.
     */
    std::vector<std::string> reminded;

    friend bool operator==(const remind_expiring_approvals_response&,
                           const remind_expiring_approvals_response&) = default;
};

/**
 * @brief Resumes the applies that stopped, and ends the ones that ran out of
 * time.
 *
 * An approved request whose kind names an apply workflow stays approved until
 * the workflow applies it. When the run stopped on a failure that can pass, or
 * the start was lost, nothing moves it on. This finds those requests, starts
 * the run again (a repeated start names the same run, so it changes nothing when
 * the run exists) and asks the engine to resume a stopped run. A run that
 * stopped on a refusal never reaches here: the refusal moves its request to
 * apply_failed.
 *
 * When the kind's window ends, the request moves to apply_failed with the
 * reason "could not apply", and the person who asked is told, so a request never
 * retries forever.
 *
 * A scheduler firing is a plain publish with no token, so this carries no
 * session: it takes no input, and it only acts on requests that are already
 * approved.
 */
struct retry_stalled_applies_request {
    using response_type = struct retry_stalled_applies_response;
    static constexpr std::string_view nats_subject = "inbox.v1.ops.retry_stalled_applies";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;

    friend bool operator==(const retry_stalled_applies_request&,
                           const retry_stalled_applies_request&) = default;
};

struct retry_stalled_applies_response {
    ores::utility::domain::result result;
    /**
     * @brief The requests whose stopped run the engine resumed, as UUID strings.
     */
    std::vector<std::string> resumed;
    /**
     * @brief The requests that moved to apply_failed because the window ended, as
     * UUID strings.
     */
    std::vector<std::string> gave_up;

    friend bool operator==(const retry_stalled_applies_response&,
                           const retry_stalled_applies_response&) = default;
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

    friend bool operator==(const approval_request_field&, const approval_request_field&) = default;
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

    friend bool operator==(const approval_request_version&,
                           const approval_request_version&) = default;
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

    friend bool operator==(const get_approval_history_request&,
                           const get_approval_history_request&) = default;
};

struct get_approval_history_response {
    ores::utility::domain::result result;
    std::vector<approval_request_version> versions;

    friend bool operator==(const get_approval_history_response&,
                           const get_approval_history_response&) = default;
};

/**
 * @brief Where one part of a request stands.
 *
 * A request that needs several functions to approve lists one of these for each.
 * The state says what a person reading the request needs to know: whether the
 * part has answered, whether its turn has come, or whether an earlier part
 * must answer first.
 */
struct approval_part_progress {
    std::string code;
    std::string name;
    int answer_order = 0;
    /**
     * @brief approved, refused, waiting (its turn has come) or later (an earlier part
     * must answer first).
     */
    std::string state;
    /**
     * @brief The account that answered the part, as a UUID string, or empty.
     */
    std::string decided_by;
    std::optional<std::chrono::system_clock::time_point> decided_at;
    std::string comment;

    friend bool operator==(const approval_part_progress&, const approval_part_progress&) = default;
};

/**
 * @brief Reads where each part of one request stands.
 *
 * Answered when the caller raised the request, and when the caller may decide
 * a request of its kind. Answered as not found otherwise, as opening the
 * request is. The person who asked is the one who waits on the parts, so the
 * entity read of the parts, which gates on the administrator's permission,
 * cannot serve them.
 */
struct get_approval_parts_request {
    using response_type = struct get_approval_parts_response;
    static constexpr std::string_view nats_subject = "inbox.v1.ops.get_approval_parts";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string request_id;

    friend bool operator==(const get_approval_parts_request&,
                           const get_approval_parts_request&) = default;
};

struct get_approval_parts_response {
    ores::utility::domain::result result;
    /**
     * @brief The parts in the order they answer in. Empty for a request that names
     * no parts.
     */
    std::vector<approval_part_progress> parts;

    friend bool operator==(const get_approval_parts_response&,
                           const get_approval_parts_response&) = default;
};

/**
 * @brief The members of raise_approval_request_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<raise_approval_request_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"kind_code", &raise_approval_request_request::kind_code},
        ::ores::utility::codec::field{"reason", &raise_approval_request_request::reason},
        ::ores::utility::codec::field{"part_codes", &raise_approval_request_request::part_codes},
    };
}

/**
 * @brief The members of raise_approval_request_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<raise_approval_request_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &raise_approval_request_response::result},
        ::ores::utility::codec::field{"request", &raise_approval_request_response::request},
    };
}

/**
 * @brief The members of withdraw_approval_request_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<withdraw_approval_request_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"request_id", &withdraw_approval_request_request::request_id},
        ::ores::utility::codec::field{"version", &withdraw_approval_request_request::version},
        ::ores::utility::codec::field{"comment", &withdraw_approval_request_request::comment},
    };
}

/**
 * @brief The members of withdraw_approval_request_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<withdraw_approval_request_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &withdraw_approval_request_response::result},
        ::ores::utility::codec::field{"request", &withdraw_approval_request_response::request},
    };
}

/**
 * @brief The members of decide_approval_request_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<decide_approval_request_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"request_id", &decide_approval_request_request::request_id},
        ::ores::utility::codec::field{"version", &decide_approval_request_request::version},
        ::ores::utility::codec::field{"decision_code",
                                      &decide_approval_request_request::decision_code},
        ::ores::utility::codec::field{"comment", &decide_approval_request_request::comment},
        ::ores::utility::codec::field{"part_code", &decide_approval_request_request::part_code},
        ::ores::utility::codec::field{"hold_reason_code",
                                      &decide_approval_request_request::hold_reason_code},
    };
}

/**
 * @brief The members of decide_approval_request_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<decide_approval_request_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &decide_approval_request_response::result},
        ::ores::utility::codec::field{"request", &decide_approval_request_response::request},
    };
}

/**
 * @brief The members of list_approval_queue_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_approval_queue_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"offset", &list_approval_queue_request::offset},
        ::ores::utility::codec::field{"limit", &list_approval_queue_request::limit},
    };
}

/**
 * @brief The members of list_approval_queue_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_approval_queue_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &list_approval_queue_response::result},
        ::ores::utility::codec::field{"requests", &list_approval_queue_response::requests},
        ::ores::utility::codec::field{"total", &list_approval_queue_response::total},
        ::ores::utility::codec::field{"answered", &list_approval_queue_response::answered},
    };
}

/**
 * @brief The members of list_my_approval_requests_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_my_approval_requests_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"offset", &list_my_approval_requests_request::offset},
        ::ores::utility::codec::field{"limit", &list_my_approval_requests_request::limit},
    };
}

/**
 * @brief The members of list_my_approval_requests_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_my_approval_requests_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &list_my_approval_requests_response::result},
        ::ores::utility::codec::field{"requests", &list_my_approval_requests_response::requests},
        ::ores::utility::codec::field{"total", &list_my_approval_requests_response::total},
    };
}

/**
 * @brief The members of get_approval_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_approval_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"request_id", &get_approval_request::request_id},
    };
}

/**
 * @brief The members of get_approval_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_approval_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &get_approval_response::result},
        ::ores::utility::codec::field{"request", &get_approval_response::request},
    };
}

/**
 * @brief The members of expire_overdue_approvals_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<expire_overdue_approvals_request>) {
    return std::tuple{};
}

/**
 * @brief The members of expire_overdue_approvals_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<expire_overdue_approvals_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &expire_overdue_approvals_response::result},
        ::ores::utility::codec::field{"expired", &expire_overdue_approvals_response::expired},
    };
}

/**
 * @brief The members of remind_expiring_approvals_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<remind_expiring_approvals_request>) {
    return std::tuple{};
}

/**
 * @brief The members of remind_expiring_approvals_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<remind_expiring_approvals_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &remind_expiring_approvals_response::result},
        ::ores::utility::codec::field{"reminded", &remind_expiring_approvals_response::reminded},
    };
}

/**
 * @brief The members of retry_stalled_applies_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retry_stalled_applies_request>) {
    return std::tuple{};
}

/**
 * @brief The members of retry_stalled_applies_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retry_stalled_applies_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &retry_stalled_applies_response::result},
        ::ores::utility::codec::field{"resumed", &retry_stalled_applies_response::resumed},
        ::ores::utility::codec::field{"gave_up", &retry_stalled_applies_response::gave_up},
    };
}

/**
 * @brief The members of approval_request_field, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<approval_request_field>) {
    return std::tuple{
        ::ores::utility::codec::field{"name", &approval_request_field::name},
        ::ores::utility::codec::field{"value", &approval_request_field::value},
    };
}

/**
 * @brief The members of approval_request_version, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<approval_request_version>) {
    return std::tuple{
        ::ores::utility::codec::field{"version", &approval_request_version::version},
        ::ores::utility::codec::field{"modified_by", &approval_request_version::modified_by},
        ::ores::utility::codec::field{"performed_by", &approval_request_version::performed_by},
        ::ores::utility::codec::field{"recorded_at", &approval_request_version::recorded_at},
        ::ores::utility::codec::field{"change_reason_code",
                                      &approval_request_version::change_reason_code},
        ::ores::utility::codec::field{"change_commentary",
                                      &approval_request_version::change_commentary},
        ::ores::utility::codec::field{"fields", &approval_request_version::fields},
    };
}

/**
 * @brief The members of get_approval_history_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_approval_history_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"request_id", &get_approval_history_request::request_id},
    };
}

/**
 * @brief The members of get_approval_history_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_approval_history_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &get_approval_history_response::result},
        ::ores::utility::codec::field{"versions", &get_approval_history_response::versions},
    };
}

/**
 * @brief The members of approval_part_progress, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<approval_part_progress>) {
    return std::tuple{
        ::ores::utility::codec::field{"code", &approval_part_progress::code},
        ::ores::utility::codec::field{"name", &approval_part_progress::name},
        ::ores::utility::codec::field{"answer_order", &approval_part_progress::answer_order},
        ::ores::utility::codec::field{"state", &approval_part_progress::state},
        ::ores::utility::codec::field{"decided_by", &approval_part_progress::decided_by},
        ::ores::utility::codec::field{"decided_at", &approval_part_progress::decided_at},
        ::ores::utility::codec::field{"comment", &approval_part_progress::comment},
    };
}

/**
 * @brief The members of get_approval_parts_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_approval_parts_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"request_id", &get_approval_parts_request::request_id},
    };
}

/**
 * @brief The members of get_approval_parts_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_approval_parts_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &get_approval_parts_response::result},
        ::ores::utility::codec::field{"parts", &get_approval_parts_response::parts},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::raise_approval_request_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::raise_approval_request_request& v);
    static ores::inbox::messaging::raise_approval_request_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::raise_approval_request_request& v);
    static ores::inbox::messaging::raise_approval_request_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::raise_approval_request_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::raise_approval_request_response& v);
    static ores::inbox::messaging::raise_approval_request_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::raise_approval_request_response& v);
    static ores::inbox::messaging::raise_approval_request_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::withdraw_approval_request_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::withdraw_approval_request_request& v);
    static ores::inbox::messaging::withdraw_approval_request_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::withdraw_approval_request_request& v);
    static ores::inbox::messaging::withdraw_approval_request_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::withdraw_approval_request_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::withdraw_approval_request_response& v);
    static ores::inbox::messaging::withdraw_approval_request_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::withdraw_approval_request_response& v);
    static ores::inbox::messaging::withdraw_approval_request_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::decide_approval_request_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::decide_approval_request_request& v);
    static ores::inbox::messaging::decide_approval_request_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::decide_approval_request_request& v);
    static ores::inbox::messaging::decide_approval_request_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::decide_approval_request_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::decide_approval_request_response& v);
    static ores::inbox::messaging::decide_approval_request_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::decide_approval_request_response& v);
    static ores::inbox::messaging::decide_approval_request_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::list_approval_queue_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::list_approval_queue_request& v);
    static ores::inbox::messaging::list_approval_queue_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::list_approval_queue_request& v);
    static ores::inbox::messaging::list_approval_queue_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::list_approval_queue_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::list_approval_queue_response& v);
    static ores::inbox::messaging::list_approval_queue_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::list_approval_queue_response& v);
    static ores::inbox::messaging::list_approval_queue_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_my_approval_requests_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_my_approval_requests_request& v);
    static ores::inbox::messaging::list_my_approval_requests_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_my_approval_requests_request& v);
    static ores::inbox::messaging::list_my_approval_requests_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_my_approval_requests_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_my_approval_requests_response& v);
    static ores::inbox::messaging::list_my_approval_requests_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_my_approval_requests_response& v);
    static ores::inbox::messaging::list_my_approval_requests_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_approval_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::get_approval_request& v);
    static ores::inbox::messaging::get_approval_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::get_approval_request& v);
    static ores::inbox::messaging::get_approval_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_approval_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::get_approval_response& v);
    static ores::inbox::messaging::get_approval_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::get_approval_response& v);
    static ores::inbox::messaging::get_approval_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::expire_overdue_approvals_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::expire_overdue_approvals_request& v);
    static ores::inbox::messaging::expire_overdue_approvals_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::expire_overdue_approvals_request& v);
    static ores::inbox::messaging::expire_overdue_approvals_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::expire_overdue_approvals_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::expire_overdue_approvals_response& v);
    static ores::inbox::messaging::expire_overdue_approvals_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::expire_overdue_approvals_response& v);
    static ores::inbox::messaging::expire_overdue_approvals_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::remind_expiring_approvals_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::remind_expiring_approvals_request& v);
    static ores::inbox::messaging::remind_expiring_approvals_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::remind_expiring_approvals_request& v);
    static ores::inbox::messaging::remind_expiring_approvals_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::remind_expiring_approvals_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::remind_expiring_approvals_response& v);
    static ores::inbox::messaging::remind_expiring_approvals_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::remind_expiring_approvals_response& v);
    static ores::inbox::messaging::remind_expiring_approvals_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::retry_stalled_applies_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::retry_stalled_applies_request& v);
    static ores::inbox::messaging::retry_stalled_applies_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::retry_stalled_applies_request& v);
    static ores::inbox::messaging::retry_stalled_applies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::retry_stalled_applies_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::retry_stalled_applies_response& v);
    static ores::inbox::messaging::retry_stalled_applies_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::retry_stalled_applies_response& v);
    static ores::inbox::messaging::retry_stalled_applies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::approval_request_field> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::approval_request_field& v);
    static ores::inbox::messaging::approval_request_field read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::approval_request_field& v);
    static ores::inbox::messaging::approval_request_field
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::approval_request_version> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::approval_request_version& v);
    static ores::inbox::messaging::approval_request_version read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::approval_request_version& v);
    static ores::inbox::messaging::approval_request_version
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_approval_history_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::get_approval_history_request& v);
    static ores::inbox::messaging::get_approval_history_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::get_approval_history_request& v);
    static ores::inbox::messaging::get_approval_history_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_approval_history_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::get_approval_history_response& v);
    static ores::inbox::messaging::get_approval_history_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_approval_history_response& v);
    static ores::inbox::messaging::get_approval_history_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::approval_part_progress> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::approval_part_progress& v);
    static ores::inbox::messaging::approval_part_progress read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::approval_part_progress& v);
    static ores::inbox::messaging::approval_part_progress
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_approval_parts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::get_approval_parts_request& v);
    static ores::inbox::messaging::get_approval_parts_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::get_approval_parts_request& v);
    static ores::inbox::messaging::get_approval_parts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_approval_parts_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::get_approval_parts_response& v);
    static ores::inbox::messaging::get_approval_parts_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::get_approval_parts_response& v);
    static ores::inbox::messaging::get_approval_parts_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
