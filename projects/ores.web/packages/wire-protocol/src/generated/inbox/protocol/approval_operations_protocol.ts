/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
import type { ApprovalRequest } from '../domain/approval_request.js';
import type { Result } from '../../../utility/protocol.js';

/**
 * @brief Asks for something that a person who may decide it must approve.
 *
 * The request is raised for the signed-in person, waiting, and expiring when
 * its kind says. The component that owns the kind writes the detail that names
 * what is asked for, keyed by the request's id.
 */
export interface RaiseApprovalRequestRequest {
    /**
     * @brief The kind of request, such as iam.role_grant.
     */
    kind_code: string;
    /**
     * @brief Why the person asks, in their words.
     */
    reason: string;
}

export interface RaiseApprovalRequestResponse {
    result: Result;
    /**
     * @brief The request as raised, when the outcome is ok.
     */
    request: ApprovalRequest;
}

/**
 * @brief Takes back a request the signed-in person raised.
 *
 * Only an open request can be withdrawn, and only by the person who asked.
 */
export interface WithdrawApprovalRequestRequest {
    request_id: string;
    /**
     * @brief The version of the request the person saw.
     */
    version: number;
    comment: string;
}

export interface WithdrawApprovalRequestResponse {
    result: Result;
    request: ApprovalRequest;
}

/**
 * @brief Approves, refuses, holds or resumes a request.
 *
 * The decider needs the permission the request's kind names, and is not the
 * person who asked. The request moves to the state the decisions reach: an
 * approval closes it only once the kind's approvals_required approvals stand.
 */
export interface DecideApprovalRequestRequest {
    request_id: string;
    /**
     * @brief The version of the request the decider saw, so a second decision
     * on a request someone else has just decided is refused.
     */
    version: number;
    /**
     * @brief approve, refuse, hold or resume.
     */
    decision_code: string;
    comment: string;
}

export interface DecideApprovalRequestResponse {
    result: Result;
    /**
     * @brief The request after the decision, when the outcome is ok.
     */
    request: ApprovalRequest;
}

/**
 * @brief Reads the open requests the signed-in person may decide.
 *
 * Open means waiting or held. A request is in the queue when the person holds
 * its kind's decide permission and did not raise it. Oldest first.
 */
export interface ListApprovalQueueRequest {
    offset: number;
    limit: number;
}

export interface ListApprovalQueueResponse {
    result: Result;
    requests: ApprovalRequest[];
    /**
     * @brief How many requests the whole queue holds, for paging.
     */
    total: number;
}

/**
 * @brief Reads the requests the signed-in person raised, newest first.
 */
export interface ListMyApprovalRequestsRequest {
    offset: number;
    limit: number;
}

export interface ListMyApprovalRequestsResponse {
    result: Result;
    requests: ApprovalRequest[];
    total: number;
}

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
export interface GetApprovalRequest {
    request_id: string;
}

export interface GetApprovalResponse {
    result: Result;
    /**
     * @brief The request, present when the caller may open it.
     *
     * Absent means one thing to the caller and two things to the store: no such
     * request, or a request this caller may not see. Telling them apart would
     * tell a stranger that a request exists.
     */
    request: ApprovalRequest | null;
}

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
export interface ExpireOverdueApprovalsRequest {}

export interface ExpireOverdueApprovalsResponse {
    result: Result;
    /**
     * @brief The requests that closed, as UUID strings, oldest deadline first.
     */
    expired: string[];
}

export const subjects = {
    raise_approval_request_request: 'inbox.v1.ops.raise_approval',
    withdraw_approval_request_request: 'inbox.v1.ops.withdraw_approval',
    decide_approval_request_request: 'inbox.v1.ops.decide_approval',
    list_approval_queue_request: 'inbox.v1.ops.list_approval_queue',
    list_my_approval_requests_request: 'inbox.v1.ops.list_my_approval_requests',
    get_approval_request: 'inbox.v1.ops.get_approval',
    expire_overdue_approvals_request: 'inbox.v1.ops.expire_overdue_approvals',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    raise_approval_request_request: true,
    withdraw_approval_request_request: true,
    decide_approval_request_request: true,
    list_approval_queue_request: true,
    list_my_approval_requests_request: true,
    get_approval_request: true,
    expire_overdue_approvals_request: false,
} as const;
