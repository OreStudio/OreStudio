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

export const subjects = {
    raise_approval_request_request: 'inbox.v1.approval-requests.raise',
    withdraw_approval_request_request: 'inbox.v1.approval-requests.withdraw',
    decide_approval_request_request: 'inbox.v1.approval-requests.decide',
    list_approval_queue_request: 'inbox.v1.approval-requests.queue',
    list_my_approval_requests_request: 'inbox.v1.approval-requests.mine',
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
} as const;
