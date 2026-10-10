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
import type { Account } from '../domain/account.js';
import type { AccountContactInformation } from '../domain/account_contact_information.js';
import type { Result } from '../../../utility/protocol.js';

/**
 * @brief The workflow step that publishes a DQ-cleared accounts bundle.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply, so it states a subject and no response. Its body is the DQ artefact
 * the server-side function knows how to expand, which is why it declares no
 * fields.
 */
export interface PublishAccountsFromDqRequest {}

export interface SaveAccountRequest {
    principal: string;
    /**
     * @brief The account's initial password, in the clear, to be hashed
     * server-side.
     *
     * The only credential a request carries, and the only direction one
     * travels. The hash the server derives from it is written to the account's
     * credential row and never leaves it.
     */
    password: string;
    email: string;
    account_type: string;
}

export interface UpdateAccountRequest {
    account_id: string;
    email: string;
    /**
     * @brief The account holder's full (real) name. Empty clears it.
     */
    full_name: string;
    /**
     * @brief Party to set as the account's default quick-login party.
     * Empty clears the default. Must be one of the account's assigned
     * parties (validated server-side).
     */
    default_party_id: string;
    /**
     * @brief Job title / functional role of the person holding this
     * account (e.g. "Head of Desk", "Senior Trader"). Empty clears it.
     */
    job_title: string;
    /**
     * @brief The account this person reports to. Empty clears it. Must
     * be another account in the same tenant (validated server-side).
     */
    reports_to_account_id: string;
    /**
     * @brief Profile picture for this account. Empty clears it. Must
     * reference an existing image (validated server-side).
     */
    image_id: string;
    change_reason_code: string;
    change_commentary: string;
}

export interface UpdateAccountResponse {
    success: boolean;
    message: string;
}

export interface SaveAccountResponse {
    success: boolean;
    message: string;
    account_id: string;
}

export interface DeleteAccountRequest {
    account_id: string;
}

export interface DeleteAccountResponse {
    success: boolean;
    message: string;
}

export interface AccountOperationResult {
    success: boolean;
    message: string;
}

export interface LockAccountRequest {
    account_ids: string[];
}

export interface LockAccountResponse {
    results: AccountOperationResult[];
}

export interface UnlockAccountRequest {
    account_ids: string[];
}

export interface UnlockAccountResponse {
    results: AccountOperationResult[];
}

export interface ResetPasswordRequest {
    account_ids: string[];
    new_password: string;
}

export interface ResetPasswordResponse {
    success: boolean;
    message: string;
    results: AccountOperationResult[];
}

export interface ChangePasswordRequest {
    current_password: string;
    new_password: string;
}

export interface ChangePasswordResponse {
    success: boolean;
    message: string;
}

export interface UpdateMyEmailRequest {
    email: string;
}

export interface UpdateMyEmailResponse {
    success: boolean;
    message: string;
}

/**
 * @brief A member's write on the party quick sign-in uses.
 *
 * The session names the account, so the request cannot aim the write at
 * another account, and the write needs no permission. The party must be one
 * the account is associated with, or the write is refused: the server checks
 * the association rather than trusting the caller.
 *
 * An empty =party_id= clears the stored default. A party id is a UUID, so an
 * empty string is unambiguous, and this is the only way the request can say
 * "no default" — the reason the member's screen had no clear control before.
 */
export interface SetMyDefaultPartyRequest {
    /**
     * @brief The party to store as the default, or empty to clear it.
     */
    party_id: string;
}

export interface SetMyDefaultPartyResponse {
    success: boolean;
    message: string;
}

export interface SelectPartyRequest {
    party_id: string;
}

/**
 * @brief Re-scopes an *already-logged-in* session to a different party.
 *
 * Deliberately a separate subject/handler from select_party rather than a
 * relaxed version of it: select_party only accepts a narrowly-scoped,
 * single-use token (audience "select_party_only") issued exclusively by
 * the login flow, by design -- see account_operations_handler.hpp's select_party for
 * why. switch_party accepts a normal, already-authenticated session token
 * instead (any token that is NOT that single-use one), so an account with
 * access to more than one party (e.g. a tenant admin with cross-entity
 * access) can change which party's data is in view mid-session without
 * logging out and back in. Same party-membership check and new-token
 * issuance as select_party otherwise.
 */
export interface SwitchPartyRequest {
    party_id: string;
}

export interface SelectPartyResponse {
    success: boolean;
    message: string;
    token: string;
    username: string;
    tenant_name: string;
    party_name: string;
    /**
     * @brief True when the selected party's status is 'Inactive'.
     * The client should present the PartyProvisioningWizard immediately.
     */
    party_setup_required: boolean;
    /**
     * @brief Set when the party provisioner wizard has completed
     * (onboarding.party = true) but the party is still Inactive. The
     * client should show a message instead of re-launching the wizard.
     */
    party_setup_warning: string;
    /**
     * @brief Token lifetime in seconds for the newly issued token.
     *
     * Clients re-arm the proactive refresh timer using this value.
     */
    access_lifetime_s: number;
}

export interface ChangePasswordRequestTyped {
    current_password: string;
    new_password: string;
}

/**
 * @brief A member's write on their own account.
 *
 * The session names the account: the request carries no account id, so it
 * cannot name another account. The three fields a member owns are the whole
 * of the profile write; the three a member does not own are declared beside
 * them so a stated value is refused by name rather than dropped in silence.
 *
 * The write states the owned fields whole: an empty string replaces the
 * field with nothing. The same empty string on an unowned field means the
 * field is not stated, and the write leaves it as it is.
 */
export interface UpdateSelfAccountRequest {
    /**
     * @brief The member's full (real) name. A member owns this field. Empty
     * clears it.
     */
    full_name: string;
    /**
     * @brief Job title / functional role. A member owns this field. Empty
     * clears it.
     */
    job_title: string;
    /**
     * @brief Profile picture, as the id of an uploaded image. A member owns
     * this field. Empty clears it.
     */
    image_id: string;
    /**
     * @brief The sign-in address. An administrator owns this field: a request
     * that states it non-empty is denied with field_not_self_writable, and the
     * field is left as it is.
     */
    email: string;
    /**
     * @brief Party used for quick login. An administrator owns this field: a
     * request that states it non-empty is denied with field_not_self_writable,
     * and the field is left as it is.
     */
    default_party_id: string;
    /**
     * @brief The account this person reports to. An administrator owns this
     * field: a request that states it non-empty is denied with
     * field_not_self_writable, and the field is left as it is. The screen carries
     * a reporting-line change as an ordinary change request, never as a write.
     */
    reports_to_account_id: string;
    change_reason_code: string;
    change_commentary: string;
}

export interface UpdateSelfAccountResponse {
    result: Result;
    /**
     * @brief The account as written, so the panel can show its new version.
     * Stated only when the outcome is ok.
     */
    account: Account | null;
}

/**
 * @brief A member's write on their own contact record.
 *
 * The session names the account and the record is found from it, so the
 * request carries no account id and no record id, and it cannot name another
 * account's record. A member who has no contact record gets one on the first
 * write.
 *
 * The write states the nine fields whole: an empty string replaces the field
 * with nothing. A stated email must be a valid address, and a stated country
 * code must be an ISO 3166-1 alpha-2 code, or the write is refused with the
 * field named.
 */
export interface UpdateSelfAccountContactInformationRequest {
    street_line_1: string;
    street_line_2: string;
    city: string;
    state: string;
    country_code: string;
    postal_code: string;
    phone: string;
    /**
     * @brief The contact address colleagues use. Empty clears it. This is not
     * the sign-in address, which is the account's email and belongs to an
     * administrator.
     */
    email: string;
    web_page: string;
    change_reason_code: string;
    change_commentary: string;
}

export interface UpdateSelfAccountContactInformationResponse {
    result: Result;
    /**
     * @brief The contact record as written, so the panel can show its new
     * version. Stated only when the outcome is ok.
     */
    account_contact_information: AccountContactInformation | null;
}

/**
 * @brief A member's read of their own contact record.
 *
 * The session names the account, so the request carries no account id and
 * cannot name another account's record. The read needs no permission: it is
 * a self read on the allow-list of Authorised reads. Reading a colleague's
 * record is list_by_account_id, which needs
 * iam::account_contact_informations:read.
 */
export interface GetMyAccountContactInformationRequest {}

export interface GetMyAccountContactInformationResponse {
    result: Result;
    /**
     * @brief The caller's contact record, or nothing when they have none yet.
     * The first update-self creates it.
     */
    account_contact_information: AccountContactInformation | null;
}

/**
 * @brief A member's read of their own account.
 *
 * The session names the account, so the request carries no account id and
 * cannot name another account's. The read needs no permission: it is a self
 * read on the allow-list of Authorised reads, so a member sees their own name,
 * picture and job title without holding iam::accounts:read. Reading a
 * colleague's account is iam.v1.accounts.get, which needs that permission.
 */
export interface GetMyAccountRequest {}

export interface GetMyAccountResponse {
    result: Result;
    /**
     * @brief The caller's account, or nothing when the session names none.
     */
    account: Account | null;
}

/**
 * @brief One party the caller's account works in, as the member's own screen
 * reads it.
 *
 * The name, the category and the business centre come from IAM's party cache,
 * which mirrors refdata's parties per tenant; the association itself carries
 * the identifier alone. A party the cache has not seen answers with empty
 * strings rather than a half-read row.
 *
 * Declared before the response that carries it, because the generators emit
 * the messages in the order this file states them.
 */
export interface MyParty {
    party_id: string;
    name: string;
    short_code: string;
    party_category: string;
    business_center_code: string;
}

/**
 * @brief A member's read of the parties their own account works in.
 *
 * The session names the account, so the request carries no account id and
 * cannot name another account's list. The read needs no permission: it is a
 * self read on the allow-list of Authorised reads.
 *
 * The subject exists because the association read answers with a party
 * identifier and nothing on the browser's path turns one into a name: the
 * sign-in reply is the only read that names a party today.
 */
export interface GetMyPartiesRequest {}

export interface GetMyPartiesResponse {
    result: Result;
    /**
     * @brief The party quick sign-in uses, or empty when the account stores none.
     */
    default_party_id: string;
    parties: MyParty[];
}

/**
 * @brief An administrator's write on who one account reports to.
 *
 * The write names one field and nothing else, so a screen that changes the
 * reporting line does not send every other field back, and a field changed
 * elsewhere between the screen's read and this write is not silently
 * overwritten. It needs =iam::accounts:update=, as every administered account
 * write does.
 *
 * An empty =reports_to_account_id= clears the line. =expected_version= is the
 * account version the caller read: when it is stated and the account has moved
 * on since, the write is refused with a conflict rather than applied to a row
 * the caller has not seen. An absent version states no precondition.
 */
export interface SetReportingLineRequest {
    account_id: string;
    /**
     * @brief The manager's account id, or empty to clear the line.
     */
    reports_to_account_id: string;
    /**
     * @brief The account version the caller read, in decimal, or empty for no
     * precondition.
     *
     * Text rather than a number because empty has to be expressible on this wire,
     * as it is for every other optional value here, and because a fresh account's
     * version is 0: no integer is left over to mean "not stated".
     */
    expected_version: string;
    change_reason_code: string;
    change_commentary: string;
}

export interface SetReportingLineResponse {
    result: Result;
    /**
     * @brief The account as written, so the screen can redraw with its new
     * version. Stated only when the outcome is ok.
     */
    account: Account | null;
}

/**
 * @brief One party the reporting shape is drawn under.
 *
 * A person works in a party, and a viewer may work in several, so the shape is
 * grouped by party. The service states which parties are in scope by id; the
 * handler names them from IAM's party cache, which mirrors refdata's parties,
 * so the name and the parent are empty for a party the cache has not seen
 * rather than a half-read row.
 *
 * Declared before the node and the response that carry it, because the
 * generators emit the messages in the order this file states them.
 */
export interface ReportingTreeParty {
    party_id: string;
    name: string;
    short_code: string;
    /**
     * @brief The parent party's id, or empty when the party has none or its parent
     * is not in the scope the caller may read.
     */
    parent_party_id: string;
}

/**
 * @brief One account in the tenant's reporting shape.
 *
 * The depth counts from a root: a root is 0, its reports are 1, and so on.
 * When the read names a root, that root is 0 in the answer. An account that
 * does not reach any root — a manager who is missing, or a ring the store
 * accepted before the guard existed — is answered with depth -1 and counted in
 * the response's unrooted total.
 *
 * Declared before the response that carries it, because the generators emit
 * the messages in the order this file states them.
 */
export interface ReportingTreeNode {
    account_id: string;
    username: string;
    full_name: string;
    job_title: string;
    /**
     * @brief The id of the account's picture, or empty when it has none. The
     * picture is drawn from this identifier, so a reader who may see the tree needs
     * no read of the account to draw the person.
     */
    image_id: string;
    /**
     * @brief The manager's account id, or empty when the account is a root or its
     * manager is outside what the caller may see. The second case is stated by
     * =reports_outside_scope=, so a person who has a manager is never drawn as one
     * who has none.
     */
    reports_to_account_id: string;
    /**
     * @brief The number of managers between this account and a root, or -1 when it
     * reaches none.
     */
    depth: number;
    direct_reports: number;
    /**
     * @brief Whether the account has a manager the caller may not see. The manager
     * is not named, and the account is drawn as the top of what the caller can see
     * with this marker, not as a person with no manager.
     */
    reports_outside_scope: boolean;
    /**
     * @brief The ids of the parties in scope that this account works in. Empty when
     * the account is linked to none of them.
     */
    party_ids: string[];
}

/**
 * @brief A reporting shape in one read: the tenant's, or what the caller may see.
 *
 * The scope follows what the caller holds. A caller with =iam::accounts:read=
 * reads the tenant's own accounts, which row-level security bounds. A caller
 * with =iam::organisation:read= and not that code reads the people they may
 * see: those who work in any party their own account works in, and everyone
 * who reports to them directly or indirectly. A node never carries an email or
 * a contact detail. A caller with neither is refused. An empty =root_account_id= asks for the whole
 * scope, ordered by depth; a stated root asks for that account's branch, so a
 * screen can open one part of a large organisation without carrying the rest.
 */
export interface GetReportingTreeRequest {
    root_account_id: string;
}

export interface GetReportingTreeResponse {
    result: Result;
    nodes: ReportingTreeNode[];
    /**
     * @brief The parties in scope, each with the party above it when that party is
     * in scope too. A holding group's parties nest, so a viewer who works in all of
     * them sees the group drawn as the group.
     */
    parties: ReportingTreeParty[];
    /**
     * @brief How many accounts in the tenant reach no root. Stated so the screen
     * can say the shape is broken rather than draw a tree that is missing people.
     */
    unrooted: number;
}

/**
 * @brief Reconciles the accounts a tenant holds with the pictures they name.
 *
 * Every account that names a picture code and carries no picture gets one:
 * the image is ensured in the tenant and the account is updated. An account
 * that already carries a picture is left alone, so a repeated call writes
 * nothing.
 *
 * A trigger rather than a report: the provisioning step sends it, and an
 * operator may re-run it from a client.
 */
export interface AttachAccountPicturesRequest {}

export interface AttachAccountPicturesResponse {
    result: Result;
    /**
     * @brief The images attached, one per account that wanted one.
     */
    image_ids: string[];
    /**
     * @brief The accounts the images were attached to.
     */
    account_ids: string[];
}

export const subjects = {
    publish_accounts_from_dq_request: 'iam.v1.ops.publish_accounts_from_dq',
    save_account_request: 'iam.v1.accounts.put',
    update_account_request: 'iam.v1.ops.update_account',
    delete_account_request: 'iam.v1.accounts.delete',
    lock_account_request: 'iam.v1.ops.lock_account',
    unlock_account_request: 'iam.v1.ops.unlock_account',
    reset_password_request: 'iam.v1.ops.reset_password',
    update_my_email_request: 'iam.v1.ops.update_my_email',
    set_my_default_party_request: 'iam.v1.ops.set_my_default_party',
    select_party_request: 'iam.v1.ops.select_party',
    switch_party_request: 'iam.v1.ops.switch_party',
    change_password_request_typed: 'iam.v1.ops.change_password',
    update_self_account_request: 'iam.v1.ops.update_self_account',
    update_self_account_contact_information_request:
        'iam.v1.ops.update_self_account_contact_information',
    get_my_account_contact_information_request: 'iam.v1.ops.get_my_account_contact_information',
    get_my_account_request: 'iam.v1.ops.get_my_account',
    get_my_parties_request: 'iam.v1.ops.get_my_parties',
    set_reporting_line_request: 'iam.v1.ops.set_reporting_line',
    get_reporting_tree_request: 'iam.v1.ops.get_reporting_tree',
    attach_account_pictures_request: 'iam.v1.ops.attach_account_pictures',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    publish_accounts_from_dq_request: true,
    save_account_request: true,
    update_account_request: true,
    delete_account_request: true,
    lock_account_request: true,
    unlock_account_request: true,
    reset_password_request: true,
    update_my_email_request: true,
    set_my_default_party_request: true,
    select_party_request: true,
    switch_party_request: true,
    change_password_request_typed: true,
    update_self_account_request: true,
    update_self_account_contact_information_request: true,
    get_my_account_contact_information_request: true,
    get_my_account_request: true,
    get_my_parties_request: true,
    set_reporting_line_request: true,
    get_reporting_tree_request: true,
    attach_account_pictures_request: true,
} as const;
