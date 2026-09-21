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
import type { LoginInfo } from '../domain/login_info.js';


export interface GetAccountsRequest {
    offset: number;
    limit: number;
}

export interface GetAccountsResponse {
    accounts: Account[];
    total_available_count: number;
}

export interface SaveAccountRequest {
    principal: string;
    password: string;
    totp_secret: string;
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

export interface ListLoginInfoRequest {
}

export interface ListLoginInfoResponse {
    login_infos: LoginInfo[];
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

export interface SetMyDefaultPartyRequest {
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

export interface GetAccountsRequestTyped {
    offset: number;
    limit: number;
}

export interface ChangePasswordRequestTyped {
    current_password: string;
    new_password: string;
}

export const subjects = {
    save_account_request: "iam.v1.accounts.save",
    update_account_request: "iam.v1.accounts.update",
    delete_account_request: "iam.v1.accounts.delete",
    lock_account_request: "iam.v1.accounts.lock",
    unlock_account_request: "iam.v1.accounts.unlock",
    list_login_info_request: "iam.v1.accounts.login-info",
    reset_password_request: "iam.v1.accounts.reset-password",
    update_my_email_request: "iam.v1.accounts.update-email",
    set_my_default_party_request: "iam.v1.accounts.set-default-party",
    select_party_request: "iam.v1.accounts.select-party",
    switch_party_request: "iam.v1.accounts.switch-party",
    get_accounts_request_typed: "iam.v1.accounts.list",
    change_password_request_typed: "iam.v1.accounts.change-password",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    save_account_request: true,
    update_account_request: true,
    delete_account_request: true,
    lock_account_request: true,
    unlock_account_request: true,
    list_login_info_request: true,
    reset_password_request: true,
    update_my_email_request: true,
    set_my_default_party_request: true,
    select_party_request: true,
    switch_party_request: true,
    get_accounts_request_typed: true,
    change_password_request_typed: true,
} as const;
