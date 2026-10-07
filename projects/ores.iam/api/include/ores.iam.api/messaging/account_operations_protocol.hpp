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
#ifndef ORES_IAM_API_MESSAGING_ACCOUNT_OPERATIONS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ACCOUNT_OPERATIONS_PROTOCOL_HPP

#include "ores.iam.api/domain/account.hpp"
#include "ores.iam.api/domain/account_contact_information.hpp"
#include "ores.iam.api/domain/login_info.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief The workflow step that publishes a DQ-cleared accounts bundle.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply, so it states a subject and no response. Its body is the DQ artefact
 * the server-side function knows how to expand, which is why it declares no
 * fields.
 */
struct publish_accounts_from_dq_request {
    static constexpr std::string_view nats_subject = "iam.v1.ops.publish_accounts_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct save_account_request {
    using response_type = struct save_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string principal;
    /**
     * @brief The account's initial password, in the clear, to be hashed
     * server-side.
     *
     * The only credential a request carries, and the only direction one
     * travels. The hash the server derives from it is written to the account's
     * credential row and never leaves it.
     */
    std::string password;
    std::string email;
    std::string account_type;
};

struct update_account_request {
    using response_type = struct update_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.update_account";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string account_id;
    std::string email;
    /**
     * @brief The account holder's full (real) name. Empty clears it.
     */
    std::string full_name;
    /**
     * @brief Party to set as the account's default quick-login party.
     * Empty clears the default. Must be one of the account's assigned
     * parties (validated server-side).
     */
    std::string default_party_id;
    /**
     * @brief Job title / functional role of the person holding this
     * account (e.g. "Head of Desk", "Senior Trader"). Empty clears it.
     */
    std::string job_title;
    /**
     * @brief The account this person reports to. Empty clears it. Must
     * be another account in the same tenant (validated server-side).
     */
    std::string reports_to_account_id;
    /**
     * @brief Profile picture for this account. Empty clears it. Must
     * reference an existing image (validated server-side).
     */
    std::string image_id;
    std::string change_reason_code;
    std::string change_commentary;
};

struct update_account_response {
    bool success = false;
    std::string message;
};

struct save_account_response {
    bool success = false;
    std::string message;
    std::string account_id;
};

struct delete_account_request {
    using response_type = struct delete_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string account_id;
};

struct delete_account_response {
    bool success = false;
    std::string message;
};

struct account_operation_result {
    bool success = false;
    std::string message;
};

struct lock_account_request {
    using response_type = struct lock_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.lock_account";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<std::string> account_ids;
};

struct lock_account_response {
    std::vector<account_operation_result> results;
};

struct unlock_account_request {
    using response_type = struct unlock_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.unlock_account";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<std::string> account_ids;
};

struct unlock_account_response {
    std::vector<account_operation_result> results;
};

struct reset_password_request {
    using response_type = struct reset_password_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.reset_password";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<std::string> account_ids;
    std::string new_password;
};

struct reset_password_response {
    bool success = false;
    std::string message;
    std::vector<account_operation_result> results;
};

struct change_password_request {
    std::string current_password;
    std::string new_password;
};

struct change_password_response {
    bool success = false;
    std::string message;
};

struct update_my_email_request {
    using response_type = struct update_my_email_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.update_my_email";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string email;
};

struct update_my_email_response {
    bool success = false;
    std::string message;
};

struct set_my_default_party_request {
    using response_type = struct set_my_default_party_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.set_my_default_party";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string party_id;
};

struct set_my_default_party_response {
    bool success = false;
    std::string message;
};

struct select_party_request {
    using response_type = struct select_party_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.select_party";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string party_id;
};

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
struct switch_party_request {
    using response_type = struct select_party_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.switch_party";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string party_id;
};

struct select_party_response {
    bool success = false;
    std::string message;
    std::string token;
    std::string username;
    std::string tenant_name;
    std::string party_name;
    /**
     * @brief True when the selected party's status is 'Inactive'.
     * The client should present the PartyProvisioningWizard immediately.
     */
    bool party_setup_required = false;
    /**
     * @brief Set when the party provisioner wizard has completed
     * (onboarding.party = true) but the party is still Inactive. The
     * client should show a message instead of re-launching the wizard.
     */
    std::string party_setup_warning;
    /**
     * @brief Token lifetime in seconds for the newly issued token.
     *
     * Clients re-arm the proactive refresh timer using this value.
     */
    int access_lifetime_s = 1800;
};

struct change_password_request_typed {
    using response_type = struct change_password_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.change_password";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string current_password;
    std::string new_password;
};

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
struct update_self_account_request {
    using response_type = struct update_self_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.update_self_account";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The member's full (real) name. A member owns this field. Empty
     * clears it.
     */
    std::string full_name;
    /**
     * @brief Job title / functional role. A member owns this field. Empty
     * clears it.
     */
    std::string job_title;
    /**
     * @brief Profile picture, as the id of an uploaded image. A member owns
     * this field. Empty clears it.
     */
    std::string image_id;
    /**
     * @brief The sign-in address. An administrator owns this field: a request
     * that states it non-empty is denied with field_not_self_writable, and the
     * field is left as it is.
     */
    std::string email;
    /**
     * @brief Party used for quick login. An administrator owns this field: a
     * request that states it non-empty is denied with field_not_self_writable,
     * and the field is left as it is.
     */
    std::string default_party_id;
    /**
     * @brief The account this person reports to. An administrator owns this
     * field: a request that states it non-empty is denied with
     * field_not_self_writable, and the field is left as it is. The screen carries
     * a reporting-line change as an ordinary change request, never as a write.
     */
    std::string reports_to_account_id;
    std::string change_reason_code;
    std::string change_commentary;
};

struct update_self_account_response {
    ores::utility::domain::result result;
    /**
     * @brief The account as written, so the panel can show its new version.
     * Stated only when the outcome is ok.
     */
    std::optional<ores::iam::domain::account> account;
};

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
struct update_self_account_contact_information_request {
    using response_type = struct update_self_account_contact_information_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.ops.update_self_account_contact_information";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string street_line_1;
    std::string street_line_2;
    std::string city;
    std::string state;
    std::string country_code;
    std::string postal_code;
    std::string phone;
    /**
     * @brief The contact address colleagues use. Empty clears it. This is not
     * the sign-in address, which is the account's email and belongs to an
     * administrator.
     */
    std::string email;
    std::string web_page;
    std::string change_reason_code;
    std::string change_commentary;
};

struct update_self_account_contact_information_response {
    ores::utility::domain::result result;
    /**
     * @brief The contact record as written, so the panel can show its new
     * version. Stated only when the outcome is ok.
     */
    std::optional<ores::iam::domain::account_contact_information> account_contact_information;
};

/**
 * @brief A member's read of their own contact record.
 *
 * The session names the account, so the request carries no account id and
 * cannot name another account's record. The read needs no permission: it is
 * a self read on the allow-list of Authorised reads. Reading a colleague's
 * record is list_by_account_id, which needs
 * iam::account_contact_informations:read.
 */
struct get_my_account_contact_information_request {
    using response_type = struct get_my_account_contact_information_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.ops.get_my_account_contact_information";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct get_my_account_contact_information_response {
    ores::utility::domain::result result;
    /**
     * @brief The caller's contact record, or nothing when they have none yet.
     * The first update-self creates it.
     */
    std::optional<ores::iam::domain::account_contact_information> account_contact_information;
};

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
struct attach_account_pictures_request {
    using response_type = struct attach_account_pictures_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.attach_account_pictures";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct attach_account_pictures_response {
    ores::utility::domain::result result;
    /**
     * @brief The images attached, one per account that wanted one.
     */
    std::vector<std::string> image_ids;
    /**
     * @brief The accounts the images were attached to.
     */
    std::vector<std::string> account_ids;
};

}

#endif
