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
#include "ores.iam.api/domain/session.hpp"
#include "ores.iam.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <cstddef>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
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
    /**
     * @brief The party to store as the default, or empty to clear it.
     */
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
 * @brief A member's read of their own account.
 *
 * The session names the account, so the request carries no account id and
 * cannot name another account's. The read needs no permission: it is a self
 * read on the allow-list of Authorised reads, so a member sees their own name,
 * picture and job title without holding iam::accounts:read. Reading a
 * colleague's account is iam.v1.accounts.get, which needs that permission.
 */
struct get_my_account_request {
    using response_type = struct get_my_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_my_account";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct get_my_account_response {
    ores::utility::domain::result result;
    /**
     * @brief The caller's account, or nothing when the session names none.
     */
    std::optional<ores::iam::domain::account> account;
};

/**
 * @brief A member's read of their own sign-in state.
 *
 * The session names the account, so the request carries no account id and
 * cannot name another account's. The read needs no permission: it is a self
 * read on the allow-list of Authorised reads, so a member sees whether their
 * account is locked and when they last signed in without holding
 * iam::login_info:read. Reading another account's is iam.v1.login_info.get,
 * which needs that permission.
 */
struct get_my_login_info_request {
    using response_type = struct get_my_login_info_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_my_login_info";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct get_my_login_info_response {
    ores::utility::domain::result result;
    /**
     * @brief The caller's sign-in state, or nothing when none is recorded.
     */
    std::optional<ores::iam::domain::login_info> login_info;
};

/**
 * @brief A member's read of the places their own account is signed in.
 *
 * The session names the account, so the request carries no account id and
 * cannot name another account's. The read needs no permission: it is a self
 * read on the allow-list of Authorised reads. The tenant-wide read of open
 * sessions is iam.v1.ops.get_active_sessions, which needs iam::sessions:read.
 */
struct get_my_sessions_request {
    using response_type = struct get_my_sessions_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_my_sessions";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct get_my_sessions_response {
    ores::utility::domain::result result;
    /**
     * @brief The caller's sessions with no end time, newest first.
     */
    std::vector<ores::iam::domain::session> sessions;
};

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
struct my_party {
    std::string party_id;
    std::string name;
    std::string short_code;
    std::string party_category;
    std::string business_center_code;
};

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
struct get_my_parties_request {
    using response_type = struct get_my_parties_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_my_parties";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct get_my_parties_response {
    ores::utility::domain::result result;
    /**
     * @brief The party quick sign-in uses, or empty when the account stores none.
     */
    std::string default_party_id;
    std::vector<my_party> parties;
};

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
struct set_reporting_line_request {
    using response_type = struct set_reporting_line_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.set_reporting_line";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string account_id;
    /**
     * @brief The manager's account id, or empty to clear the line.
     */
    std::string reports_to_account_id;
    /**
     * @brief The account version the caller read, in decimal, or empty for no
     * precondition.
     *
     * Text rather than a number because empty has to be expressible on this wire,
     * as it is for every other optional value here, and because a fresh account's
     * version is 0: no integer is left over to mean "not stated".
     */
    std::string expected_version;
    std::string change_reason_code;
    std::string change_commentary;
};

struct set_reporting_line_response {
    ores::utility::domain::result result;
    /**
     * @brief The account as written, so the screen can redraw with its new
     * version. Stated only when the outcome is ok.
     */
    std::optional<ores::iam::domain::account> account;
};

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
struct reporting_tree_party {
    std::string party_id;
    std::string name;
    std::string short_code;
    /**
     * @brief The parent party's id, or empty when the party has none or its parent
     * is not in the scope the caller may read.
     */
    std::string parent_party_id;
};

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
struct reporting_tree_node {
    std::string account_id;
    std::string username;
    std::string full_name;
    std::string job_title;
    /**
     * @brief The account's kind: =user= for a person, otherwise the kind of service
     * account. A reader tells a person from a service by it.
     */
    std::string account_type;
    /**
     * @brief The id of the account's picture, or empty when it has none. The
     * picture is drawn from this identifier, so a reader who may see the tree needs
     * no read of the account to draw the person.
     */
    std::string image_id;
    /**
     * @brief The manager's account id, or empty when the account is a root or its
     * manager is outside what the caller may see. The second case is stated by
     * =reports_outside_scope=, so a person who has a manager is never drawn as one
     * who has none.
     */
    std::string reports_to_account_id;
    /**
     * @brief The number of managers between this account and a root, or -1 when it
     * reaches none.
     */
    int depth = 0;
    int direct_reports = 0;
    /**
     * @brief Whether the account has a manager the caller may not see. The manager
     * is not named, and the account is drawn as the top of what the caller can see
     * with this marker, not as a person with no manager.
     */
    bool reports_outside_scope = false;
    /**
     * @brief The ids of the parties in scope that this account works in. Empty when
     * the account is linked to none of them.
     */
    std::vector<std::string> party_ids;
};

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
struct get_reporting_tree_request {
    using response_type = struct get_reporting_tree_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_reporting_tree";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string root_account_id;
};

struct get_reporting_tree_response {
    ores::utility::domain::result result;
    std::vector<reporting_tree_node> nodes;
    /**
     * @brief The parties in scope, each with the party above it when that party is
     * in scope too. A holding group's parties nest, so a viewer who works in all of
     * them sees the group drawn as the group.
     */
    std::vector<reporting_tree_party> parties;
    /**
     * @brief How many accounts in the tenant reach no root. Stated so the screen
     * can say the shape is broken rather than draw a tree that is missing people.
     */
    int unrooted = 0;
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

/**
 * @brief The members of publish_accounts_from_dq_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const publish_accounts_from_dq_request&) {
    return std::tuple{};
}

/**
 * @brief The members of save_account_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const save_account_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"principal", &save_account_request::principal},
        ::ores::utility::codec::field{"password", &save_account_request::password},
        ::ores::utility::codec::field{"email", &save_account_request::email},
        ::ores::utility::codec::field{"account_type", &save_account_request::account_type},
    };
}

/**
 * @brief The members of update_account_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const update_account_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"account_id", &update_account_request::account_id},
        ::ores::utility::codec::field{"email", &update_account_request::email},
        ::ores::utility::codec::field{"full_name", &update_account_request::full_name},
        ::ores::utility::codec::field{"default_party_id",
                                      &update_account_request::default_party_id},
        ::ores::utility::codec::field{"job_title", &update_account_request::job_title},
        ::ores::utility::codec::field{"reports_to_account_id",
                                      &update_account_request::reports_to_account_id},
        ::ores::utility::codec::field{"image_id", &update_account_request::image_id},
        ::ores::utility::codec::field{"change_reason_code",
                                      &update_account_request::change_reason_code},
        ::ores::utility::codec::field{"change_commentary",
                                      &update_account_request::change_commentary},
    };
}

/**
 * @brief The members of update_account_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const update_account_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &update_account_response::success},
        ::ores::utility::codec::field{"message", &update_account_response::message},
    };
}

/**
 * @brief The members of save_account_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const save_account_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &save_account_response::success},
        ::ores::utility::codec::field{"message", &save_account_response::message},
        ::ores::utility::codec::field{"account_id", &save_account_response::account_id},
    };
}

/**
 * @brief The members of delete_account_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_account_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"account_id", &delete_account_request::account_id},
    };
}

/**
 * @brief The members of delete_account_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_account_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &delete_account_response::success},
        ::ores::utility::codec::field{"message", &delete_account_response::message},
    };
}

/**
 * @brief The members of account_operation_result, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const account_operation_result&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &account_operation_result::success},
        ::ores::utility::codec::field{"message", &account_operation_result::message},
    };
}

/**
 * @brief The members of lock_account_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const lock_account_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"account_ids", &lock_account_request::account_ids},
    };
}

/**
 * @brief The members of lock_account_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const lock_account_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"results", &lock_account_response::results},
    };
}

/**
 * @brief The members of unlock_account_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const unlock_account_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"account_ids", &unlock_account_request::account_ids},
    };
}

/**
 * @brief The members of unlock_account_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const unlock_account_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"results", &unlock_account_response::results},
    };
}

/**
 * @brief The members of reset_password_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const reset_password_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"account_ids", &reset_password_request::account_ids},
        ::ores::utility::codec::field{"new_password", &reset_password_request::new_password},
    };
}

/**
 * @brief The members of reset_password_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const reset_password_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &reset_password_response::success},
        ::ores::utility::codec::field{"message", &reset_password_response::message},
        ::ores::utility::codec::field{"results", &reset_password_response::results},
    };
}

/**
 * @brief The members of change_password_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const change_password_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"current_password",
                                      &change_password_request::current_password},
        ::ores::utility::codec::field{"new_password", &change_password_request::new_password},
    };
}

/**
 * @brief The members of change_password_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const change_password_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &change_password_response::success},
        ::ores::utility::codec::field{"message", &change_password_response::message},
    };
}

/**
 * @brief The members of update_my_email_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const update_my_email_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"email", &update_my_email_request::email},
    };
}

/**
 * @brief The members of update_my_email_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const update_my_email_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &update_my_email_response::success},
        ::ores::utility::codec::field{"message", &update_my_email_response::message},
    };
}

/**
 * @brief The members of set_my_default_party_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const set_my_default_party_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"party_id", &set_my_default_party_request::party_id},
    };
}

/**
 * @brief The members of set_my_default_party_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const set_my_default_party_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &set_my_default_party_response::success},
        ::ores::utility::codec::field{"message", &set_my_default_party_response::message},
    };
}

/**
 * @brief The members of select_party_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const select_party_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"party_id", &select_party_request::party_id},
    };
}

/**
 * @brief The members of switch_party_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const switch_party_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"party_id", &switch_party_request::party_id},
    };
}

/**
 * @brief The members of select_party_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const select_party_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &select_party_response::success},
        ::ores::utility::codec::field{"message", &select_party_response::message},
        ::ores::utility::codec::field{"token", &select_party_response::token},
        ::ores::utility::codec::field{"username", &select_party_response::username},
        ::ores::utility::codec::field{"tenant_name", &select_party_response::tenant_name},
        ::ores::utility::codec::field{"party_name", &select_party_response::party_name},
        ::ores::utility::codec::field{"party_setup_required",
                                      &select_party_response::party_setup_required},
        ::ores::utility::codec::field{"party_setup_warning",
                                      &select_party_response::party_setup_warning},
        ::ores::utility::codec::field{"access_lifetime_s",
                                      &select_party_response::access_lifetime_s},
    };
}

/**
 * @brief The members of change_password_request_typed, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const change_password_request_typed&) {
    return std::tuple{
        ::ores::utility::codec::field{"current_password",
                                      &change_password_request_typed::current_password},
        ::ores::utility::codec::field{"new_password", &change_password_request_typed::new_password},
    };
}

/**
 * @brief The members of update_self_account_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const update_self_account_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"full_name", &update_self_account_request::full_name},
        ::ores::utility::codec::field{"job_title", &update_self_account_request::job_title},
        ::ores::utility::codec::field{"image_id", &update_self_account_request::image_id},
        ::ores::utility::codec::field{"email", &update_self_account_request::email},
        ::ores::utility::codec::field{"default_party_id",
                                      &update_self_account_request::default_party_id},
        ::ores::utility::codec::field{"reports_to_account_id",
                                      &update_self_account_request::reports_to_account_id},
        ::ores::utility::codec::field{"change_reason_code",
                                      &update_self_account_request::change_reason_code},
        ::ores::utility::codec::field{"change_commentary",
                                      &update_self_account_request::change_commentary},
    };
}

/**
 * @brief The members of update_self_account_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const update_self_account_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &update_self_account_response::result},
        ::ores::utility::codec::field{"account", &update_self_account_response::account},
    };
}

/**
 * @brief The members of update_self_account_contact_information_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const update_self_account_contact_information_request&) {
    return std::tuple{
        ::ores::utility::codec::field{
            "street_line_1", &update_self_account_contact_information_request::street_line_1},
        ::ores::utility::codec::field{
            "street_line_2", &update_self_account_contact_information_request::street_line_2},
        ::ores::utility::codec::field{"city",
                                      &update_self_account_contact_information_request::city},
        ::ores::utility::codec::field{"state",
                                      &update_self_account_contact_information_request::state},
        ::ores::utility::codec::field{
            "country_code", &update_self_account_contact_information_request::country_code},
        ::ores::utility::codec::field{
            "postal_code", &update_self_account_contact_information_request::postal_code},
        ::ores::utility::codec::field{"phone",
                                      &update_self_account_contact_information_request::phone},
        ::ores::utility::codec::field{"email",
                                      &update_self_account_contact_information_request::email},
        ::ores::utility::codec::field{"web_page",
                                      &update_self_account_contact_information_request::web_page},
        ::ores::utility::codec::field{
            "change_reason_code",
            &update_self_account_contact_information_request::change_reason_code},
        ::ores::utility::codec::field{
            "change_commentary",
            &update_self_account_contact_information_request::change_commentary},
    };
}

/**
 * @brief The members of update_self_account_contact_information_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const update_self_account_contact_information_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result",
                                      &update_self_account_contact_information_response::result},
        ::ores::utility::codec::field{
            "account_contact_information",
            &update_self_account_contact_information_response::account_contact_information},
    };
}

/**
 * @brief The members of get_my_account_contact_information_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_my_account_contact_information_request&) {
    return std::tuple{};
}

/**
 * @brief The members of get_my_account_contact_information_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_my_account_contact_information_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result",
                                      &get_my_account_contact_information_response::result},
        ::ores::utility::codec::field{
            "account_contact_information",
            &get_my_account_contact_information_response::account_contact_information},
    };
}

/**
 * @brief The members of get_my_account_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_my_account_request&) {
    return std::tuple{};
}

/**
 * @brief The members of get_my_account_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_my_account_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &get_my_account_response::result},
        ::ores::utility::codec::field{"account", &get_my_account_response::account},
    };
}

/**
 * @brief The members of get_my_login_info_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_my_login_info_request&) {
    return std::tuple{};
}

/**
 * @brief The members of get_my_login_info_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_my_login_info_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &get_my_login_info_response::result},
        ::ores::utility::codec::field{"login_info", &get_my_login_info_response::login_info},
    };
}

/**
 * @brief The members of get_my_sessions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_my_sessions_request&) {
    return std::tuple{};
}

/**
 * @brief The members of get_my_sessions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_my_sessions_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &get_my_sessions_response::result},
        ::ores::utility::codec::field{"sessions", &get_my_sessions_response::sessions},
    };
}

/**
 * @brief The members of my_party, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const my_party&) {
    return std::tuple{
        ::ores::utility::codec::field{"party_id", &my_party::party_id},
        ::ores::utility::codec::field{"name", &my_party::name},
        ::ores::utility::codec::field{"short_code", &my_party::short_code},
        ::ores::utility::codec::field{"party_category", &my_party::party_category},
        ::ores::utility::codec::field{"business_center_code", &my_party::business_center_code},
    };
}

/**
 * @brief The members of get_my_parties_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_my_parties_request&) {
    return std::tuple{};
}

/**
 * @brief The members of get_my_parties_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_my_parties_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &get_my_parties_response::result},
        ::ores::utility::codec::field{"default_party_id",
                                      &get_my_parties_response::default_party_id},
        ::ores::utility::codec::field{"parties", &get_my_parties_response::parties},
    };
}

/**
 * @brief The members of set_reporting_line_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const set_reporting_line_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"account_id", &set_reporting_line_request::account_id},
        ::ores::utility::codec::field{"reports_to_account_id",
                                      &set_reporting_line_request::reports_to_account_id},
        ::ores::utility::codec::field{"expected_version",
                                      &set_reporting_line_request::expected_version},
        ::ores::utility::codec::field{"change_reason_code",
                                      &set_reporting_line_request::change_reason_code},
        ::ores::utility::codec::field{"change_commentary",
                                      &set_reporting_line_request::change_commentary},
    };
}

/**
 * @brief The members of set_reporting_line_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const set_reporting_line_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &set_reporting_line_response::result},
        ::ores::utility::codec::field{"account", &set_reporting_line_response::account},
    };
}

/**
 * @brief The members of reporting_tree_party, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const reporting_tree_party&) {
    return std::tuple{
        ::ores::utility::codec::field{"party_id", &reporting_tree_party::party_id},
        ::ores::utility::codec::field{"name", &reporting_tree_party::name},
        ::ores::utility::codec::field{"short_code", &reporting_tree_party::short_code},
        ::ores::utility::codec::field{"parent_party_id", &reporting_tree_party::parent_party_id},
    };
}

/**
 * @brief The members of reporting_tree_node, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const reporting_tree_node&) {
    return std::tuple{
        ::ores::utility::codec::field{"account_id", &reporting_tree_node::account_id},
        ::ores::utility::codec::field{"username", &reporting_tree_node::username},
        ::ores::utility::codec::field{"full_name", &reporting_tree_node::full_name},
        ::ores::utility::codec::field{"job_title", &reporting_tree_node::job_title},
        ::ores::utility::codec::field{"account_type", &reporting_tree_node::account_type},
        ::ores::utility::codec::field{"image_id", &reporting_tree_node::image_id},
        ::ores::utility::codec::field{"reports_to_account_id",
                                      &reporting_tree_node::reports_to_account_id},
        ::ores::utility::codec::field{"depth", &reporting_tree_node::depth},
        ::ores::utility::codec::field{"direct_reports", &reporting_tree_node::direct_reports},
        ::ores::utility::codec::field{"reports_outside_scope",
                                      &reporting_tree_node::reports_outside_scope},
        ::ores::utility::codec::field{"party_ids", &reporting_tree_node::party_ids},
    };
}

/**
 * @brief The members of get_reporting_tree_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_reporting_tree_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"root_account_id",
                                      &get_reporting_tree_request::root_account_id},
    };
}

/**
 * @brief The members of get_reporting_tree_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_reporting_tree_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &get_reporting_tree_response::result},
        ::ores::utility::codec::field{"nodes", &get_reporting_tree_response::nodes},
        ::ores::utility::codec::field{"parties", &get_reporting_tree_response::parties},
        ::ores::utility::codec::field{"unrooted", &get_reporting_tree_response::unrooted},
    };
}

/**
 * @brief The members of attach_account_pictures_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const attach_account_pictures_request&) {
    return std::tuple{};
}

/**
 * @brief The members of attach_account_pictures_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const attach_account_pictures_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &attach_account_pictures_response::result},
        ::ores::utility::codec::field{"image_ids", &attach_account_pictures_response::image_ids},
        ::ores::utility::codec::field{"account_ids",
                                      &attach_account_pictures_response::account_ids},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::publish_accounts_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::publish_accounts_from_dq_request& v);
    static ores::iam::messaging::publish_accounts_from_dq_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::publish_accounts_from_dq_request& v);
    static ores::iam::messaging::publish_accounts_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::save_account_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::save_account_request& v);
    static ores::iam::messaging::save_account_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::save_account_request& v);
    static ores::iam::messaging::save_account_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::update_account_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::update_account_request& v);
    static ores::iam::messaging::update_account_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::update_account_request& v);
    static ores::iam::messaging::update_account_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::update_account_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::update_account_response& v);
    static ores::iam::messaging::update_account_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::update_account_response& v);
    static ores::iam::messaging::update_account_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::save_account_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::save_account_response& v);
    static ores::iam::messaging::save_account_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::save_account_response& v);
    static ores::iam::messaging::save_account_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_account_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_account_request& v);
    static ores::iam::messaging::delete_account_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_account_request& v);
    static ores::iam::messaging::delete_account_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_account_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_account_response& v);
    static ores::iam::messaging::delete_account_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_account_response& v);
    static ores::iam::messaging::delete_account_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_operation_result> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_operation_result& v);
    static ores::iam::messaging::account_operation_result read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_operation_result& v);
    static ores::iam::messaging::account_operation_result
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::lock_account_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::lock_account_request& v);
    static ores::iam::messaging::lock_account_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::lock_account_request& v);
    static ores::iam::messaging::lock_account_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::lock_account_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::lock_account_response& v);
    static ores::iam::messaging::lock_account_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::lock_account_response& v);
    static ores::iam::messaging::lock_account_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::unlock_account_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::unlock_account_request& v);
    static ores::iam::messaging::unlock_account_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::unlock_account_request& v);
    static ores::iam::messaging::unlock_account_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::unlock_account_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::unlock_account_response& v);
    static ores::iam::messaging::unlock_account_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::unlock_account_response& v);
    static ores::iam::messaging::unlock_account_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::reset_password_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::reset_password_request& v);
    static ores::iam::messaging::reset_password_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::reset_password_request& v);
    static ores::iam::messaging::reset_password_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::reset_password_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::reset_password_response& v);
    static ores::iam::messaging::reset_password_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::reset_password_response& v);
    static ores::iam::messaging::reset_password_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::change_password_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::change_password_request& v);
    static ores::iam::messaging::change_password_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::change_password_request& v);
    static ores::iam::messaging::change_password_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::change_password_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::change_password_response& v);
    static ores::iam::messaging::change_password_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::change_password_response& v);
    static ores::iam::messaging::change_password_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::update_my_email_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::update_my_email_request& v);
    static ores::iam::messaging::update_my_email_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::update_my_email_request& v);
    static ores::iam::messaging::update_my_email_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::update_my_email_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::update_my_email_response& v);
    static ores::iam::messaging::update_my_email_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::update_my_email_response& v);
    static ores::iam::messaging::update_my_email_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::set_my_default_party_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::set_my_default_party_request& v);
    static ores::iam::messaging::set_my_default_party_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::set_my_default_party_request& v);
    static ores::iam::messaging::set_my_default_party_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::set_my_default_party_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::set_my_default_party_response& v);
    static ores::iam::messaging::set_my_default_party_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::set_my_default_party_response& v);
    static ores::iam::messaging::set_my_default_party_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::select_party_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::select_party_request& v);
    static ores::iam::messaging::select_party_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::select_party_request& v);
    static ores::iam::messaging::select_party_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::switch_party_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::switch_party_request& v);
    static ores::iam::messaging::switch_party_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::switch_party_request& v);
    static ores::iam::messaging::switch_party_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::select_party_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::select_party_response& v);
    static ores::iam::messaging::select_party_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::select_party_response& v);
    static ores::iam::messaging::select_party_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::change_password_request_typed> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::change_password_request_typed& v);
    static ores::iam::messaging::change_password_request_typed read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::change_password_request_typed& v);
    static ores::iam::messaging::change_password_request_typed
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::update_self_account_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::update_self_account_request& v);
    static ores::iam::messaging::update_self_account_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::update_self_account_request& v);
    static ores::iam::messaging::update_self_account_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::update_self_account_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::update_self_account_response& v);
    static ores::iam::messaging::update_self_account_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::update_self_account_response& v);
    static ores::iam::messaging::update_self_account_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::update_self_account_contact_information_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::update_self_account_contact_information_request& v);
    static ores::iam::messaging::update_self_account_contact_information_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::update_self_account_contact_information_request& v);
    static ores::iam::messaging::update_self_account_contact_information_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::update_self_account_contact_information_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::update_self_account_contact_information_response& v);
    static ores::iam::messaging::update_self_account_contact_information_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::update_self_account_contact_information_response& v);
    static ores::iam::messaging::update_self_account_contact_information_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_my_account_contact_information_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_my_account_contact_information_request& v);
    static ores::iam::messaging::get_my_account_contact_information_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_my_account_contact_information_request& v);
    static ores::iam::messaging::get_my_account_contact_information_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_my_account_contact_information_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_my_account_contact_information_response& v);
    static ores::iam::messaging::get_my_account_contact_information_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_my_account_contact_information_response& v);
    static ores::iam::messaging::get_my_account_contact_information_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_my_account_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_my_account_request& v);
    static ores::iam::messaging::get_my_account_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_my_account_request& v);
    static ores::iam::messaging::get_my_account_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_my_account_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_my_account_response& v);
    static ores::iam::messaging::get_my_account_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_my_account_response& v);
    static ores::iam::messaging::get_my_account_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_my_login_info_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_my_login_info_request& v);
    static ores::iam::messaging::get_my_login_info_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_my_login_info_request& v);
    static ores::iam::messaging::get_my_login_info_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_my_login_info_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_my_login_info_response& v);
    static ores::iam::messaging::get_my_login_info_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_my_login_info_response& v);
    static ores::iam::messaging::get_my_login_info_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_my_sessions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_my_sessions_request& v);
    static ores::iam::messaging::get_my_sessions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_my_sessions_request& v);
    static ores::iam::messaging::get_my_sessions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_my_sessions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_my_sessions_response& v);
    static ores::iam::messaging::get_my_sessions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_my_sessions_response& v);
    static ores::iam::messaging::get_my_sessions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::my_party> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::my_party& v);
    static ores::iam::messaging::my_party read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::my_party& v);
    static ores::iam::messaging::my_party read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_my_parties_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_my_parties_request& v);
    static ores::iam::messaging::get_my_parties_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_my_parties_request& v);
    static ores::iam::messaging::get_my_parties_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_my_parties_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_my_parties_response& v);
    static ores::iam::messaging::get_my_parties_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_my_parties_response& v);
    static ores::iam::messaging::get_my_parties_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::set_reporting_line_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::set_reporting_line_request& v);
    static ores::iam::messaging::set_reporting_line_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::set_reporting_line_request& v);
    static ores::iam::messaging::set_reporting_line_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::set_reporting_line_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::set_reporting_line_response& v);
    static ores::iam::messaging::set_reporting_line_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::set_reporting_line_response& v);
    static ores::iam::messaging::set_reporting_line_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::reporting_tree_party> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::reporting_tree_party& v);
    static ores::iam::messaging::reporting_tree_party read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::reporting_tree_party& v);
    static ores::iam::messaging::reporting_tree_party read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::reporting_tree_node> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::reporting_tree_node& v);
    static ores::iam::messaging::reporting_tree_node read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::reporting_tree_node& v);
    static ores::iam::messaging::reporting_tree_node read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_reporting_tree_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_reporting_tree_request& v);
    static ores::iam::messaging::get_reporting_tree_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_reporting_tree_request& v);
    static ores::iam::messaging::get_reporting_tree_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_reporting_tree_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_reporting_tree_response& v);
    static ores::iam::messaging::get_reporting_tree_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_reporting_tree_response& v);
    static ores::iam::messaging::get_reporting_tree_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::attach_account_pictures_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::attach_account_pictures_request& v);
    static ores::iam::messaging::attach_account_pictures_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::attach_account_pictures_request& v);
    static ores::iam::messaging::attach_account_pictures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::attach_account_pictures_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::attach_account_pictures_response& v);
    static ores::iam::messaging::attach_account_pictures_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::attach_account_pictures_response& v);
    static ores::iam::messaging::attach_account_pictures_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
