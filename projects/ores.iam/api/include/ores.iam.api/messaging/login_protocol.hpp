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
#ifndef ORES_IAM_API_MESSAGING_LOGIN_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_LOGIN_PROTOCOL_HPP

#include "ores.iam.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::iam::messaging {

struct party_summary {
    std::string id;
    std::string name;
    std::string party_category;
    std::string business_center_code;
};

/**
 * @brief The database row the deployment was built against.
 *
 * The four values =ores_database_info_tbl= records when the database is
 * created or recreated: the hash of the schema it was built from, the build
 * environment, the commit, and when the database was created. The table holds
 * exactly one row. It is declared by hand and owes nothing to the generators,
 * because it records the checkout that built the database before any service
 * runs.
 */
struct database_info {
    /** @brief The hash of the SQL scripts the database was built from. */
    std::string fingerprint;
    /** @brief The build environment the database was built in. */
    std::string environment;
    /** @brief The git commit the database was built from. */
    std::string commit;
    /** @brief When the database was created or recreated. */
    std::string created;
};

struct login_request {
    using response_type = struct login_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.login";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
    std::string principal;
    std::string password;
};

struct login_response {
    bool success = false;
    std::string account_id;
    std::string tenant_id;
    std::string tenant_name;
    /**
     * @brief The build the answering service runs, in full.
     *
     * A session is a session with a deployment, so the answer that opens one
     * states which build it was opened against: a client that signs in states the
     * deployment's version without having asked whether the deployment still
     * needs an administrator, and a client that did ask can see whether the two
     * answers agree.
     */
    std::string version;
    /**
     * @brief The database the deployment stores into, in full.
     *
     * The row travels with the build the answer already states, because the two
     * answer one question — what am I talking to — and the database is its third.
     * Its audience is the build's audience: everyone who may open a session may
     * read it, so it needs no subject, no permission and no read of its own. The
     * iam service reads it once per login from =ores_database_info_tbl=.
     */
    database_info database;
    std::string username;
    std::string email;
    bool password_reset_required = false;
    bool tenant_bootstrap_mode = false;
    /**
     * @brief Whether the caller's tenant is still being set up.
     *
     * This is the tenant's own lifecycle status, which reads @c bootstrapping
     * until the tenant's setup run marks it @c active. It is a fact about the
     * tenant row, so every party of the tenant reads it the same way, and it is
     * what a screen must hold a tenant administrator's setup on.
     *
     * It is stated beside @c tenant_bootstrap_mode rather than derived from it.
     * That flag is a setting under the tenant's system party, and the completing
     * step clears it as a warning rather than a condition: a tenant that is
     * already active can still report the flag as set, so the flag cannot answer
     * this question on its own.
     */
    bool tenant_bootstrapping = false;
    bool party_setup_required = false;
    /**
     * @brief Set when the party provisioner wizard has completed
     * (onboarding.party = true) but the party is still Inactive. The
     * client should show a message instead of re-launching the wizard.
     */
    std::string party_setup_warning;
    std::string token;
    std::string error_message;
    /**
     * @brief The stable code a client branches on.
     *
     * Empty when the sign-in succeeded. The message beside it is for a person
     * and may change; this is what a screen branches on, so a locked account is
     * distinguishable from a wrong password without matching English prose. The
     * codes a sign-in can answer with are collected on the Entry journeys page.
     */
    std::string error_code;
    std::string message;
    std::string selected_party_id;
    std::vector<party_summary> available_parties;
    /**
     * @brief The account's stored default party, if set and among
     * @c available_parties. Empty when unset. Only meaningful when
     * @c selected_party_id is empty (multi-party login, picker step).
     */
    std::string default_party_id;
    /**
     * @brief Token lifetime in seconds as configured on the server.
     *
     * Clients use this to arm the proactive refresh timer so that the
     * timer interval tracks any server-side configuration changes.
     */
    int access_lifetime_s = 1800;
    /**
     * @brief The IAM session UUID created for this login.
     *
     * Matches the session record in ores_iam_sessions_tbl. Clients should
     * forward this as Nats-Session-Id on every subsequent request so that
     * all calls from a single login session can be correlated in logs.
     */
    std::string session_id;
};

struct logout_request {
    using response_type = struct logout_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.logout";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct logout_response {
    bool success = false;
    std::string message;
};

struct public_key_request {
    static constexpr std::string_view nats_subject = "iam.v1.ops.public_key";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
};

/**
 * @brief Request to refresh a JWT token.
 *
 * The current token is passed in the Authorization: Bearer header.
 * No request body is needed — identity is taken from the token claims.
 */
struct refresh_request {
    using response_type = struct refresh_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.refresh";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief Response to a token refresh request.
 */
struct refresh_response {
    bool success = false;
    std::string token;
    std::string message;
    /**
     * @brief Token lifetime in seconds for the newly issued token.
     *
     * Clients re-arm the proactive refresh timer using this value.
     */
    int access_lifetime_s = 1800;
};

/**
 * @brief Authenticates a service account and issues a JWT.
 *
 * Service accounts cannot log in with the regular password-based login path.
 * They authenticate by presenting their database user password (which is
 * stored as a SHA-256 hash in the service account row). On success the IAM
 * service creates a session and returns a short-lived RS256 JWT identical in
 * structure to a human login token.
 *
 * The @p username must match the @c username column of an existing service
 * account (i.e. the database user name such as "ores_local1_reporting_service").
 * The @p password is the plaintext database password for that user.
 */
struct service_login_request {
    using response_type = struct service_login_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.service_login";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
    std::string username;
    std::string password;
};

struct service_login_response {
    bool success = false;
    std::string token;
    std::string message;
    int access_lifetime_s = 1800;
};

/**
 * @brief The members of party_summary, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_summary&) {
    return std::tuple{
        ::ores::utility::codec::field{"id", &party_summary::id},
        ::ores::utility::codec::field{"name", &party_summary::name},
        ::ores::utility::codec::field{"party_category", &party_summary::party_category},
        ::ores::utility::codec::field{"business_center_code", &party_summary::business_center_code},
    };
}

/**
 * @brief The members of database_info, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const database_info&) {
    return std::tuple{
        ::ores::utility::codec::field{"fingerprint", &database_info::fingerprint},
        ::ores::utility::codec::field{"environment", &database_info::environment},
        ::ores::utility::codec::field{"commit", &database_info::commit},
        ::ores::utility::codec::field{"created", &database_info::created},
    };
}

/**
 * @brief The members of login_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const login_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"principal", &login_request::principal},
        ::ores::utility::codec::field{"password", &login_request::password},
    };
}

/**
 * @brief The members of login_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const login_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &login_response::success},
        ::ores::utility::codec::field{"account_id", &login_response::account_id},
        ::ores::utility::codec::field{"tenant_id", &login_response::tenant_id},
        ::ores::utility::codec::field{"tenant_name", &login_response::tenant_name},
        ::ores::utility::codec::field{"version", &login_response::version},
        ::ores::utility::codec::field{"database", &login_response::database},
        ::ores::utility::codec::field{"username", &login_response::username},
        ::ores::utility::codec::field{"email", &login_response::email},
        ::ores::utility::codec::field{"password_reset_required",
                                      &login_response::password_reset_required},
        ::ores::utility::codec::field{"tenant_bootstrap_mode",
                                      &login_response::tenant_bootstrap_mode},
        ::ores::utility::codec::field{"tenant_bootstrapping",
                                      &login_response::tenant_bootstrapping},
        ::ores::utility::codec::field{"party_setup_required",
                                      &login_response::party_setup_required},
        ::ores::utility::codec::field{"party_setup_warning", &login_response::party_setup_warning},
        ::ores::utility::codec::field{"token", &login_response::token},
        ::ores::utility::codec::field{"error_message", &login_response::error_message},
        ::ores::utility::codec::field{"error_code", &login_response::error_code},
        ::ores::utility::codec::field{"message", &login_response::message},
        ::ores::utility::codec::field{"selected_party_id", &login_response::selected_party_id},
        ::ores::utility::codec::field{"available_parties", &login_response::available_parties},
        ::ores::utility::codec::field{"default_party_id", &login_response::default_party_id},
        ::ores::utility::codec::field{"access_lifetime_s", &login_response::access_lifetime_s},
        ::ores::utility::codec::field{"session_id", &login_response::session_id},
    };
}

/**
 * @brief The members of logout_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const logout_request&) {
    return std::tuple{};
}

/**
 * @brief The members of logout_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const logout_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &logout_response::success},
        ::ores::utility::codec::field{"message", &logout_response::message},
    };
}

/**
 * @brief The members of public_key_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const public_key_request&) {
    return std::tuple{};
}

/**
 * @brief The members of refresh_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const refresh_request&) {
    return std::tuple{};
}

/**
 * @brief The members of refresh_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const refresh_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &refresh_response::success},
        ::ores::utility::codec::field{"token", &refresh_response::token},
        ::ores::utility::codec::field{"message", &refresh_response::message},
        ::ores::utility::codec::field{"access_lifetime_s", &refresh_response::access_lifetime_s},
    };
}

/**
 * @brief The members of service_login_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const service_login_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"username", &service_login_request::username},
        ::ores::utility::codec::field{"password", &service_login_request::password},
    };
}

/**
 * @brief The members of service_login_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const service_login_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &service_login_response::success},
        ::ores::utility::codec::field{"token", &service_login_response::token},
        ::ores::utility::codec::field{"message", &service_login_response::message},
        ::ores::utility::codec::field{"access_lifetime_s",
                                      &service_login_response::access_lifetime_s},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::party_summary> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::party_summary& v);
    static ores::iam::messaging::party_summary read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::party_summary& v);
    static ores::iam::messaging::party_summary read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::database_info> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::database_info& v);
    static ores::iam::messaging::database_info read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::database_info& v);
    static ores::iam::messaging::database_info read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::login_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::login_request& v);
    static ores::iam::messaging::login_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::login_request& v);
    static ores::iam::messaging::login_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::login_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::login_response& v);
    static ores::iam::messaging::login_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::login_response& v);
    static ores::iam::messaging::login_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::logout_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::logout_request& v);
    static ores::iam::messaging::logout_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::logout_request& v);
    static ores::iam::messaging::logout_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::logout_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::logout_response& v);
    static ores::iam::messaging::logout_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::logout_response& v);
    static ores::iam::messaging::logout_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::public_key_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::public_key_request& v);
    static ores::iam::messaging::public_key_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::public_key_request& v);
    static ores::iam::messaging::public_key_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::refresh_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::refresh_request& v);
    static ores::iam::messaging::refresh_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::refresh_request& v);
    static ores::iam::messaging::refresh_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::refresh_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::refresh_response& v);
    static ores::iam::messaging::refresh_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::refresh_response& v);
    static ores::iam::messaging::refresh_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::service_login_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::service_login_request& v);
    static ores::iam::messaging::service_login_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::service_login_request& v);
    static ores::iam::messaging::service_login_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::service_login_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::service_login_response& v);
    static ores::iam::messaging::service_login_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::service_login_response& v);
    static ores::iam::messaging::service_login_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
