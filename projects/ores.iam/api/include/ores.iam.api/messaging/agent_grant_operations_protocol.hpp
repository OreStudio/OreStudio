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
#ifndef ORES_IAM_API_MESSAGING_AGENT_GRANT_OPERATIONS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_AGENT_GRANT_OPERATIONS_PROTOCOL_HPP

#include "ores.iam.api/domain/agent_call.hpp"
#include "ores.iam.api/domain/agent_grant.hpp"
#include "ores.iam.api/domain/agent_held_write.hpp"
#include "ores.iam.api/domain/agent_key.hpp"
#include "ores.iam.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief Records a person's consent that one named agent acts for them.
 *
 * Sent by the person, with their own session, who must hold
 * iam::agent_grants:create and every permission of the role. The grant's
 * tenant and party are the session's, and its grantor is the session's
 * account. Refused when the session names an agent, so an agent never makes a
 * grant, and when the role names a permission withheld from agents. The
 * response carries the grant's first key, the only time it is shown.
 */
struct create_agent_grant_request {
    using response_type = struct create_agent_grant_response;
    static constexpr std::string_view nats_subject = "iam.v1.agent_grants.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The name the person gives the agent.
     */
    std::string agent_name;
    /**
     * @brief The name of the role the agent acts under.
     */
    std::string role;
    /**
     * @brief How many days the grant serves, from one to ninety.
     */
    int valid_days = 30;

    friend bool operator==(const create_agent_grant_request&,
                           const create_agent_grant_request&) = default;
};

/**
 * @brief The grant and its first key, or why there is none.
 */
struct create_agent_grant_response {
    bool success = false;
    std::string message;
    std::string grant_id;
    /**
     * @brief The key, shown this once and never stored.
     */
    std::string key;

    friend bool operator==(const create_agent_grant_response&,
                           const create_agent_grant_response&) = default;
};

/**
 * @brief Makes another key for a grant, so a key can be replaced.
 *
 * Sent by the grant's person, with their own session.
 */
struct create_agent_key_request {
    using response_type = struct create_agent_key_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.create_agent_key";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string grant_id;

    friend bool operator==(const create_agent_key_request&,
                           const create_agent_key_request&) = default;
};

/**
 * @brief The new key, or why there is none.
 */
struct create_agent_key_response {
    bool success = false;
    std::string message;
    std::string key_id;
    /**
     * @brief The key, shown this once and never stored.
     */
    std::string key;

    friend bool operator==(const create_agent_key_response&,
                           const create_agent_key_response&) = default;
};

/**
 * @brief Ends a grant, and with it every key that redeems it.
 *
 * Sent by the grant's person or by an administrator.
 */
struct revoke_agent_grant_request {
    using response_type = struct revoke_agent_grant_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.revoke_agent_grant";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string grant_id;
    std::string reason;

    friend bool operator==(const revoke_agent_grant_request&,
                           const revoke_agent_grant_request&) = default;
};

/**
 * @brief Whether the grant was revoked.
 */
struct revoke_agent_grant_response {
    bool success = false;
    std::string message;

    friend bool operator==(const revoke_agent_grant_response&,
                           const revoke_agent_grant_response&) = default;
};

/**
 * @brief Ends one key, leaving the grant and its other keys.
 *
 * Sent by the grant's person or by an administrator.
 */
struct revoke_agent_key_request {
    using response_type = struct revoke_agent_key_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.revoke_agent_key";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string key_id;
    std::string reason;

    friend bool operator==(const revoke_agent_key_request&,
                           const revoke_agent_key_request&) = default;
};

/**
 * @brief Whether the key was revoked.
 */
struct revoke_agent_key_response {
    bool success = false;
    std::string message;

    friend bool operator==(const revoke_agent_key_response&,
                           const revoke_agent_key_response&) = default;
};

/**
 * @brief Asks IAM for a token, presenting an agent's key.
 *
 * Sent by ores.http, with its own token, when a request carries an agent key
 * as its bearer token. The key names the tenant to read it in.
 */
struct exchange_agent_key_request {
    using response_type = struct exchange_agent_key_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.exchange_agent_key";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string key;
    /**
     * @brief The agent's address as ores.http saw it, for its session and the log.
     */
    std::string client_address;

    friend bool operator==(const exchange_agent_key_request&,
                           const exchange_agent_key_request&) = default;
};

/**
 * @brief The token naming the person and the agent, or why there is none.
 */
struct exchange_agent_key_response {
    bool success = false;
    std::string message;
    std::string token;
    /**
     * @brief When the token stops working, in seconds since the epoch.
     */
    std::int64_t expires_at = 0;

    friend bool operator==(const exchange_agent_key_response&,
                           const exchange_agent_key_response&) = default;
};

/**
 * @brief Lists the caller's own agent grants and their keys.
 *
 * A self read: sent by a person who holds iam::agent_grants:create, with
 * their own session, and answering only grants whose grantor is the caller.
 * Reading another person's grants is the privileged read of the generated
 * entity protocol, which needs iam::agent_grants:read.
 */
struct list_own_agent_grants_request {
    using response_type = struct list_own_agent_grants_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.list_own_agent_grants";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const list_own_agent_grants_request&,
                           const list_own_agent_grants_request&) = default;
};

/**
 * @brief The caller's grants and their keys, never a key's secret.
 */
struct list_own_agent_grants_response {
    bool success = false;
    std::string message;
    std::vector<ores::iam::domain::agent_grant> grants;
    std::vector<ores::iam::domain::agent_key> keys;

    friend bool operator==(const list_own_agent_grants_response&,
                           const list_own_agent_grants_response&) = default;
};

/**
 * @brief Lists every agent grant and its keys in the caller's parties.
 *
 * The privileged read: it needs iam::agent_grants:read, which administrators
 * hold and no agent does. Row-level security keeps it to the parties the
 * caller sees.
 */
struct list_every_agent_grant_request {
    using response_type = struct list_every_agent_grant_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.list_every_agent_grant";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const list_every_agent_grant_request&,
                           const list_every_agent_grant_request&) = default;
};

/**
 * @brief Every grant the caller sees and its keys, never a key's secret or hash.
 */
struct list_every_agent_grant_response {
    bool success = false;
    std::string message;
    std::vector<ores::iam::domain::agent_grant> grants;
    std::vector<ores::iam::domain::agent_key> keys;

    friend bool operator==(const list_every_agent_grant_response&,
                           const list_every_agent_grant_response&) = default;
};

/**
 * @brief Lists the calls the caller's own LLM agents made, newest first.
 *
 * A self read, like the caller's own grants: it needs
 * iam::agent_grants:create and is refused to an agent.
 */
struct list_own_agent_activity_request {
    using response_type = struct list_own_agent_activity_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.list_own_agent_activity";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string grant_id;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;

    friend bool operator==(const list_own_agent_activity_request&,
                           const list_own_agent_activity_request&) = default;
};

/**
 * @brief The caller's agents' calls: route, method, answer and duration, never
 * a request or its answer.
 */
struct list_own_agent_activity_response {
    bool success = false;
    std::string message;
    std::vector<ores::iam::domain::agent_call> calls;

    friend bool operator==(const list_own_agent_activity_response&,
                           const list_own_agent_activity_response&) = default;
};

/**
 * @brief Asks for the token that replays an LLM agent's approved write.
 *
 * Only the gateway asks, under its own identity and iam::agent_keys:exchange,
 * once it has read the request's approval and claimed the held write. IAM
 * issues it only for a held write that is being applied, and checks the grant
 * and the person again, as an exchange does. Never a route.
 */
struct issue_agent_replay_token_request {
    using response_type = struct issue_agent_replay_token_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.issue_agent_replay_token";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string tenant_id;
    std::string request_id;

    friend bool operator==(const issue_agent_replay_token_request&,
                           const issue_agent_replay_token_request&) = default;
};

/**
 * @brief The replay token, naming the person and the agent, or a refusal.
 */
struct issue_agent_replay_token_response {
    bool success = false;
    std::string message;
    std::string token;

    friend bool operator==(const issue_agent_replay_token_response&,
                           const issue_agent_replay_token_response&) = default;
};

/**
 * @brief Reads the write an LLM agent made that an approval request holds.
 *
 * The review panel's read: the person the agent acts for reads their own, and
 * a holder of iam::agent_grants:read reads any in their parties. An agent never
 * reads one.
 */
struct get_agent_held_write_request {
    using response_type = struct get_agent_held_write_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_agent_held_write";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string request_id;

    friend bool operator==(const get_agent_held_write_request&,
                           const get_agent_held_write_request&) = default;
};

/**
 * @brief The held call, the agent that made it and what its replay answered.
 */
struct get_agent_held_write_response {
    bool success = false;
    std::string message;
    ores::iam::domain::agent_held_write write;
    std::string agent_name;

    friend bool operator==(const get_agent_held_write_response&,
                           const get_agent_held_write_response&) = default;
};

/**
 * @brief The members of create_agent_grant_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<create_agent_grant_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"agent_name", &create_agent_grant_request::agent_name},
        ::ores::utility::codec::field{"role", &create_agent_grant_request::role},
        ::ores::utility::codec::field{"valid_days", &create_agent_grant_request::valid_days},
    };
}

/**
 * @brief The members of create_agent_grant_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<create_agent_grant_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &create_agent_grant_response::success},
        ::ores::utility::codec::field{"message", &create_agent_grant_response::message},
        ::ores::utility::codec::field{"grant_id", &create_agent_grant_response::grant_id},
        ::ores::utility::codec::field{"key", &create_agent_grant_response::key},
    };
}

/**
 * @brief The members of create_agent_key_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<create_agent_key_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"grant_id", &create_agent_key_request::grant_id},
    };
}

/**
 * @brief The members of create_agent_key_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<create_agent_key_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &create_agent_key_response::success},
        ::ores::utility::codec::field{"message", &create_agent_key_response::message},
        ::ores::utility::codec::field{"key_id", &create_agent_key_response::key_id},
        ::ores::utility::codec::field{"key", &create_agent_key_response::key},
    };
}

/**
 * @brief The members of revoke_agent_grant_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<revoke_agent_grant_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"grant_id", &revoke_agent_grant_request::grant_id},
        ::ores::utility::codec::field{"reason", &revoke_agent_grant_request::reason},
    };
}

/**
 * @brief The members of revoke_agent_grant_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<revoke_agent_grant_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &revoke_agent_grant_response::success},
        ::ores::utility::codec::field{"message", &revoke_agent_grant_response::message},
    };
}

/**
 * @brief The members of revoke_agent_key_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<revoke_agent_key_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"key_id", &revoke_agent_key_request::key_id},
        ::ores::utility::codec::field{"reason", &revoke_agent_key_request::reason},
    };
}

/**
 * @brief The members of revoke_agent_key_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<revoke_agent_key_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &revoke_agent_key_response::success},
        ::ores::utility::codec::field{"message", &revoke_agent_key_response::message},
    };
}

/**
 * @brief The members of exchange_agent_key_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<exchange_agent_key_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"key", &exchange_agent_key_request::key},
        ::ores::utility::codec::field{"client_address",
                                      &exchange_agent_key_request::client_address},
    };
}

/**
 * @brief The members of exchange_agent_key_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<exchange_agent_key_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &exchange_agent_key_response::success},
        ::ores::utility::codec::field{"message", &exchange_agent_key_response::message},
        ::ores::utility::codec::field{"token", &exchange_agent_key_response::token},
        ::ores::utility::codec::field{"expires_at", &exchange_agent_key_response::expires_at},
    };
}

/**
 * @brief The members of list_own_agent_grants_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_own_agent_grants_request>) {
    return ::ores::utility::codec::field_list{};
}

/**
 * @brief The members of list_own_agent_grants_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_own_agent_grants_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &list_own_agent_grants_response::success},
        ::ores::utility::codec::field{"message", &list_own_agent_grants_response::message},
        ::ores::utility::codec::field{"grants", &list_own_agent_grants_response::grants},
        ::ores::utility::codec::field{"keys", &list_own_agent_grants_response::keys},
    };
}

/**
 * @brief The members of list_every_agent_grant_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_every_agent_grant_request>) {
    return ::ores::utility::codec::field_list{};
}

/**
 * @brief The members of list_every_agent_grant_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_every_agent_grant_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &list_every_agent_grant_response::success},
        ::ores::utility::codec::field{"message", &list_every_agent_grant_response::message},
        ::ores::utility::codec::field{"grants", &list_every_agent_grant_response::grants},
        ::ores::utility::codec::field{"keys", &list_every_agent_grant_response::keys},
    };
}

/**
 * @brief The members of list_own_agent_activity_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_own_agent_activity_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"grant_id", &list_own_agent_activity_request::grant_id},
        ::ores::utility::codec::field{"offset", &list_own_agent_activity_request::offset},
        ::ores::utility::codec::field{"limit", &list_own_agent_activity_request::limit},
    };
}

/**
 * @brief The members of list_own_agent_activity_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_own_agent_activity_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &list_own_agent_activity_response::success},
        ::ores::utility::codec::field{"message", &list_own_agent_activity_response::message},
        ::ores::utility::codec::field{"calls", &list_own_agent_activity_response::calls},
    };
}

/**
 * @brief The members of issue_agent_replay_token_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<issue_agent_replay_token_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"tenant_id", &issue_agent_replay_token_request::tenant_id},
        ::ores::utility::codec::field{"request_id", &issue_agent_replay_token_request::request_id},
    };
}

/**
 * @brief The members of issue_agent_replay_token_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<issue_agent_replay_token_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &issue_agent_replay_token_response::success},
        ::ores::utility::codec::field{"message", &issue_agent_replay_token_response::message},
        ::ores::utility::codec::field{"token", &issue_agent_replay_token_response::token},
    };
}

/**
 * @brief The members of get_agent_held_write_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_agent_held_write_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"request_id", &get_agent_held_write_request::request_id},
    };
}

/**
 * @brief The members of get_agent_held_write_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_agent_held_write_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &get_agent_held_write_response::success},
        ::ores::utility::codec::field{"message", &get_agent_held_write_response::message},
        ::ores::utility::codec::field{"write", &get_agent_held_write_response::write},
        ::ores::utility::codec::field{"agent_name", &get_agent_held_write_response::agent_name},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::create_agent_grant_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::create_agent_grant_request& v);
    static ores::iam::messaging::create_agent_grant_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::create_agent_grant_request& v);
    static ores::iam::messaging::create_agent_grant_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::create_agent_grant_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::create_agent_grant_response& v);
    static ores::iam::messaging::create_agent_grant_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::create_agent_grant_response& v);
    static ores::iam::messaging::create_agent_grant_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::create_agent_key_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::create_agent_key_request& v);
    static ores::iam::messaging::create_agent_key_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::create_agent_key_request& v);
    static ores::iam::messaging::create_agent_key_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::create_agent_key_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::create_agent_key_response& v);
    static ores::iam::messaging::create_agent_key_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::create_agent_key_response& v);
    static ores::iam::messaging::create_agent_key_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::revoke_agent_grant_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::revoke_agent_grant_request& v);
    static ores::iam::messaging::revoke_agent_grant_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::revoke_agent_grant_request& v);
    static ores::iam::messaging::revoke_agent_grant_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::revoke_agent_grant_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::revoke_agent_grant_response& v);
    static ores::iam::messaging::revoke_agent_grant_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::revoke_agent_grant_response& v);
    static ores::iam::messaging::revoke_agent_grant_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::revoke_agent_key_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::revoke_agent_key_request& v);
    static ores::iam::messaging::revoke_agent_key_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::revoke_agent_key_request& v);
    static ores::iam::messaging::revoke_agent_key_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::revoke_agent_key_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::revoke_agent_key_response& v);
    static ores::iam::messaging::revoke_agent_key_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::revoke_agent_key_response& v);
    static ores::iam::messaging::revoke_agent_key_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::exchange_agent_key_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::exchange_agent_key_request& v);
    static ores::iam::messaging::exchange_agent_key_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::exchange_agent_key_request& v);
    static ores::iam::messaging::exchange_agent_key_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::exchange_agent_key_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::exchange_agent_key_response& v);
    static ores::iam::messaging::exchange_agent_key_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::exchange_agent_key_response& v);
    static ores::iam::messaging::exchange_agent_key_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_own_agent_grants_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_own_agent_grants_request& v);
    static ores::iam::messaging::list_own_agent_grants_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_own_agent_grants_request& v);
    static ores::iam::messaging::list_own_agent_grants_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_own_agent_grants_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_own_agent_grants_response& v);
    static ores::iam::messaging::list_own_agent_grants_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_own_agent_grants_response& v);
    static ores::iam::messaging::list_own_agent_grants_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_every_agent_grant_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_every_agent_grant_request& v);
    static ores::iam::messaging::list_every_agent_grant_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_every_agent_grant_request& v);
    static ores::iam::messaging::list_every_agent_grant_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_every_agent_grant_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_every_agent_grant_response& v);
    static ores::iam::messaging::list_every_agent_grant_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_every_agent_grant_response& v);
    static ores::iam::messaging::list_every_agent_grant_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_own_agent_activity_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_own_agent_activity_request& v);
    static ores::iam::messaging::list_own_agent_activity_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_own_agent_activity_request& v);
    static ores::iam::messaging::list_own_agent_activity_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_own_agent_activity_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_own_agent_activity_response& v);
    static ores::iam::messaging::list_own_agent_activity_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_own_agent_activity_response& v);
    static ores::iam::messaging::list_own_agent_activity_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::issue_agent_replay_token_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::issue_agent_replay_token_request& v);
    static ores::iam::messaging::issue_agent_replay_token_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::issue_agent_replay_token_request& v);
    static ores::iam::messaging::issue_agent_replay_token_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::issue_agent_replay_token_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::issue_agent_replay_token_response& v);
    static ores::iam::messaging::issue_agent_replay_token_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::issue_agent_replay_token_response& v);
    static ores::iam::messaging::issue_agent_replay_token_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_agent_held_write_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_agent_held_write_request& v);
    static ores::iam::messaging::get_agent_held_write_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_agent_held_write_request& v);
    static ores::iam::messaging::get_agent_held_write_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_agent_held_write_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_agent_held_write_response& v);
    static ores::iam::messaging::get_agent_held_write_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_agent_held_write_response& v);
    static ores::iam::messaging::get_agent_held_write_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
