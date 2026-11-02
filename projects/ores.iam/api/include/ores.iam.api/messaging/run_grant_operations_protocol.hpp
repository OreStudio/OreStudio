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
#ifndef ORES_IAM_API_MESSAGING_RUN_GRANT_OPERATIONS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_RUN_GRANT_OPERATIONS_PROTOCOL_HPP

#include "ores.iam.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::iam::messaging {

/**
 * @brief Records a person's consent that runs of one resource act for them.
 *
 * Sent on behalf of the person, with their token. The grant's tenant and
 * party are the session's, and its grantor is the session's account. Refused
 * when the session acts for no party, or when the person does not hold every
 * permission of the role. A second create for the same resource returns the
 * active grant, and re-activates a revoked one.
 */
struct create_run_grant_request {
    using response_type = struct create_run_grant_response;
    static constexpr std::string_view nats_subject = "iam.v1.run_grants.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief What the grant serves, as <component>.<entity>/<id>.
     */
    std::string resource;
    /**
     * @brief The name of the role a run token carries.
     */
    std::string role;
    /**
     * @brief The service names that may exchange the grant, comma-separated.
     */
    std::string audience;
    /**
     * @brief The number of runs the grant serves; zero for a standing grant.
     */
    int max_runs = 0;
    /**
     * @brief How long the grant serves runs; zero for a standing grant.
     */
    int valid_seconds = 0;
};

/**
 * @brief The grant, or why there is none.
 */
struct create_run_grant_response {
    bool success = false;
    std::string message;
    std::string grant_id;
    /**
     * @brief True when this request created or re-activated the grant; false
     * when it returned one that was already active.
     */
    bool created = false;
};

/**
 * @brief Ends a run grant.
 *
 * The grantor may revoke their own grant, and a holder of
 * iam::run_grants:revoke may revoke any grant of the tenant. Revoking a
 * revoked grant succeeds and changes nothing.
 */
struct revoke_run_grant_request {
    using response_type = struct revoke_run_grant_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.revoke_run_grant";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string grant_id;
    /**
     * @brief Why the grant ends: unscheduled, deleted, party_changed, or a
     * person's reason.
     */
    std::string reason;
};

/**
 * @brief Whether the grant is now revoked.
 */
struct revoke_run_grant_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Asks IAM for a run token, presenting a grant and a run.
 *
 * Sent by a step service in the grant's audience, with its own token. The
 * tenant is the one the run acts in, and must be the grant's. The run id
 * names the run the token serves, so two runs under one grant never share a
 * token.
 */
struct exchange_run_grant_request {
    using response_type = struct exchange_run_grant_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.exchange_run_grant";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string grant_id;
    std::string run_id;
    std::string tenant_id;
};

/**
 * @brief The run token, or why there is none.
 */
struct exchange_run_grant_response {
    bool success = false;
    std::string message;
    std::string token;
    /**
     * @brief When the token stops working, in seconds since the epoch.
     */
    std::int64_t expires_at = 0;
};

/**
 * @brief The members of create_run_grant_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const create_run_grant_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"resource", &create_run_grant_request::resource},
        ::ores::utility::codec::field{"role", &create_run_grant_request::role},
        ::ores::utility::codec::field{"audience", &create_run_grant_request::audience},
        ::ores::utility::codec::field{"max_runs", &create_run_grant_request::max_runs},
        ::ores::utility::codec::field{"valid_seconds", &create_run_grant_request::valid_seconds},
    };
}

/**
 * @brief The members of create_run_grant_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const create_run_grant_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &create_run_grant_response::success},
        ::ores::utility::codec::field{"message", &create_run_grant_response::message},
        ::ores::utility::codec::field{"grant_id", &create_run_grant_response::grant_id},
        ::ores::utility::codec::field{"created", &create_run_grant_response::created},
    };
}

/**
 * @brief The members of revoke_run_grant_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const revoke_run_grant_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"grant_id", &revoke_run_grant_request::grant_id},
        ::ores::utility::codec::field{"reason", &revoke_run_grant_request::reason},
    };
}

/**
 * @brief The members of revoke_run_grant_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const revoke_run_grant_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &revoke_run_grant_response::success},
        ::ores::utility::codec::field{"message", &revoke_run_grant_response::message},
    };
}

/**
 * @brief The members of exchange_run_grant_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const exchange_run_grant_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"grant_id", &exchange_run_grant_request::grant_id},
        ::ores::utility::codec::field{"run_id", &exchange_run_grant_request::run_id},
        ::ores::utility::codec::field{"tenant_id", &exchange_run_grant_request::tenant_id},
    };
}

/**
 * @brief The members of exchange_run_grant_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const exchange_run_grant_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &exchange_run_grant_response::success},
        ::ores::utility::codec::field{"message", &exchange_run_grant_response::message},
        ::ores::utility::codec::field{"token", &exchange_run_grant_response::token},
        ::ores::utility::codec::field{"expires_at", &exchange_run_grant_response::expires_at},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::create_run_grant_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::create_run_grant_request& v);
    static ores::iam::messaging::create_run_grant_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::create_run_grant_request& v);
    static ores::iam::messaging::create_run_grant_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::create_run_grant_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::create_run_grant_response& v);
    static ores::iam::messaging::create_run_grant_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::create_run_grant_response& v);
    static ores::iam::messaging::create_run_grant_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::revoke_run_grant_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::revoke_run_grant_request& v);
    static ores::iam::messaging::revoke_run_grant_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::revoke_run_grant_request& v);
    static ores::iam::messaging::revoke_run_grant_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::revoke_run_grant_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::revoke_run_grant_response& v);
    static ores::iam::messaging::revoke_run_grant_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::revoke_run_grant_response& v);
    static ores::iam::messaging::revoke_run_grant_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::exchange_run_grant_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::exchange_run_grant_request& v);
    static ores::iam::messaging::exchange_run_grant_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::exchange_run_grant_request& v);
    static ores::iam::messaging::exchange_run_grant_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::exchange_run_grant_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::exchange_run_grant_response& v);
    static ores::iam::messaging::exchange_run_grant_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::exchange_run_grant_response& v);
    static ores::iam::messaging::exchange_run_grant_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
