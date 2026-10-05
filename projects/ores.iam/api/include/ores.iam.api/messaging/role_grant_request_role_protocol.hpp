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
#ifndef ORES_IAM_API_MESSAGING_ROLE_GRANT_REQUEST_ROLE_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ROLE_GRANT_REQUEST_ROLE_PROTOCOL_HPP

#include "ores.iam.api/domain/role_grant_request_role.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct role_grant_request_role_key {
    boost::uuids::uuid request_id;
    boost::uuids::uuid role_id;
};

struct role_grant_request_role_write {
    boost::uuids::uuid request_id;
    boost::uuids::uuid role_id;
    std::optional<std::chrono::system_clock::time_point> applied_at;
};

struct role_grant_request_role_change {
    role_grant_request_role_write write;
    ores::utility::domain::precondition precondition;
};

struct role_grant_request_role_removal {
    role_grant_request_role_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct role_grant_request_role_lookup {
    role_grant_request_role_key key;
    std::optional<ores::iam::domain::role_grant_request_role> role_grant_request_role;
};

struct role_grant_request_roles_filter {
    std::optional<boost::uuids::uuid> request_id;
    std::optional<std::vector<boost::uuids::uuid>> request_id_one_of;
};

struct list_role_grant_request_roles_request {
    using response_type = struct list_role_grant_request_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_request_roles.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<role_grant_request_roles_filter> filter;
};

struct list_role_grant_request_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_grant_request_role> role_grant_request_roles;
    std::uint64_t total;
};

struct get_role_grant_request_role_request {
    using response_type = struct get_role_grant_request_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_request_roles.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_grant_request_role_key key;
};

struct get_role_grant_request_role_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role_grant_request_role> role_grant_request_role;
};

struct get_many_role_grant_request_roles_request {
    using response_type = struct get_many_role_grant_request_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_request_roles.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_grant_request_role_key> keys;
};

struct get_many_role_grant_request_roles_response {
    ores::utility::domain::result result;
    std::vector<role_grant_request_role_lookup> entries;
};

struct put_role_grant_request_role_request {
    using response_type = struct put_role_grant_request_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_request_roles.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_grant_request_role_change change;
    ores::utility::domain::change_intent intent;
};

struct put_role_grant_request_role_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role_grant_request_role> role_grant_request_role;
};

struct put_many_role_grant_request_roles_request {
    using response_type = struct put_many_role_grant_request_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_request_roles.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_grant_request_role_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_role_grant_request_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_grant_request_role> role_grant_request_roles;
};

struct delete_role_grant_request_role_request {
    using response_type = struct delete_role_grant_request_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_request_roles.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_grant_request_role_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_role_grant_request_role_response {
    ores::utility::domain::result result;
};

struct delete_many_role_grant_request_roles_request {
    using response_type = struct delete_many_role_grant_request_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_request_roles.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_grant_request_role_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_role_grant_request_roles_response {
    ores::utility::domain::result result;
};

struct list_by_request_id_role_grant_request_roles_request {
    using response_type = struct list_by_request_id_role_grant_request_roles_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.role_grant_request_roles.list_by_request_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid request_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<role_grant_request_roles_filter> filter;
};

struct list_by_request_id_role_grant_request_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_grant_request_role> role_grant_request_roles;
    std::uint64_t total;
};

}

#endif
