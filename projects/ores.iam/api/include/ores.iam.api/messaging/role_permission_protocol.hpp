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
#ifndef ORES_IAM_API_MESSAGING_ROLE_PERMISSION_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ROLE_PERMISSION_PROTOCOL_HPP

#include "ores.iam.api/domain/role_permission.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct role_permission_key {
    boost::uuids::uuid role_id;
    boost::uuids::uuid permission_id;
};

struct role_permission_write {
    boost::uuids::uuid role_id;
    boost::uuids::uuid permission_id;
};

struct role_permission_change {
    role_permission_write write;
    ores::utility::domain::precondition precondition;
};

struct role_permission_removal {
    role_permission_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct role_permission_lookup {
    role_permission_key key;
    std::optional<ores::iam::domain::role_permission> role_permission;
};

struct role_permissions_filter {
    std::optional<boost::uuids::uuid> role_id;
    std::optional<boost::uuids::uuid> permission_id;
    std::optional<std::vector<boost::uuids::uuid>> role_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> permission_id_one_of;
};

struct role_permission_event {
    boost::uuids::uuid event_id;
    role_permission_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_role_permissions_request {
    using response_type = struct list_role_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.list";
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
    std::optional<role_permissions_filter> filter;
};

struct list_role_permissions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_permission> role_permissions;
    std::uint64_t total;
};

struct get_role_permission_request {
    using response_type = struct get_role_permission_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_permission_key key;
};

struct get_role_permission_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role_permission> role_permission;
};

struct get_many_role_permissions_request {
    using response_type = struct get_many_role_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_permission_key> keys;
};

struct get_many_role_permissions_response {
    ores::utility::domain::result result;
    std::vector<role_permission_lookup> entries;
};

struct put_role_permission_request {
    using response_type = struct put_role_permission_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_permission_change change;
    ores::utility::domain::change_intent intent;
};

struct put_role_permission_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role_permission> role_permission;
};

struct put_many_role_permissions_request {
    using response_type = struct put_many_role_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_permission_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_role_permissions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_permission> role_permissions;
};

struct delete_role_permission_request {
    using response_type = struct delete_role_permission_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_permission_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_role_permission_response {
    ores::utility::domain::result result;
};

struct delete_many_role_permissions_request {
    using response_type = struct delete_many_role_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_permission_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_role_permissions_response {
    ores::utility::domain::result result;
};

struct list_by_role_id_role_permissions_request {
    using response_type = struct list_by_role_id_role_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.list_by_role_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid role_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<role_permissions_filter> filter;
};

struct list_by_role_id_role_permissions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_permission> role_permissions;
    std::uint64_t total;
};

struct list_by_permission_id_role_permissions_request {
    using response_type = struct list_by_permission_id_role_permissions_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.role_permissions.list_by_permission_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid permission_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<role_permissions_filter> filter;
};

struct list_by_permission_id_role_permissions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_permission> role_permissions;
    std::uint64_t total;
};

}

#endif
