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
#ifndef ORES_IAM_API_MESSAGING_ACCOUNT_ROLE_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ACCOUNT_ROLE_PROTOCOL_HPP

#include "ores.iam.api/domain/account_role.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct account_role_key {
    boost::uuids::uuid account_id;
    boost::uuids::uuid role_id;
};

struct account_role_write {
    boost::uuids::uuid account_id;
    boost::uuids::uuid role_id;
};

struct account_role_change {
    account_role_write write;
    ores::utility::domain::precondition precondition;
};

struct account_role_removal {
    account_role_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct account_role_lookup {
    account_role_key key;
    std::optional<ores::iam::domain::account_role> account_role;
};

struct account_roles_filter {
    std::optional<boost::uuids::uuid> account_id;
    std::optional<boost::uuids::uuid> role_id;
    std::optional<std::vector<boost::uuids::uuid>> account_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> role_id_one_of;
};

struct list_account_roles_request {
    using response_type = struct list_account_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.list";
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
    std::optional<account_roles_filter> filter;
};

struct list_account_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_role> account_roles;
    std::uint64_t total;
};

struct get_account_role_request {
    using response_type = struct get_account_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_role_key key;
};

struct get_account_role_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account_role> account_role;
};

struct get_many_account_roles_request {
    using response_type = struct get_many_account_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_role_key> keys;
};

struct get_many_account_roles_response {
    ores::utility::domain::result result;
    std::vector<account_role_lookup> entries;
};

struct put_account_role_request {
    using response_type = struct put_account_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_role_change change;
    ores::utility::domain::change_intent intent;
};

struct put_account_role_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account_role> account_role;
};

struct put_many_account_roles_request {
    using response_type = struct put_many_account_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_role_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_account_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_role> account_roles;
};

struct delete_account_role_request {
    using response_type = struct delete_account_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_role_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_account_role_response {
    ores::utility::domain::result result;
};

struct delete_many_account_roles_request {
    using response_type = struct delete_many_account_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_role_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_account_roles_response {
    ores::utility::domain::result result;
};

struct list_by_account_id_account_roles_request {
    using response_type = struct list_by_account_id_account_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.list_by_account_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid account_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<account_roles_filter> filter;
};

struct list_by_account_id_account_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_role> account_roles;
    std::uint64_t total;
};

struct list_by_role_id_account_roles_request {
    using response_type = struct list_by_role_id_account_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.list_by_role_id";
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
    std::optional<account_roles_filter> filter;
};

struct list_by_role_id_account_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_role> account_roles;
    std::uint64_t total;
};

}

#endif
