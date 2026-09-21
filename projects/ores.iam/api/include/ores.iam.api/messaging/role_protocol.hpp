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
#ifndef ORES_IAM_API_MESSAGING_ROLE_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ROLE_PROTOCOL_HPP

#include "ores.iam.api/domain/role.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct role_key {
    boost::uuids::uuid id;
};

struct role_write {
    boost::uuids::uuid id;
    std::string name;
    std::string description;
};

struct role_change {
    role_write write;
    ores::utility::domain::precondition precondition;
};

struct role_removal {
    role_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct role_lookup {
    role_key key;
    std::optional<ores::iam::domain::role> role;
};

struct role_version_key {
    role_key role;
    std::uint32_t version;
};

struct role_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_roles_request {
    using response_type = struct list_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
};

struct list_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role> roles;
    std::uint64_t total;
};

struct get_role_request {
    using response_type = struct get_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.get";
    role_key key;
};

struct get_role_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role> role;
};

struct get_many_roles_request {
    using response_type = struct get_many_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.get_many";
    std::vector<role_key> keys;
};

struct get_many_roles_response {
    ores::utility::domain::result result;
    std::vector<role_lookup> entries;
};

struct put_role_request {
    using response_type = struct put_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.put";
    role_change change;
    ores::utility::domain::change_intent intent;
};

struct put_role_response {
    ores::utility::domain::result result;
    ores::iam::domain::role role;
};

struct put_many_roles_request {
    using response_type = struct put_many_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.put_many";
    std::vector<role_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role> roles;
};

struct delete_role_request {
    using response_type = struct delete_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.delete";
    role_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_role_response {
    ores::utility::domain::result result;
};

struct delete_many_roles_request {
    using response_type = struct delete_many_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.delete_many";
    std::vector<role_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_roles_response {
    ores::utility::domain::result result;
};

struct list_role_versions_request {
    using response_type = struct list_role_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles_versions.list";
    role_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<role_versions_filter> filter;
};

struct list_role_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role> versions;
    std::uint64_t total;
};

struct get_role_version_request {
    using response_type = struct get_role_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles_versions.get";
    role_version_key key;
};

struct get_role_version_response {
    ores::utility::domain::result result;
    ores::iam::domain::role version;
};

}

#endif
