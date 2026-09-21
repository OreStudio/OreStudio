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
#ifndef ORES_IAM_API_MESSAGING_PERMISSION_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_PERMISSION_PROTOCOL_HPP

#include "ores.iam.api/domain/permission.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct permission_key {
    boost::uuids::uuid id;
};

struct permission_write {
    boost::uuids::uuid id;
    std::string code;
    std::string description;
};

struct permission_change {
    permission_write write;
    ores::utility::domain::precondition precondition;
};

struct permission_removal {
    permission_key key;
    ores::utility::domain::precondition precondition;
};

struct permission_lookup {
    permission_key key;
    std::optional<ores::iam::domain::permission> permission;
};

struct list_permissions_request {
    using response_type = struct list_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
};

struct list_permissions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::permission> permissions;
    std::uint64_t total;
};

struct get_permission_request {
    using response_type = struct get_permission_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.get";
    permission_key key;
};

struct get_permission_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::permission> permission;
};

struct get_many_permissions_request {
    using response_type = struct get_many_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.get_many";
    std::vector<permission_key> keys;
};

struct get_many_permissions_response {
    ores::utility::domain::result result;
    std::vector<permission_lookup> entries;
};

struct put_permission_request {
    using response_type = struct put_permission_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.put";
    permission_change change;
    ores::utility::domain::change_intent intent;
};

struct put_permission_response {
    ores::utility::domain::result result;
    ores::iam::domain::permission permission;
};

struct put_many_permissions_request {
    using response_type = struct put_many_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.put_many";
    std::vector<permission_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_permissions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::permission> permissions;
};

struct delete_permission_request {
    using response_type = struct delete_permission_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.delete";
    permission_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_permission_response {
    ores::utility::domain::result result;
};

struct delete_many_permissions_request {
    using response_type = struct delete_many_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.delete_many";
    std::vector<permission_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_permissions_response {
    ores::utility::domain::result result;
};

}

#endif
