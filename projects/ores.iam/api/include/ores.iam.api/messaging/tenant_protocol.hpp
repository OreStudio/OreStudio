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
#ifndef ORES_IAM_API_MESSAGING_TENANT_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_TENANT_PROTOCOL_HPP

#include "ores.iam.api/domain/tenant.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct tenant_key {
    boost::uuids::uuid id;
};

struct tenant_write {
    boost::uuids::uuid id;
    std::string code;
    std::string name;
    std::string type;
    std::string description;
    std::string hostname;
    std::string status;
};

struct tenant_change {
    tenant_write write;
    ores::utility::domain::precondition precondition;
};

struct tenant_removal {
    tenant_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct tenant_lookup {
    tenant_key key;
    std::optional<ores::iam::domain::tenant> tenant;
};

struct tenant_event {
    boost::uuids::uuid event_id;
    tenant_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct tenant_version_key {
    tenant_key tenant;
    std::uint32_t version;
};

struct tenant_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_tenants_request {
    using response_type = struct list_tenants_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
};

struct list_tenants_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::tenant> tenants;
    std::uint64_t total;
};

struct get_tenant_request {
    using response_type = struct get_tenant_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.get";
    tenant_key key;
};

struct get_tenant_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::tenant> tenant;
};

struct get_many_tenants_request {
    using response_type = struct get_many_tenants_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.get_many";
    std::vector<tenant_key> keys;
};

struct get_many_tenants_response {
    ores::utility::domain::result result;
    std::vector<tenant_lookup> entries;
};

struct put_tenant_request {
    using response_type = struct put_tenant_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.put";
    tenant_change change;
    ores::utility::domain::change_intent intent;
};

struct put_tenant_response {
    ores::utility::domain::result result;
    ores::iam::domain::tenant tenant;
};

struct put_many_tenants_request {
    using response_type = struct put_many_tenants_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.put_many";
    std::vector<tenant_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_tenants_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::tenant> tenants;
};

struct delete_tenant_request {
    using response_type = struct delete_tenant_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.delete";
    tenant_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_tenant_response {
    ores::utility::domain::result result;
};

struct delete_many_tenants_request {
    using response_type = struct delete_many_tenants_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.delete_many";
    std::vector<tenant_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_tenants_response {
    ores::utility::domain::result result;
};

struct list_tenant_versions_request {
    using response_type = struct list_tenant_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants_versions.list";
    tenant_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tenant_versions_filter> filter;
};

struct list_tenant_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::tenant> versions;
    std::uint64_t total;
};

struct get_tenant_version_request {
    using response_type = struct get_tenant_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants_versions.get";
    tenant_version_key key;
};

struct get_tenant_version_response {
    ores::utility::domain::result result;
    ores::iam::domain::tenant version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace tenant_event_subjects {
inline constexpr std::string_view created = "iam.v1.tenants_events.created";
inline constexpr std::string_view updated = "iam.v1.tenants_events.updated";
inline constexpr std::string_view deleted = "iam.v1.tenants_events.deleted";
}

}

#endif
