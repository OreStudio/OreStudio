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
#ifndef ORES_IAM_API_MESSAGING_TENANT_STATUS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_TENANT_STATUS_PROTOCOL_HPP

#include "ores.iam.api/domain/tenant_status.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct tenant_status_key {
    std::string status;
};

struct tenant_status_write {
    std::string status;
    std::string name;
    std::string description;
    int display_order;
};

struct tenant_status_change {
    tenant_status_write write;
    ores::utility::domain::precondition precondition;
};

struct tenant_status_removal {
    tenant_status_key key;
    ores::utility::domain::precondition precondition;
};

struct tenant_status_lookup {
    tenant_status_key key;
    std::optional<ores::iam::domain::tenant_status> tenant_status;
};

struct tenant_status_version_key {
    tenant_status_key tenant_status;
    std::uint32_t version;
};

struct tenant_status_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_tenant_statuses_request {
    using response_type = struct list_tenant_statuses_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenant_statuses.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
};

struct list_tenant_statuses_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::tenant_status> statuses;
    std::uint64_t total;
};

struct get_tenant_status_request {
    using response_type = struct get_tenant_status_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenant_statuses.get";
    tenant_status_key key;
};

struct get_tenant_status_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::tenant_status> tenant_status;
};

struct get_many_tenant_statuses_request {
    using response_type = struct get_many_tenant_statuses_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenant_statuses.get_many";
    std::vector<tenant_status_key> keys;
};

struct get_many_tenant_statuses_response {
    ores::utility::domain::result result;
    std::vector<tenant_status_lookup> entries;
};

struct put_tenant_status_request {
    using response_type = struct put_tenant_status_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenant_statuses.put";
    tenant_status_change change;
    ores::utility::domain::change_intent intent;
};

struct put_tenant_status_response {
    ores::utility::domain::result result;
    ores::iam::domain::tenant_status tenant_status;
};

struct put_many_tenant_statuses_request {
    using response_type = struct put_many_tenant_statuses_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenant_statuses.put_many";
    std::vector<tenant_status_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_tenant_statuses_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::tenant_status> statuses;
};

struct delete_tenant_status_request {
    using response_type = struct delete_tenant_status_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenant_statuses.delete";
    tenant_status_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_tenant_status_response {
    ores::utility::domain::result result;
};

struct delete_many_tenant_statuses_request {
    using response_type = struct delete_many_tenant_statuses_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenant_statuses.delete_many";
    std::vector<tenant_status_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_tenant_statuses_response {
    ores::utility::domain::result result;
};

struct list_tenant_status_versions_request {
    using response_type = struct list_tenant_status_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenant_statuses_versions.list";
    tenant_status_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tenant_status_versions_filter> filter;
};

struct list_tenant_status_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::tenant_status> versions;
    std::uint64_t total;
};

struct get_tenant_status_version_request {
    using response_type = struct get_tenant_status_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenant_statuses_versions.get";
    tenant_status_version_key key;
};

struct get_tenant_status_version_response {
    ores::utility::domain::result result;
    ores::iam::domain::tenant_status version;
};

}

#endif
