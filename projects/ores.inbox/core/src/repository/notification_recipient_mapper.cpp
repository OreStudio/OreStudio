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
 * Template: cpp_domain_type_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.inbox.core/repository/notification_recipient_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.inbox.api/domain/notification_recipient_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::inbox::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::notification_recipient
notification_recipient_mapper::map(const notification_recipient_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::notification_recipient r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.notification_id = boost::lexical_cast<boost::uuids::uuid>(v.notification_id.value());
    r.account_id = boost::lexical_cast<boost::uuids::uuid>(v.account_id);
    r.read_at =
        v.read_at.has_value() ? std::optional(timestamp_to_timepoint(*v.read_at)) : std::nullopt;
    r.cleared_at = v.cleared_at.has_value() ? std::optional(timestamp_to_timepoint(*v.cleared_at)) :
                                              std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

notification_recipient_entity
notification_recipient_mapper::map(const domain::notification_recipient& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    notification_recipient_entity r;
    r.notification_id = boost::uuids::to_string(v.notification_id);
    r.tenant_id = v.tenant_id;
    r.account_id = boost::uuids::to_string(v.account_id);
    r.version = v.version;
    r.read_at = v.read_at.has_value() ?
                    std::optional(ores::platform::time::datetime::to_db_string(*v.read_at)) :
                    std::nullopt;
    r.cleared_at = v.cleared_at.has_value() ?
                       std::optional(ores::platform::time::datetime::to_db_string(*v.cleared_at)) :
                       std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::notification_recipient>
notification_recipient_mapper::map(const std::vector<notification_recipient_entity>& v) {
    return map_vector<notification_recipient_entity, domain::notification_recipient>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<notification_recipient_entity>
notification_recipient_mapper::map(const std::vector<domain::notification_recipient>& v) {
    return map_vector<domain::notification_recipient, notification_recipient_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
