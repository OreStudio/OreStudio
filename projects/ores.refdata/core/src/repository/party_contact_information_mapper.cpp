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
#include "ores.refdata.core/repository/party_contact_information_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/party_contact_information_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::party_contact_information
party_contact_information_mapper::map(const party_contact_information_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::party_contact_information r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);


    r.contact_type = v.contact_type;

    r.street_line_1 = v.street_line_1.value_or("");
    r.street_line_2 = v.street_line_2.value_or("");
    r.city = v.city.value_or("");
    r.state = v.state.value_or("");
    r.country_code = v.country_code.value_or("");
    r.postal_code = v.postal_code.value_or("");
    r.phone = v.phone.value_or("");
    r.email = v.email.value_or("");
    r.web_page = v.web_page.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

party_contact_information_entity
party_contact_information_mapper::map(const domain::party_contact_information& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    party_contact_information_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);


    r.contact_type = v.contact_type;

    r.street_line_1 = v.street_line_1.empty() ? std::nullopt : std::optional(v.street_line_1);
    r.street_line_2 = v.street_line_2.empty() ? std::nullopt : std::optional(v.street_line_2);
    r.city = v.city.empty() ? std::nullopt : std::optional(v.city);
    r.state = v.state.empty() ? std::nullopt : std::optional(v.state);
    r.country_code = v.country_code.empty() ? std::nullopt : std::optional(v.country_code);
    r.postal_code = v.postal_code.empty() ? std::nullopt : std::optional(v.postal_code);
    r.phone = v.phone.empty() ? std::nullopt : std::optional(v.phone);
    r.email = v.email.empty() ? std::nullopt : std::optional(v.email);
    r.web_page = v.web_page.empty() ? std::nullopt : std::optional(v.web_page);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::party_contact_information>
party_contact_information_mapper::map(const std::vector<party_contact_information_entity>& v) {
    return map_vector<party_contact_information_entity, domain::party_contact_information>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<party_contact_information_entity>
party_contact_information_mapper::map(const std::vector<domain::party_contact_information>& v) {
    return map_vector<domain::party_contact_information, party_contact_information_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
