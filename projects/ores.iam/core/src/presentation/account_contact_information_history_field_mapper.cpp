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
#include "ores.iam.core/presentation/account_contact_information_history_field_mapper.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::iam::presentation {

std::vector<ores::diff::domain::field_value>
render_account_contact_information_fields(const domain::account_contact_information& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Account ID", .value = boost::uuids::to_string(v.account_id)});
    fields.push_back({.name = "Full Name", .value = v.full_name});
    fields.push_back({.name = "Street Line 1", .value = v.street_line_1});
    fields.push_back({.name = "Street Line 2", .value = v.street_line_2});
    fields.push_back({.name = "City", .value = v.city});
    fields.push_back({.name = "State", .value = v.state});
    fields.push_back({.name = "Country Code", .value = v.country_code});
    fields.push_back({.name = "Postal Code", .value = v.postal_code});
    fields.push_back({.name = "Phone", .value = v.phone});
    fields.push_back({.name = "Email", .value = v.email});
    fields.push_back({.name = "Web Page", .value = v.web_page});
    fields.push_back({.name = "Modified By", .value = v.modified_by});
    fields.push_back({.name = "Performed By", .value = v.performed_by});
    fields.push_back({.name = "Change Reason Code", .value = v.change_reason_code});
    fields.push_back({.name = "Change Commentary", .value = v.change_commentary});
    fields.push_back({.name = "Recorded At",
                      .value = ores::platform::time::datetime::to_iso8601_utc(v.recorded_at)});

    return fields;
}

}
