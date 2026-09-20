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
 * Template: cpp_history_field_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.iam.core/presentation/account_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::iam::presentation {

std::vector<ores::diff::domain::field_value> render_account_fields(const domain::account& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Username", .value = v.username});
    fields.push_back({.name = "Account Type", .value = v.account_type});
    fields.push_back({.name = "Full Name", .value = v.full_name});
    fields.push_back({.name = "Password Hash", .value = v.password_hash});
    fields.push_back({.name = "Password Salt", .value = v.password_salt});
    fields.push_back({.name = "Totp Secret", .value = v.totp_secret});
    fields.push_back({.name = "Email", .value = v.email});
    fields.push_back({.name = "Default Party ID",
                      .value = v.default_party_id ? boost::uuids::to_string(*v.default_party_id) :
                                                    std::string{}});
    fields.push_back({.name = "Image ID",
                      .value = v.image_id ? boost::uuids::to_string(*v.image_id) : std::string{}});
    fields.push_back({.name = "Job Title", .value = v.job_title});
    fields.push_back({.name = "Reports To Account ID",
                      .value = v.reports_to_account_id ?
                                   boost::uuids::to_string(*v.reports_to_account_id) :
                                   std::string{}});
    using ores::history::domain::provenance_fields;
    fields.push_back({.name = provenance_fields::modified_by, .value = v.modified_by});
    fields.push_back({.name = provenance_fields::performed_by, .value = v.performed_by});
    fields.push_back(
        {.name = provenance_fields::change_reason_code, .value = v.change_reason_code});
    fields.push_back({.name = provenance_fields::change_commentary, .value = v.change_commentary});
    fields.push_back({.name = provenance_fields::recorded_at,
                      .value = ores::platform::time::datetime::to_iso8601_utc(v.recorded_at)});

    return fields;
}

}
