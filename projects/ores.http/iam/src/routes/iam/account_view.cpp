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
#include "ores.http/routes/iam/account_view.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <array>
#include <string_view>

namespace ores::http::routes::iam::account_view {

namespace json = ores::utility::codec::json;

namespace {

constexpr std::array<std::string_view, 4> account_types{"user", "service", "algorithm", "llm"};

/// A reference to another record, or null when there is none. The service
/// states no reference as the nil identifier as well as by absence, and the
/// screens read both as null, as the BFF's mapAccount did.
std::string reference(const std::optional<boost::uuids::uuid>& id) {
    if (!id || id->is_nil())
        return "null";
    return json::write(boost::uuids::to_string(*id));
}

}

std::string json_of(const ores::iam::domain::account& a) {
    const auto type = std::ranges::find(account_types, a.account_type) == account_types.end() ?
                          "user" :
                          a.account_type;
    std::string out = "{\"version\":" + std::to_string(a.version);
    out += ",\"id\":" + json::write(boost::uuids::to_string(a.id));
    out += ",\"tenantId\":" + json::write(a.tenant_id.to_string());
    out += ",\"username\":" + json::write(a.username);
    out += ",\"fullName\":" + json::write(a.full_name);
    out += ",\"email\":" + json::write(a.email);
    out += ",\"accountType\":" + json::write(type);
    out += ",\"jobTitle\":" + json::write(a.job_title);
    out += ",\"reportsToAccountId\":" + reference(a.reports_to_account_id);
    out += ",\"defaultPartyId\":" + reference(a.default_party_id);
    out += ",\"imageId\":" + reference(a.image_id);
    out += ",\"modifiedBy\":" + json::write(a.modified_by);
    out += ",\"changeReasonCode\":" + json::write(a.change_reason_code);
    out += ",\"changeCommentary\":" + json::write(a.change_commentary);
    out += ",\"performedBy\":" + json::write(a.performed_by);
    out += ",\"recordedAt\":" + json::write(a.recorded_at);
    return out + "}";
}

std::string json_of(const std::optional<ores::iam::domain::account>& account) {
    return account ? json_of(*account) : std::string("null");
}

}
