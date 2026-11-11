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
#include "ores.http/routes/iam/browser_session.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <algorithm>

namespace ores::http::routes::iam {

namespace json = ores::utility::codec::json;

namespace {

browser_party party_of(const ores::iam::messaging::party_summary& p) {
    return {p.id, p.name, p.party_category, p.business_center_code};
}

std::vector<browser_party> parties_of(const ores::iam::messaging::login_response& login) {
    std::vector<browser_party> out;
    for (const auto& p : login.available_parties)
        out.push_back(party_of(p));
    return out;
}

browser_database database_of(const ores::iam::messaging::login_response& login) {
    return {login.database.fingerprint,
            login.database.environment,
            login.database.commit,
            login.database.created};
}

}

std::string mode_of(const std::string& tenant_id) {
    return tenant_id == ores::http::net::record_routes::system_tenant_id ? "system-administration" :
                                                                           "application";
}

browser_session session_of(const ores::iam::messaging::login_response& login) {
    browser_session s;
    s.username = login.username;
    s.email = login.email;
    s.accountId = login.account_id;
    s.tenantId = login.tenant_id;
    s.tenantName = login.tenant_name;
    s.mode = mode_of(login.tenant_id);
    s.tenantBootstrapping = login.tenant_bootstrapping;
    s.version = login.version;
    s.database = database_of(login);
    s.availableParties = parties_of(login);
    s.accessLifetimeSeconds = login.access_lifetime_s;
    s.passwordResetRequired = login.password_reset_required;
    if (!login.selected_party_id.empty()) {
        const auto found = std::ranges::find(login.available_parties,
                                             login.selected_party_id,
                                             &ores::iam::messaging::party_summary::id);
        if (found != login.available_parties.end())
            s.party = party_of(*found);
    }
    return s;
}

std::string party_choice_json(const ores::iam::messaging::login_response& login) {
    return "{\"outcome\":\"party-required\",\"username\":" + json::write(login.username) +
           ",\"email\":" + json::write(login.email) +
           ",\"accountId\":" + json::write(login.account_id) +
           ",\"tenantName\":" + json::write(login.tenant_name) +
           ",\"tenantBootstrapping\":" + json::write(login.tenant_bootstrapping) +
           ",\"version\":" + json::write(login.version) +
           ",\"database\":" + json::write(database_of(login)) +
           ",\"availableParties\":" + json::write(parties_of(login)) + ",\"defaultPartyId\":" +
           (login.default_party_id.empty() ? std::string("null") :
                                             json::write(login.default_party_id)) +
           ",\"passwordResetRequired\":" + json::write(login.password_reset_required) +
           ",\"accessLifetimeSeconds\":" + std::to_string(login.access_lifetime_s) + "}";
}

}
