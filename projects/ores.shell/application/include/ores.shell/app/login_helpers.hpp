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
#ifndef ORES_SHELL_APP_LOGIN_HELPERS_HPP
#define ORES_SHELL_APP_LOGIN_HELPERS_HPP

#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include <optional>
#include <ostream>
#include <string>
#include <utility>

namespace ores::shell::app::commands {

/**
 * @brief Persist a successful login in @p session.
 *
 * A login response with selected_party_id empty and available_parties
 * populated means the account spans several parties: iam issued a
 * single-use token restricted to party selection. Session-scoped
 * commands need the party-scoped access token that
 * iam.v1.accounts.select-party returns for the account's default party,
 * so exchange the token before storing it. Mirrors the Qt client's
 * ClientManager::selectParty().
 *
 * @return the selected party name when an exchange happened, an empty
 * string when the login token already carried a party scope, and
 * std::nullopt when no party could be selected. Failures are reported
 * through @p out and leave the session without auth.
 */
inline std::optional<std::string> complete_login(std::ostream& out,
                                                 ores::nats::service::nats_client& session,
                                                 const iam::messaging::login_response& response) {

    ores::nats::service::nats_client::login_info info;
    info.jwt = response.token;
    info.username = response.username;
    info.account_id = response.account_id;
    info.tenant_id = response.tenant_id;
    info.tenant_name = response.tenant_name;
    info.default_party_id = response.default_party_id;

    if (!response.selected_party_id.empty() || response.available_parties.empty()) {
        session.set_auth(std::move(info));
        return std::string();
    }

    // The login token only authorises party selection; select the
    // account's default party to obtain a party-scoped access token.
    std::string party_id = response.default_party_id;
    if (party_id.empty())
        party_id = response.available_parties[0].id;

    session.set_auth(info);
    auto selected = do_auth_request<iam::messaging::select_party_response>(
        out,
        session,
        iam::messaging::select_party_request::nats_subject,
        iam::messaging::select_party_request{.party_id = party_id});
    if (!selected || !selected->success) {
        // Do not leave the single-use login token as the session bearer.
        session.clear_auth();
        if (selected)
            fail(out) << "Failed to select party: " << selected->message << std::endl;
        return std::nullopt;
    }

    info.jwt = selected->token;
    session.set_auth(std::move(info));
    return selected->party_name;
}

}

#endif
