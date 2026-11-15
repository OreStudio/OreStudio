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
#ifndef ORES_HTTP_ROUTES_IAM_BROWSER_SESSION_HPP
#define ORES_HTTP_ROUTES_IAM_BROWSER_SESSION_HPP

#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <optional>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

/**
 * @brief The session a browser holds, as the BFF's session stated it.
 *
 * The gateway's session store keeps the token and its claims; these are what
 * the login answered beside them, which the browser reads back as its session.
 * The store keeps them as JSON in the record's view, so the store itself names
 * nothing of IAM.
 */
namespace ores::http::routes::iam {

/**
 * @brief A party the session may work in.
 */
struct browser_party final {
    std::string id;
    std::string name;
    std::string partyCategory;
    std::string businessCenterCode;

    friend bool operator==(const browser_party&, const browser_party&) = default;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<browser_party>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{
        field{"id", &browser_party::id},
        field{"name", &browser_party::name},
        field{"partyCategory", &browser_party::partyCategory},
        field{"businessCenterCode", &browser_party::businessCenterCode}};
}

/**
 * @brief The database the login answer carried, beside the build it states.
 */
struct browser_database final {
    std::string fingerprint;
    std::string environment;
    std::string commit;
    std::string created;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<browser_database>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"fingerprint", &browser_database::fingerprint},
                                              field{"environment", &browser_database::environment},
                                              field{"commit", &browser_database::commit},
                                              field{"created", &browser_database::created}};
}

/**
 * @brief The browser's session. The party is empty while the login waits for
 * one to be chosen, and the session's own read refuses it until then.
 */
struct browser_session final {
    std::string username;
    std::string email;
    std::string accountId;
    std::string tenantId;
    std::string tenantName;
    std::string mode;
    bool tenantBootstrapping = false;
    std::string version;
    browser_database database;
    std::optional<browser_party> party;
    std::vector<browser_party> availableParties;
    int accessLifetimeSeconds = 0;
    bool passwordResetRequired = false;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<browser_session>) {
    using ores::utility::codec::field;
    using s = browser_session;
    return ::ores::utility::codec::field_list{
        field{"username", &s::username},
        field{"email", &s::email},
        field{"accountId", &s::accountId},
        field{"tenantId", &s::tenantId},
        field{"tenantName", &s::tenantName},
        field{"mode", &s::mode},
        field{"tenantBootstrapping", &s::tenantBootstrapping},
        field{"version", &s::version},
        field{"database", &s::database},
        field{"party", &s::party},
        field{"availableParties", &s::availableParties},
        field{"accessLifetimeSeconds", &s::accessLifetimeSeconds},
        field{"passwordResetRequired", &s::passwordResetRequired}};
}

/**
 * @brief The context a session runs in, decided when the login is accepted:
 * the system tenant acts on the deployment, every other tenant is an application.
 */
[[nodiscard]] std::string mode_of(const std::string& tenant_id);

/**
 * @brief The browser's session a login answered, with the party it selected,
 * or none when the login waits for one to be chosen.
 */
[[nodiscard]] browser_session session_of(const ores::iam::messaging::login_response& login);

/**
 * @brief What a login that waits for a party answers: the parties offered and
 * the one quick sign-in would use.
 */
[[nodiscard]] std::string party_choice_json(const ores::iam::messaging::login_response& login);

}

#endif
