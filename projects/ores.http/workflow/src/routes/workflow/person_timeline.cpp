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
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/workflow/timeline_sources.hpp"
#include "ores.iam.api/messaging/account_contact_information_protocol.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/auth_event_operations_protocol.hpp"
#include "ores.iam.api/messaging/role_protocol.hpp"
#include "ores.logging/logger.hpp"
#include <boost/uuid/string_generator.hpp>
#include <cstdint>
#include <exception>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::workflow {

namespace {

namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace iam_messaging = ores::iam::messaging;

using ores::http::domain::http_request;
using ores::nats::service::nats_client;

/// The trail the person's stream leaves when a source cannot be read or named.
auto& person_timeline_lg() {
    static auto instance = ores::logging::make_logger("ores.http.workflow.person_timeline");
    return instance;
}

/// The tables a person's stream draws from, named as the models name them.
constexpr std::string_view account_entity = "ores.iam.account";
constexpr std::string_view account_contact_entity = "ores.iam.account_contact_information";
constexpr std::string_view account_role_entity = "ores.iam.account_role";
constexpr std::string_view account_party_entity = "ores.iam.account_party";
constexpr std::string_view auth_event_entity = "ores.iam.auth_event";
constexpr std::string_view role_entity = "ores.iam.role";

/// How many sign-ins one person's stream carries.
constexpr std::uint32_t sign_in_page = 200;

/// The roles the catalogue holds fewer of than this, so one page reads them all.
constexpr std::uint32_t role_page = 1000;

/// What each sign-in event type reads as in the stream.
std::string sign_in_kind(const std::string& event_type) {
    if (event_type == "login_success")
        return "signed_in";
    if (event_type == "login_failure")
        return "sign_in_failed";
    if (event_type == "logout")
        return "signed_out";
    if (event_type == "token_refresh")
        return "refreshed";
    return "noticed";
}

/// Whether the path names the caller's own account, which the self read answers.
bool is_own_account(const http_request& req, const std::string& username) {
    const auto& claims = req.authenticated_user;
    return claims && claims->username && *claims->username == username;
}

/**
 * One account by username: the caller's own by the self read, which needs no
 * account permission, any other by the account read.
 *
 * A username nothing holds answers with nothing, which is what the person screen
 * answers for the same name. A read the caller may not make is refused, because a
 * stream that read as though the person did not exist would be a lie the screen
 * cannot see through.
 */
std::optional<ores::iam::domain::account>
account_of(const http_request& req, nats_client& session, const std::string& username) {
    if (is_own_account(req, username)) {
        const auto reply =
            forwarding::forward(req, session, iam_messaging::get_my_account_request{});
        rr::require_done(reply.result);
        return reply.account;
    }
    const auto reply = forwarding::forward(
        req, session, iam_messaging::get_account_request{iam_messaging::account_key{username}});
    if (reply.result.outcome == ores::utility::domain::outcome::missing)
        return std::nullopt;
    rr::require_ok(reply.result);
    return reply.account;
}

/// The roles a set of ids names, as the catalogue states them.
std::map<std::string, ores::iam::domain::role>
roles_of(const http_request& req, nats_client& session, const std::set<std::string>& ids) {
    std::map<std::string, ores::iam::domain::role> roles;
    if (ids.empty())
        return roles;
    std::vector<boost::uuids::uuid> wanted;
    for (const auto& id : ids) {
        try {
            wanted.push_back(boost::uuids::string_generator()(id));
        } catch (const std::exception& e) {
            ORES_LOG_SEV(person_timeline_lg(), ores::logging::warn)
                << "A role this stream points at is not named by a uuid, so it keeps its id: "
                << e.what();
        }
    }
    iam_messaging::list_roles_request msg;
    msg.limit = role_page;
    msg.filter = iam_messaging::roles_filter{wanted};
    const auto reply = forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
    for (const auto& role : reply.roles)
        roles[rr::as_text(role.id)] = role;
    return roles;
}

/// A role no longer held is still told, because the link's versions carry both
/// ends of it: a role given is a "granted" entry and a role taken away a "closed".
void name_roles(std::vector<story_event>& grants,
                const std::map<std::string, ores::iam::domain::role>& roles) {
    for (auto& event : grants) {
        if (event.kind == "raised")
            event.kind = "granted";
        std::vector<std::pair<std::string, std::string>> named;
        named.reserve(event.fields.size());
        for (const auto& [name, value] : event.fields) {
            const auto role = name == "Role" ? roles.find(value) : roles.end();
            if (role == roles.end()) {
                named.emplace_back(name, value);
                continue;
            }
            named.emplace_back("Role", role->second.name);
            named.emplace_back("Description", role->second.description);
        }
        event.fields = std::move(named);
    }
}

/// The sign-ins this reader may see, as the stream reads them.
std::vector<story_event>
sign_in_events(const http_request& req, nats_client& session, const std::string& account_id) {
    iam_messaging::list_auth_events_request msg;
    msg.account_id = account_id;
    msg.limit = sign_in_page;
    const auto reply = forwarding::forward(req, session, msg);
    if (!reply.success)
        rr::refuse(ores::http::domain::http_status::forbidden, "forbidden", reply.message);

    std::vector<story_event> events;
    events.reserve(reply.events.size());
    for (const auto& sign_in : reply.events) {
        story_event event;
        event.entity_type = std::string(auth_event_entity);
        event.entity_id = sign_in.id;
        event.kind = sign_in_kind(sign_in.event_type);
        event.at = sign_in.event_time;
        event.actor = sign_in.username;
        event.version = 1;
        event.fields.emplace_back("Event", sign_in.event_type);
        event.fields.emplace_back("Session", sign_in.session_id);
        event.fields.emplace_back("Party", sign_in.party_id);
        event.fields.emplace_back("Detail", sign_in.error_detail);
        events.push_back(std::move(event));
    }
    return events;
}

}

timeline_answer
person_timeline(const http_request& req, nats_client& session, const std::string& username) {
    timeline_answer answer;
    answer.subject = "person";
    answer.id = username;

    const auto account = account_of(req, session, username);
    if (!account)
        return answer;
    const auto account_id = rr::as_text(account->id);

    const auto account_versions = versions_or_gap(
        answer.gaps,
        account_entity,
        "The account's own versions are not shown: the service did not answer the read.",
        req,
        session,
        account_entity,
        account->username);

    const auto contact =
        or_gap(answer.gaps,
               account_contact_entity,
               "This person's contact record is not shown: the service did not answer the read.",
               [&]() -> std::optional<ores::iam::domain::account_contact_information> {
                   iam_messaging::list_by_account_id_account_contact_informations_request msg;
                   msg.account_id = account->id;
                   msg.limit = 1;
                   const auto reply = forwarding::forward(req, session, msg);
                   if (reply.result.outcome == ores::utility::domain::outcome::missing)
                       return std::nullopt;
                   rr::require_ok(reply.result);
                   if (reply.account_contact_informations.empty())
                       return std::nullopt;
                   return reply.account_contact_informations.front();
               });

    // The contact record's history is keyed by the address its model declares, not by the
    // row's id: reading it by the id answers with no versions, which reads on the stream
    // as a record that was never written.
    std::vector<story_event> contact_events;
    if (contact && !contact->email.empty())
        contact_events = story_record_events(
            account_contact_entity,
            rr::as_text(contact->id),
            versions_or_gap(
                answer.gaps,
                account_contact_entity,
                "The contact record's versions are not shown: the service did not answer the "
                "read.",
                req,
                session,
                account_contact_entity,
                contact->email));

    const auto role_versions = versions_or_gap(
        answer.gaps,
        account_role_entity,
        "The roles this person was given could not be read, so giving or taking one away is not "
        "shown.",
        req,
        session,
        account_role_entity,
        account_id);
    auto grants = story_link_events(account_role_entity, role_versions);
    const auto roles = or_gap(answer.gaps,
                              role_entity,
                              "The roles could not be read, so a role is named by its id.",
                              [&]() { return roles_of(req, session, ids_in(grants, "Role")); });
    name_roles(grants, roles);

    const auto party_links = versions_or_gap(
        answer.gaps,
        account_party_entity,
        "The parties this person works in could not be read, so opening or closing one is not "
        "shown.",
        req,
        session,
        account_party_entity,
        account_id);
    const auto links = story_link_events(account_party_entity, party_links);
    const auto named_links =
        with_names(links, "Party", party_names(req, session, ids_in(links, "Party"), answer.gaps));

    const auto sign_ins =
        or_gap(answer.gaps,
               auth_event_entity,
               "This person's sign-ins are not shown: the service did not answer the read.",
               [&]() { return sign_in_events(req, session, account_id); });

    // The read answers a window rather than everything, so the stream says which
    // part of the story it cannot show. It says so even for a person with no
    // sign-ins at all, because the window is a fact about the read and not about
    // the person.
    answer.gaps.push_back(
        {std::string(auth_event_entity),
         "The sign-in log keeps a rolling window rather than everything, so a sign-in older than "
         "the window is not shown. This stream reads the most recent " +
             std::to_string(sign_in_page) + "."});

    for (auto& event : story_record_events(account_entity, account_id, account_versions))
        answer.events.push_back(std::move(event));
    for (auto& event : contact_events)
        answer.events.push_back(std::move(event));
    for (auto& event : named_links)
        answer.events.push_back(std::move(event));
    for (auto& event : grants)
        answer.events.push_back(std::move(event));
    for (auto& event : sign_ins)
        answer.events.push_back(std::move(event));
    order_story(answer.events);
    return answer;
}

}
