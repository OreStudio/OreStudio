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
#include "ores.http/routes/iam/role_write_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/iam/access_read_routes.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.iam.api/messaging/permission_protocol.hpp"
#include "ores.iam.api/messaging/role_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <functional>
#include <limits>
#include <regex>
#include <unordered_set>
#include <utility>

namespace ores::http::routes::iam {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace json = ores::utility::codec::json;
namespace messaging = ores::iam::messaging;
namespace rr = ores::http::net::record_routes;
using ores::http::net::record_routes::characters_of;
using ores::http::forwarding::guarded;
using ores::http::net::record_routes::refused;

namespace {

/// Whether a role is one a service holds, whose permissions no screen shows.
bool is_service_role(const ores::iam::domain::role& role) {
    return role.name.ends_with("Service");
}

/// The most a catalogue read holds, as the BFF read it.
constexpr std::uint32_t catalogue_page = 1000;

bool codes_fit(const std::vector<std::string>& codes) {
    return codes.size() <= 2000 && std::ranges::all_of(codes, [](const std::string& code) {
               const auto n = characters_of(code);
               return n >= 1 && n <= 200;
           });
}

bool note_fits(const std::optional<std::string>& note) {
    return !note || characters_of(*note) <= 2000;
}

std::optional<role_body> read_role(const std::string& body, bool edited) {
    auto role = json::read_expected<role_body>(body);
    if (!role)
        return std::nullopt;
    role->name = rr::trimmed(role->name);
    const auto name = characters_of(role->name);
    if (name < 1 || name > 100 || (role->description && characters_of(*role->description) > 1000))
        return std::nullopt;
    if (edited && (!role->version || *role->version < 0 ||
                   *role->version > std::numeric_limits<std::uint32_t>::max() ||
                   !role->registrationDefault || !role->requestable))
        return std::nullopt;
    return std::move(*role);
}

/// A role write's answer: the BFF answered a refused write as a conflict.
http_response written(const ores::utility::domain::result& result, std::string ok_body = {}) {
    if (result.outcome != ores::utility::domain::outcome::ok)
        return refused(http_status::conflict, "conflict", result.message);
    return ok_body.empty() ? rr::no_content() : http_response::json(ok_body);
}

messaging::put_role_request put_role_of(const boost::uuids::uuid& id,
                                        const role_body& role,
                                        std::optional<std::uint32_t> version,
                                        bool registration_default,
                                        bool requestable) {
    messaging::put_role_request out;
    out.change.write = {
        id, role.name, role.description.value_or(""), registration_default, requestable};
    out.change.precondition =
        version ?
            ores::utility::domain::precondition{
                ores::utility::domain::precondition_kind::must_match_version, version} :
            ores::utility::domain::precondition{
                ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return out;
}

/// Lock or unlock one account, refusing as the BFF did when the service would not.
template <typename Request>
http_response set_locked(const http_request& req, nats_client& session) {
    const auto account = req.get_path_param("accountId");
    if (account.empty())
        return refused(http_status::bad_request, "invalid-request", "An account id is required.");
    return guarded<Request>([&] {
        const auto reply = ores::http::forwarding::forward(req, session, Request{{account}});
        if (reply.results.empty() || !reply.results.front().success)
            return refused(http_status::conflict,
                           "invalid-request",
                           reply.results.empty() ? "The server refused the change." :
                                                   reply.results.front().message);
        return http_response::json(R"({"success":true})");
    });
}

std::vector<std::string>
permissions_of(const http_request& req, nats_client& session, const std::string& role_id) {
    const auto reply = ores::http::forwarding::forward(
        req, session, messaging::get_role_permissions_request{role_id});
    rr::require_ok(reply.result);
    return reply.permission_codes;
}

http_response save_permissions(const http_request& req,
                               nats_client& session,
                               const std::string& role_id,
                               std::vector<std::string> codes,
                               const std::string& note) {
    messaging::put_role_permissions_request msg;
    msg.role_id = role_id;
    msg.permission_codes = std::move(codes);
    msg.change_commentary = note;
    const auto reply = ores::http::forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
    return http_response::json("{\"codes\":" + json::write(reply.permission_codes) + "}");
}

/// The tenant's roles, each with what it grants; a service role's permissions
/// are not read, as no screen shows them.
std::vector<role_write_routes::role_summary> read_roles(const http_request& req,
                                                        nats_client& session) {
    messaging::list_roles_request list;
    list.limit = catalogue_page;
    const auto reply = ores::http::forwarding::forward(req, session, list);
    rr::require_ok(reply.result);
    std::vector<role_write_routes::role_summary> roles;
    for (const auto& role : reply.roles)
        roles.push_back({role,
                         is_service_role(role) ?
                             std::vector<std::string>{} :
                             permissions_of(req, session, boost::uuids::to_string(role.id))});
    return roles;
}

}

std::optional<give_role_body> role_write_routes::read_give_role(const std::string& body) {
    auto give = json::read_expected<give_role_body>(body);
    if (!give || !access_read_routes::is_uuid(give->roleId))
        return std::nullopt;
    const auto reason = characters_of(give->reasonCode);
    if (reason < 1 || reason > 100 || !note_fits(give->note))
        return std::nullopt;
    return std::move(*give);
}

std::optional<role_body> role_write_routes::read_new_role(const std::string& body) {
    return read_role(body, false);
}

std::optional<role_body> role_write_routes::read_edited_role(const std::string& body) {
    return read_role(body, true);
}

std::optional<permission_bundle_body> role_write_routes::read_bundle(const std::string& body) {
    auto bundle = json::read_expected<permission_bundle_body>(body);
    if (!bundle || !codes_fit(bundle->codes) || !note_fits(bundle->note))
        return std::nullopt;
    return std::move(*bundle);
}

std::optional<permission_changes_body> role_write_routes::read_changes(const std::string& body) {
    auto changes = json::read_expected<permission_changes_body>(body);
    if (!changes || !codes_fit(changes->add.value_or(std::vector<std::string>{})) ||
        !codes_fit(changes->remove.value_or(std::vector<std::string>{})))
        return std::nullopt;
    const auto note = characters_of(changes->note);
    if (note < 1 || note > 2000)
        return std::nullopt;
    return std::move(*changes);
}

std::vector<std::string> role_write_routes::apply_changes(std::vector<std::string> held,
                                                          const permission_changes_body& changes) {
    const auto& remove = changes.remove.value_or(std::vector<std::string>{});
    const std::unordered_set<std::string> removed(remove.begin(), remove.end());
    std::erase_if(held, [&](const std::string& code) { return removed.contains(code); });
    std::unordered_set<std::string> present(held.begin(), held.end());
    for (const auto& code : changes.add.value_or(std::vector<std::string>{}))
        if (present.insert(code).second)
            held.push_back(code);
    return held;
}

std::string role_write_routes::roles_json(const std::vector<role_summary>& roles) {
    std::string out = "{\"roles\":[";
    for (std::size_t i = 0; i < roles.size(); ++i) {
        const auto& r = roles[i].role;
        out += (i == 0 ? "{" : ",{");
        out += "\"id\":" + json::write(boost::uuids::to_string(r.id));
        out += ",\"version\":" + std::to_string(r.version);
        out += ",\"name\":" + json::write(r.name);
        out += ",\"description\":" + json::write(r.description);
        out += ",\"service\":" + std::string(is_service_role(r) ? "true" : "false");
        out +=
            ",\"registrationDefault\":" + std::string(r.is_registration_default ? "true" : "false");
        out += ",\"requestable\":" + std::string(r.is_requestable ? "true" : "false");
        out += ",\"permissionCodes\":" + json::write(roles[i].permission_codes) + "}";
    }
    return out + "]}";
}

std::string
role_write_routes::permissions_json(std::vector<ores::iam::domain::permission> permissions) {
    std::ranges::sort(permissions, {}, &ores::iam::domain::permission::code);
    std::string out = "{\"permissions\":[";
    for (std::size_t i = 0; i < permissions.size(); ++i)
        out += (i == 0 ? "" : ",") + std::string("{\"code\":") + json::write(permissions[i].code) +
               ",\"description\":" + json::write(permissions[i].description) + "}";
    return out + "]}";
}

// The code is one the route has read as area::resource:action, so its area is
// what precedes the first double colon.
bool role_write_routes::grants(const role_summary& role, const std::string& code) {
    const auto area = code.substr(0, code.find("::")) + "::*";
    return std::ranges::any_of(role.permission_codes, [&](const std::string& held) {
        return held == "*" || held == area || held == code;
    });
}

std::vector<role_write_routes::role_summary>
role_write_routes::ways_to_hold(const std::string& code,
                                const std::vector<role_summary>& roles,
                                const std::unordered_set<std::string>& held) {
    std::vector<role_summary> out;
    for (const auto& r : roles)
        if (r.role.is_requestable && !is_service_role(r.role) &&
            !held.contains(boost::uuids::to_string(r.role.id)) && grants(r, code))
            out.push_back(r);
    return out;
}

std::string role_write_routes::ways_json(const std::vector<role_summary>& roles) {
    std::string out = "{\"roles\":[";
    for (std::size_t i = 0; i < roles.size(); ++i)
        out += (i == 0 ? "" : ",") + std::string("{\"id\":") +
               json::write(boost::uuids::to_string(roles[i].role.id)) +
               ",\"name\":" + json::write(roles[i].role.name) +
               ",\"description\":" + json::write(roles[i].role.description) + "}";
    return out + "]}";
}

void role_write_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    const auto add = [&](ores::http::net::route_builder builder,
                         std::string summary,
                         std::function<http_response(const http_request&)> handle) {
        const auto built =
            builder.summary(std::move(summary))
                .tags({"access"})
                .auth_required()
                .handler([handle = std::move(handle)](const http_request& req)
                             -> boost::asio::awaitable<http_response> { co_return handle(req); })
                .build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->post("/api/accounts/{accountId}/roles"),
        "Give a role to an account, for a reason",
        [&session](const http_request& req) {
            const auto account = req.get_path_param("accountId");
            const auto give = read_give_role(req.body);
            if (!access_read_routes::is_uuid(account) || !give)
                return refused(
                    http_status::bad_request, "invalid-request", "Choose a role and a reason.");
            return guarded<messaging::assign_role_request>([&] {
                const auto reply = ores::http::forwarding::forward(
                    req,
                    session,
                    messaging::assign_role_request{
                        account, give->roleId, give->reasonCode, give->note.value_or("")});
                if (!reply.success)
                    return refused(http_status::conflict, "conflict", reply.error_message);
                return rr::no_content();
            });
        });

    add(router->delete_("/api/accounts/{accountId}/roles/{roleId}"),
        "Take a role away from an account",
        [&session](const http_request& req) {
            const auto account = req.get_path_param("accountId");
            const auto role = req.get_path_param("roleId");
            if (!access_read_routes::is_uuid(account) || !access_read_routes::is_uuid(role))
                return refused(
                    http_status::not_found, "not-found", "No such role on this account.");
            return guarded<messaging::revoke_role_request>([&] {
                const auto reply = ores::http::forwarding::forward(
                    req, session, messaging::revoke_role_request{account, role});
                if (!reply.success)
                    return refused(http_status::conflict, "conflict", reply.error_message);
                return rr::no_content();
            });
        });

    add(router->post("/api/accounts/{accountId}/lock"),
        "Lock one account",
        [&session](const http_request& req) {
            return set_locked<messaging::lock_account_request>(req, session);
        });

    add(router->post("/api/accounts/{accountId}/unlock"),
        "Unlock one account",
        [&session](const http_request& req) {
            return set_locked<messaging::unlock_account_request>(req, session);
        });

    add(router->get("/api/roles"),
        "Read the tenant's roles with the permissions each grants",
        [&session](const http_request& req) {
            return guarded<messaging::list_roles_request>([&] {
                const auto roles = read_roles(req, session);
                return http_response::json(roles_json(roles));
            });
        });

    add(router->get("/api/permissions"),
        "Read every permission the platform defines",
        [&session](const http_request& req) {
            return guarded<messaging::list_permissions_request>([&] {
                messaging::list_permissions_request list;
                list.limit = catalogue_page;
                const auto reply = ores::http::forwarding::forward(req, session, list);
                rr::require_ok(reply.result);
                return http_response::json(permissions_json(reply.permissions));
            });
        });

    add(router->post("/api/roles"), "Make a role", [&session](const http_request& req) {
        const auto role = read_new_role(req.body);
        if (!role)
            return refused(http_status::bad_request, "invalid-request", "A role needs a name.");
        const auto id = boost::uuids::random_generator()();
        return guarded<messaging::put_role_request>([&] {
            // A new role is requestable and not given at registration, whatever
            // the body says: the BFF made every new role this way, and the
            // editor sets the flags once the role exists.
            const auto reply = ores::http::forwarding::forward(
                req, session, put_role_of(id, *role, std::nullopt, false, true));
            return written(reply.result,
                           "{\"id\":" + json::write(boost::uuids::to_string(id)) + "}");
        });
    });

    add(router->put("/api/roles/{roleId}"), "Edit a role", [&session](const http_request& req) {
        const auto id = req.get_path_param("roleId");
        const auto role = read_edited_role(req.body);
        if (!access_read_routes::is_uuid(id) || !role)
            return refused(http_status::bad_request,
                           "invalid-request",
                           "A role needs a name and the version it was read at.");
        return guarded<messaging::put_role_request>([&] {
            const auto reply = ores::http::forwarding::forward(
                req,
                session,
                put_role_of(boost::uuids::string_generator()(id),
                            *role,
                            static_cast<std::uint32_t>(*role->version),
                            *role->registrationDefault,
                            *role->requestable));
            return written(reply.result);
        });
    });

    add(router->delete_("/api/roles/{name}"), "Remove a role", [&session](const http_request& req) {
        return guarded<messaging::delete_role_request>([&] {
            messaging::delete_role_request remove;
            remove.removal.key = messaging::role_key{req.get_path_param("name")};
            const auto reply = ores::http::forwarding::forward(req, session, remove);
            return written(reply.result);
        });
    });

    add(router->put("/api/roles/{roleId}/permissions"),
        "Write every permission a role grants",
        [&session](const http_request& req) {
            const auto id = req.get_path_param("roleId");
            const auto bundle = read_bundle(req.body);
            if (!access_read_routes::is_uuid(id) || !bundle)
                return refused(http_status::bad_request,
                               "invalid-request",
                               "Send the permissions the role grants.");
            return guarded<messaging::put_role_permissions_request>([&] {
                return save_permissions(req, session, id, bundle->codes, bundle->note.value_or(""));
            });
        });

    add(router->post("/api/roles/{roleId}/permissions/changes"),
        "Add and remove permissions from a role",
        [&session](const http_request& req) {
            const auto id = req.get_path_param("roleId");
            const auto changes = read_changes(req.body);
            if (!access_read_routes::is_uuid(id) || !changes)
                return refused(http_status::bad_request,
                               "invalid-request",
                               "Send the permissions to add or remove, and why.");
            return guarded<messaging::put_role_permissions_request>([&] {
                // The read and the write are two calls with no version between
                // them, as the BFF made them, so two changes to one role at the
                // same moment can each drop the other's.
                return save_permissions(req,
                                        session,
                                        id,
                                        apply_changes(permissions_of(req, session, id), *changes),
                                        changes->note);
            });
        });

    add(router->get("/api/me/ways-to-hold"),
        "Read the roles the caller could ask for that would give a permission",
        [&session](const http_request& req) {
            static const std::regex code_form("^[a-z][a-z0-9_]*::[a-z][a-z0-9_]*:[a-z][a-z0-9_]*$");
            const auto code = req.get_query_param("permission");
            if (!std::regex_match(code, code_form))
                return refused(http_status::bad_request,
                               "invalid-request",
                               "The permission asked about is not one.");
            // No way being known is an answer the screen states, so a failure
            // to read the roles answers with none, as the BFF did.
            try {
                const auto mine = ores::http::forwarding::forward(
                    req, session, messaging::get_my_roles_request{});
                rr::require_ok(mine.result);
                std::unordered_set<std::string> held;
                for (const auto& r : mine.roles)
                    held.insert(boost::uuids::to_string(r.role.id));
                return http_response::json(
                    ways_json(ways_to_hold(code, read_roles(req, session), held)));
            } catch (...) {
                return http_response::json(R"({"roles":[]})");
            }
        });
}

}
