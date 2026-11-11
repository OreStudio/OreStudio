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
#include "ores.http/routes/iam/browser_session_routes.hpp"
#include "ores.http.api/net/rate_limiter.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/iam/access_screen_shapes.hpp"
#include "ores.http/routes/iam/tenant_parties.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.iam.api/messaging/bootstrap_protocol.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <exception>
#include <utility>

namespace ores::http::routes::iam {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::domain::session_cookie;
using ores::http::domain::session_record;
namespace json = ores::utility::codec::json;
namespace messaging = ores::iam::messaging;
namespace rr = ores::http::net::record_routes;

namespace {

http_response not_signed_in() {
    return rr::refused(http_status::unauthorized, "not-authenticated", "Sign in to continue.");
}

http_response session_ended() {
    return rr::refused(
        http_status::unauthorized, "session-expired", "Your session has ended. Sign in again.");
}

std::optional<browser_session> view_of(const session_record& record) {
    auto read = json::read_expected<browser_session>(record.view);
    if (!read)
        return std::nullopt;
    return std::move(*read);
}

/// The setup run's state ores.iam names on a tenant's own setup run.
constexpr std::string_view tenant_setup_run_type = "tenant_setup_workflow";

}

browser_session_routes::browser_session_routes(
    ores::nats::service::nats_client& session,
    std::shared_ptr<http::domain::session_store> sessions,
    verifier_finder find_verifier,
    http::domain::cookie_settings cookie)
    : session_(session)
    , sessions_(std::move(sessions))
    , find_verifier_(std::move(find_verifier))
    , cookie_(std::move(cookie)) {}

std::string browser_session_routes::login_json(const messaging::login_response& login) {
    const auto view = session_of(login);
    if (!view.party)
        return party_choice_json(login);
    return "{\"outcome\":\"active\",\"session\":" + json::write(view) + "}";
}

std::optional<std::string> browser_session_routes::identifier_of(const http_request& req) const {
    return session_cookie::read(req.get_header("cookie"), cookie_);
}

std::optional<ores::security::jwt::jwt_claims>
browser_session_routes::claims_of(const std::string& token) const {
    const auto verifier = find_verifier_ ? find_verifier_() : nullptr;
    if (!verifier || !verifier->is_configured())
        return std::nullopt;
    auto claims = verifier->validate(token);
    if (!claims)
        return std::nullopt;
    return std::move(claims.value());
}

http_response browser_session_routes::sign_in(const http_request& req) {
    const auto body = json::read_expected<credentials_body>(req.body);
    if (!body || body->username.empty() || body->password.empty())
        return rr::refused(
            http_status::bad_request, "invalid-request", "A username and password are required.");
    return ores::http::forwarding::guarded<messaging::login_request>([&] {
        /*
         * Asked before the credentials are used: a deployment in bootstrap mode has
         * no accounts, and a refused login would send somebody hunting for a
         * password that cannot exist.
         */
        const auto status =
            ores::http::forwarding::forward(req, session_, messaging::bootstrap_status_request{});
        if (status.is_in_bootstrap_mode)
            return rr::refused(http_status::conflict,
                               "bootstrap-mode",
                               "This deployment is in bootstrap mode: it has not been provisioned "
                               "yet, so there are no accounts to sign in with. An administrator "
                               "must complete the setup wizard first.");
        messaging::login_request login_msg;
        login_msg.principal = body->username;
        login_msg.password = body->password;
        const auto login = ores::http::forwarding::forward(req, session_, login_msg);
        if (!login.success || login.token.empty()) {
            const auto& words = !login.error_message.empty() ? login.error_message : login.message;
            return rr::refused(http_status::unauthorized,
                               "invalid-credentials",
                               words.empty() ? "Invalid username or password." : words);
        }
        auto claims = claims_of(login.token);
        if (!claims) {
            BOOST_LOG_SEV(lg(), ores::logging::error)
                << "Signing in failed: the token IAM minted does not verify";
            return http_response::internal_error("The token could not be read");
        }
        // A sign-in replaces whatever session the browser held, so the old one ends now.
        if (const auto previous = identifier_of(req))
            sessions_->close(*previous);
        session_record record;
        record.token = login.token;
        record.claims = std::move(*claims);
        record.view = json::write(session_of(login));
        auto response = http_response::json(login_json(login));
        response.set_header("Set-Cookie",
                            session_cookie::issue(cookie_, sessions_->open(std::move(record))));
        return response;
    });
}

http_response browser_session_routes::read(const http_request& req) {
    const auto id = identifier_of(req);
    const auto record = id ? sessions_->find(*id) : std::nullopt;
    auto view = record ? view_of(*record) : std::nullopt;
    if (record && !view)
        BOOST_LOG_SEV(lg(), ores::logging::warn)
            << "A live session holds a view that cannot be read; the browser is sent to sign in";
    // A login that still waits on a party has no session to show yet.
    if (!view || !view->party)
        return not_signed_in();
    if (view->tenantBootstrapping) {
        /*
         * The flag is the login's snapshot, so a session opened while its tenant was
         * setting up still says so after the run ends. The tenant's own setup run is
         * what the rail follows; a read that fails leaves the snapshot as it stands.
         */
        try {
            ores::workflow::messaging::list_workflow_instance_summaries_request runs;
            runs.limit = 1;
            runs.type_filter = std::string(tenant_setup_run_type);
            const auto reply = ores::http::forwarding::forward(req, session_, runs);
            if (reply.success && !reply.instances.empty() &&
                reply.instances.front().status == "completed") {
                view->tenantBootstrapping = false;
                // Only the flag changes, read again under the lock, so a party
                // chosen while the run was read is not written back over.
                sessions_->update(*id, [](session_record& r) {
                    if (auto current = view_of(r)) {
                        current->tenantBootstrapping = false;
                        r.view = json::write(*current);
                    }
                });
            }
        } catch (const rr::refusal&) {
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), ores::logging::debug) << "The setup run was not read: " << e.what();
        }
    }
    return http_response::json(json::write(*view));
}

http_response browser_session_routes::sign_out(const http_request& req) {
    if (const auto id = identifier_of(req)) {
        if (const auto record = sessions_->find(*id)) {
            // Ending the IAM session is best effort; the gateway's ends either way.
            try {
                auto as_session = req;
                as_session.headers["authorization"] = "Bearer " + record->token;
                (void)ores::http::forwarding::forward(
                    as_session, session_, messaging::logout_request{});
            } catch (const rr::refusal&) {
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), ores::logging::debug)
                    << "The IAM session was not ended: " << e.what();
            }
        }
        sessions_->close(*id);
    }
    auto response = http_response::json(R"({"ok":true})");
    response.set_header("Set-Cookie", session_cookie::clear(cookie_));
    return response;
}

http_response browser_session_routes::choose_party(const http_request& req) {
    const auto id = identifier_of(req);
    const auto record = id ? sessions_->find(*id) : std::nullopt;
    auto view = record ? view_of(*record) : std::nullopt;
    if (!view)
        return not_signed_in();
    const auto body = json::read_expected<party_choice_body>(req.body);
    if (!body || body->partyId.empty())
        return rr::refused(http_status::bad_request, "invalid-request", "A partyId is required.");
    return ores::http::forwarding::guarded<messaging::select_party_request>([&] {
        const auto reply = ores::http::forwarding::forward(
            req, session_, messaging::select_party_request{body->partyId});
        if (!reply.success || reply.token.empty())
            return session_ended();
        const auto party =
            std::ranges::find(view->availableParties, body->partyId, &browser_party::id);
        auto claims = claims_of(reply.token);
        if (party == view->availableParties.end() || !claims)
            return not_signed_in();
        view->party = *party;
        if (!reply.tenant_name.empty())
            view->tenantName = reply.tenant_name;
        if (!reply.username.empty())
            view->username = reply.username;
        view->accessLifetimeSeconds = reply.access_lifetime_s;
        const auto updated = json::write(*view);
        // The token, its claims and the view are written together, so two moves at
        // once leave the last one whole rather than a token for one party and a
        // view for another.
        const auto kept = sessions_->update(*id, [&](session_record& r) {
            r.token = reply.token;
            r.claims = *claims;
            r.view = updated;
        });
        return kept ? http_response::json(updated) : not_signed_in();
    });
}

http_response browser_session_routes::switch_party(const http_request& req) {
    const auto id = identifier_of(req);
    const auto record = id ? sessions_->find(*id) : std::nullopt;
    auto view = record ? view_of(*record) : std::nullopt;
    if (!view)
        return not_signed_in();
    const auto body = json::read_expected<party_choice_body>(req.body);
    if (!body || body->partyId.empty())
        return rr::refused(http_status::bad_request, "invalid-request", "A partyId is required.");
    return ores::http::forwarding::guarded<messaging::switch_party_request>([&] {
        /*
         * The session's list of parties is the login's answer, so a party added
         * since then is not in it: the tenant's own parties say whether it exists.
         */
        const auto parties = tenant_parties::every(req, session_);
        const auto wanted = std::ranges::find_if(
            parties, [&](const auto& p) { return boost::uuids::to_string(p.id) == body->partyId; });
        if (wanted == parties.end())
            return rr::refused(
                http_status::bad_request, "invalid-request", "The tenant holds no such party.");
        const browser_party summary{boost::uuids::to_string(wanted->id),
                                    wanted->full_name,
                                    wanted->party_category,
                                    wanted->business_center_code};
        const auto reply = ores::http::forwarding::forward(
            req, session_, messaging::switch_party_request{summary.id});
        if (!reply.success || reply.token.empty())
            return session_ended();
        auto claims = claims_of(reply.token);
        if (!claims)
            return not_signed_in();
        std::erase_if(view->availableParties,
                      [&](const browser_party& p) { return p.id == summary.id; });
        view->availableParties.push_back(summary);
        view->party = summary;
        view->accessLifetimeSeconds = reply.access_lifetime_s;
        const auto updated = json::write(*view);
        // The token, its claims and the view are written together, so two moves at
        // once leave the last one whole rather than a token for one party and a
        // view for another.
        const auto kept = sessions_->update(*id, [&](session_record& r) {
            r.token = reply.token;
            r.claims = *claims;
            r.view = updated;
        });
        return kept ? http_response::json(updated) : not_signed_in();
    });
}

http_response browser_session_routes::change_password(const http_request& req) {
    const auto body = json::read_expected<password_change_body>(req.body);
    if (!body || body->currentPassword.empty() || body->newPassword.empty())
        return rr::refused(http_status::bad_request,
                           "invalid-request",
                           "The current password and the new password are required.");
    return ores::http::forwarding::guarded<messaging::change_password_request_typed>([&] {
        const auto reply = ores::http::forwarding::forward(
            req,
            session_,
            messaging::change_password_request_typed{body->currentPassword, body->newPassword});
        if (!reply.success)
            return rr::refused(http_status::conflict, "invalid-request", reply.message);
        /*
         * The session's copy of the flag is the login's snapshot, so it is cleared
         * here rather than read again: the account has just done what it asked for.
         */
        if (const auto id = identifier_of(req)) {
            sessions_->update(*id, [](session_record& r) {
                if (auto view = view_of(r)) {
                    view->passwordResetRequired = false;
                    r.view = json::write(*view);
                }
            });
        }
        return http_response::json(R"({"success":true})");
    });
}

void browser_session_routes::register_routes(
    std::shared_ptr<http::net::router> router,
    std::shared_ptr<http::openapi::endpoint_registry> registry) {
    const auto sign_in_route =
        router->post("/api/session")
            .summary("Sign in and hold the session in a cookie")
            .tags({"auth"})
            .auth_optional()
            .rate_limit(ores::http::net::rate_policies::credential)
            .handler([this](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return sign_in(req);
            })
            .build();
    router->add_route(sign_in_route);
    registry->register_route(sign_in_route);

    const auto read_route =
        router->get("/api/session")
            .summary("Read the browser's session")
            .tags({"auth"})
            .auth_optional()
            .handler([this](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return read(req);
            })
            .build();
    router->add_route(read_route);
    registry->register_route(read_route);

    const auto sign_out_route =
        router->delete_("/api/session")
            .summary("Sign out and clear the cookie")
            .tags({"auth"})
            .auth_optional()
            .handler([this](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return sign_out(req);
            })
            .build();
    router->add_route(sign_out_route);
    registry->register_route(sign_out_route);

    const auto party_route =
        router->post("/api/session/party")
            .summary("Choose the party a login waits on")
            .tags({"auth"})
            .auth_required()
            .handler([this](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return choose_party(req);
            })
            .build();
    router->add_route(party_route);
    registry->register_route(party_route);

    const auto switch_route =
        router->post("/api/session/switch-party")
            .summary("Move the session to another of the account's parties")
            .tags({"auth"})
            .auth_required()
            .handler([this](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return switch_party(req);
            })
            .build();
    router->add_route(switch_route);
    registry->register_route(switch_route);

    const auto password_route =
        router->post("/api/account/password")
            .summary("Change the signed-in account's own password")
            .tags({"auth"})
            .auth_required()
            .handler([this](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return change_password(req);
            })
            .build();
    router->add_route(password_route);
    registry->register_route(password_route);
}

}
