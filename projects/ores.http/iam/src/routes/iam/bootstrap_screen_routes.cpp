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
#include "ores.http/routes/iam/bootstrap_screen_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/iam/tenancy_screen_shapes.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.variability.api/messaging/operations_protocol.hpp"
#include "ores.variability.api/messaging/system_setting_protocol.hpp"
#include <boost/asio/awaitable.hpp>
#include <exception>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <utility>

namespace ores::http::routes::iam {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace json = ores::utility::codec::json;
namespace messaging = ores::iam::messaging;
namespace variability = ores::variability::messaging;
namespace workflow = ores::workflow::messaging;
namespace rr = ores::http::net::record_routes;

namespace {

/// The run type ores.iam names on a tenant's own setup run.
constexpr std::string_view tenant_setup_workflow_type = "tenant_setup_workflow";

/// The setting each wizard records when it finishes.
constexpr std::string_view onboarding_system_setting = "onboarding.system";
constexpr std::string_view onboarding_tenant_setting = "onboarding.tenant";

/// Whether a wizard recorded that it finished, read through the browser's
/// session. A browser with no session, or a read the server refuses, answers
/// false: a deployment that cannot prove the wizard finished stays where it is.
bool wizard_finished(const http_request& req, nats_client& session, std::string_view setting) {
    if (!req.authenticated_user)
        return false;
    try {
        const auto reply = ores::http::forwarding::forward(
            req,
            session,
            variability::get_system_setting_request{
                variability::system_setting_key{std::string(setting)}});
        return reply.system_setting && reply.system_setting->value == "true";
    } catch (const rr::refusal&) {
        return false;
    } catch (const std::exception&) {
        return false;
    }
}

}

bootstrap_screen_routes::bootstrap_screen_routes(nats_client& session,
                                                 ores::http::domain::cookie_settings cookie)
    : session_(session)
    , cookie_(std::move(cookie)) {}

std::string bootstrap_screen_routes::hostname_of(const std::string& host) {
    if (!host.empty() && host.front() == '[')
        return host.substr(0, host.find(']') + 1);
    return host.substr(0, host.find(':'));
}

std::string bootstrap_screen_routes::status_json(const messaging::bootstrap_status_response& status,
                                                 bool onboarding_complete,
                                                 bool onboarding_tenant_complete,
                                                 const std::string& account_id,
                                                 bool session_present) {
    return "{\"isInBootstrapMode\":" + json::write(status.is_in_bootstrap_mode) +
           ",\"hasTenant\":" + json::write(status.has_tenant) +
           ",\"onboardingComplete\":" + json::write(onboarding_complete) +
           ",\"onboardingTenantComplete\":" + json::write(onboarding_tenant_complete) +
           ",\"accountId\":" + json::write(account_id) +
           ",\"sessionPresent\":" + json::write(session_present) +
           ",\"message\":" + json::write(status.message) +
           ",\"version\":" + json::write(status.version) + "}";
}

std::string bootstrap_screen_routes::policy_json(const messaging::registration_policy_response& p) {
    return "{\"success\":" + json::write(p.success) + ",\"message\":" + json::write(p.message) +
           ",\"errorCode\":" + json::write(p.error_code) +
           ",\"signupsEnabled\":" + json::write(p.signups_enabled) +
           ",\"authorizationRequired\":" + json::write(p.authorization_required) +
           ",\"tenantId\":" + json::write(p.tenant_id) +
           ",\"tenantName\":" + json::write(p.tenant_name) +
           ",\"partyId\":" + json::write(p.party_id) +
           ",\"partyName\":" + json::write(p.party_name) + ",\"roleId\":" + json::write(p.role_id) +
           ",\"roleName\":" + json::write(p.role_name) +
           ",\"usableNow\":" + json::write(p.usable_now) + "}";
}

std::string bootstrap_screen_routes::setup_run_json(
    const std::optional<workflow::workflow_instance_summary>& run) {
    if (!run)
        return R"({"instanceId":"","status":"","error":""})";
    return "{\"instanceId\":" + json::write(run->id) + ",\"status\":" + json::write(run->status) +
           ",\"error\":" + json::write(run->error) + "}";
}

void bootstrap_screen_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry) {
    using ores::http::forwarding::guarded;
    auto& session = session_;
    const auto cookie = cookie_;

    const auto status =
        router->get("/api/bootstrap")
            .summary("Read whether the deployment is set up, as a visitor or a signed-in browser")
            .tags({"setup"})
            .auth_optional()
            .handler([&session,
                      cookie](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::bootstrap_status_request>([&] {
                    const auto reply = ores::http::forwarding::forward(
                        req, session, messaging::bootstrap_status_request{});
                    const auto& claims = req.authenticated_user;
                    return http_response::json(status_json(
                        reply,
                        wizard_finished(req, session, onboarding_system_setting),
                        wizard_finished(req, session, onboarding_tenant_setting),
                        claims ? claims->subject : std::string(),
                        ores::http::domain::session_cookie::read(req.get_header("cookie"), cookie)
                            .has_value()));
                });
            })
            .build();
    router->add_route(status);
    registry->register_route(status);

    const auto administrator =
        router->post("/api/bootstrap/administrator")
            .summary("Create the first administrator, which ends bootstrap mode")
            .tags({"setup"})
            .auth_optional()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                const auto body = json::read_expected<initial_administrator_body>(req.body);
                if (!body || body->principal.empty() || body->password.empty() ||
                    body->email.empty())
                    co_return rr::refused(
                        http_status::bad_request,
                        "invalid-request",
                        "A username, an email address and a password are required.");
                co_return guarded<messaging::create_initial_admin_request>([&] {
                    // The create grants SuperAdmin and does not check the flag itself.
                    const auto state = ores::http::forwarding::forward(
                        req, session, messaging::bootstrap_status_request{});
                    if (!state.is_in_bootstrap_mode)
                        return rr::refused(
                            http_status::conflict,
                            "bootstrap-complete",
                            "This deployment already has an administrator, so it is not in "
                            "bootstrap mode. Sign in instead.");
                    const auto created = ores::http::forwarding::forward(
                        req,
                        session,
                        messaging::create_initial_admin_request{
                            body->principal, body->password, body->email});
                    if (!created.success)
                        return rr::refused(http_status::bad_request,
                                           "invalid-request",
                                           created.error_message.empty() ?
                                               "The administrator could not be created." :
                                               created.error_message);
                    return http_response::json("{\"accountId\":" + json::write(created.account_id) +
                                               ",\"tenantId\":" + json::write(created.tenant_id) +
                                               "}");
                });
            })
            .build();
    router->add_route(administrator);
    registry->register_route(administrator);

    const auto complete =
        router->post("/api/bootstrap/complete")
            .summary("Record that the first-run journey finished")
            .tags({"setup"})
            .auth_required()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<variability::complete_system_onboarding_request>([&] {
                    const auto reply = ores::http::forwarding::forward(
                        req, session, variability::complete_system_onboarding_request{});
                    rr::require_done(reply.result);
                    return http_response::json(R"({"success":true})");
                });
            })
            .build();
    router->add_route(complete);
    registry->register_route(complete);

    const auto setup_run =
        router->get("/api/tenant-setup")
            .summary("Read the caller's tenant's own setup run")
            .tags({"setup"})
            .auth_required()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<workflow::list_workflow_instance_summaries_request>([&] {
                    workflow::list_workflow_instance_summaries_request msg;
                    msg.limit = 1;
                    msg.type_filter = std::string(tenant_setup_workflow_type);
                    const auto reply = ores::http::forwarding::forward(req, session, msg);
                    if (!reply.success)
                        throw std::runtime_error(
                            reply.message.empty() ? "The setup run was not read." : reply.message);
                    return http_response::json(
                        setup_run_json(reply.instances.empty() ?
                                           std::nullopt :
                                           std::optional<workflow::workflow_instance_summary>(
                                               reply.instances.front())));
                });
            })
            .build();
    router->add_route(setup_run);
    registry->register_route(setup_run);

    const auto policy =
        router->get("/api/registration-policy")
            .summary("Read what the deployment offers somebody not in it yet")
            .tags({"setup"})
            .auth_optional()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::registration_policy_request>([&] {
                    return http_response::json(policy_json(
                        ores::http::forwarding::forward(req,
                                                        session,
                                                        messaging::registration_policy_request{
                                                            hostname_of(req.get_header("host"))})));
                });
            })
            .build();
    router->add_route(policy);
    registry->register_route(policy);
}

}
