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
#include "ores.shell/app/commands/provision_commands.hpp"
#include "ores.iam.api/messaging/bootstrap_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/accounts_commands.hpp"
#include <chrono>
#include <cli/cli.h>
#include <functional>
#include <ostream>
#include <regex>
#include <rfl/json.hpp>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

// The wizard's "slow" timeout for the provision-tenant request.
constexpr std::chrono::seconds provision_timeout(120);

// Validation rules lifted from the SystemProvisionerWizard pages.
const std::regex username_regex("^[a-zA-Z][a-zA-Z0-9_]{2,49}$");
const std::regex email_regex("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9._-]+\\.[a-zA-Z]{2,}$");
const std::regex tenant_code_regex("^[a-z][a-z0-9_]{0,49}$");
constexpr std::size_t min_password_length = 8;

template <typename Request>
std::optional<typename Request::response_type>
do_request(std::ostream& out, nats_client& session, const Request& req,
           std::chrono::milliseconds timeout = std::chrono::seconds(30),
           bool authenticated = false) {
    using Response = typename Request::response_type;
    try {
        const auto body = rfl::json::write(req);
        // The unauthenticated request overload has no timeout knob.
        auto reply = authenticated
            ? session.authenticated_request(std::string(req.nats_subject), body, timeout)
            : session.request(std::string(req.nats_subject), body);
        auto result = rfl::json::read<Response>(ores::nats::as_string_view(reply.data));
        if (!result) {
            fail(out) << "Failed to parse response: " << result.error().what()
                      << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        fail(out) << "Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

bool validate_account(std::ostream& out, std::string_view what,
                      const std::string& username, const std::string& email,
                      const std::string& password) {
    if (!std::regex_match(username, username_regex)) {
        fail(out) << what << " username must be 3-50 characters, starting with a "
                     "letter (letters, digits, underscore): " << username << std::endl;
        return false;
    }
    if (!std::regex_match(email, email_regex)) {
        fail(out) << what << " email is not a valid address: " << email << std::endl;
        return false;
    }
    if (password.size() < min_password_length) {
        fail(out) << what << " password must be at least " << min_password_length
                  << " characters." << std::endl;
        return false;
    }
    return true;
}

}

void provision_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto provision_menu = std::make_unique<cli::Menu>("provision");

    provision_menu->Insert(
        "system",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_system(std::ref(out), std::ref(session), args);
        },
        "Bootstrap the system: create the initial admin, log in as it and "
        "provision the first tenant (wizard single-tenant defaults)",
        {"username password email --tenant-admin-password <pw> [--tenant-code <c>] "
         "[--tenant-name <n>] [--tenant-type <t>] [--tenant-hostname <h>] "
         "[--tenant-description <d>] [--tenant-admin <user>] [--tenant-admin-email <email>]"});

    root_menu.Insert(std::move(provision_menu));
}

void provision_commands::process_system(std::ostream& out,
                                        nats_client& session,
                                        const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {
        {.name = "tenant-admin-password", .requires_value = true, .default_value = ""},
        {.name = "tenant-code", .requires_value = true, .default_value = "default"},
        {.name = "tenant-name", .requires_value = true, .default_value = "Default Tenant"},
        {.name = "tenant-type", .requires_value = true, .default_value = "evaluation"},
        {.name = "tenant-hostname", .requires_value = true, .default_value = "localhost"},
        {.name = "tenant-description", .requires_value = true,
         .default_value = "Default tenant for single-tenant deployment"},
        {.name = "tenant-admin", .requires_value = true, .default_value = "tenant_admin"},
        {.name = "tenant-admin-email", .requires_value = true, .default_value = ""}
    });
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 3) {
        fail(out) << "Usage: provision system <username> <password> <email> "
                     "--tenant-admin-password <pw> [--tenant-* flags]" << std::endl;
        return;
    }

    const auto& username = parsed->positionals[0];
    const auto& password = parsed->positionals[1];
    const auto& email = parsed->positionals[2];
    const auto& tenant_code = parsed->flag("tenant-code");
    const auto& tenant_admin = parsed->flag("tenant-admin");
    const auto& tenant_admin_password = parsed->flag("tenant-admin-password");
    // The wizard pre-fills the tenant admin email from the tenant code.
    const auto tenant_admin_email = parsed->flag("tenant-admin-email").empty()
        ? "admin@" + tenant_code + ".com"
        : parsed->flag("tenant-admin-email");

    // Validate everything before touching the backend, as the wizard
    // pages do.
    if (tenant_admin_password.empty()) {
        fail(out) << "--tenant-admin-password is required (no wizard default exists)."
                  << std::endl;
        return;
    }
    if (!validate_account(out, "Admin", username, email, password))
        return;
    if (!validate_account(out, "Tenant admin", tenant_admin, tenant_admin_email,
                          tenant_admin_password))
        return;
    if (!std::regex_match(tenant_code, tenant_code_regex)) {
        fail(out) << "Tenant code must start with a lowercase letter (lowercase, "
                     "digits, underscore, max 50): " << tenant_code << std::endl;
        return;
    }
    if (parsed->flag("tenant-name").empty() || parsed->flag("tenant-hostname").empty()) {
        fail(out) << "Tenant name and hostname must not be empty." << std::endl;
        return;
    }
    if (session.is_logged_in()) {
        fail(out) << "Already logged in; provision system runs against a fresh, "
                     "bootstrap-mode system. Log out first." << std::endl;
        return;
    }

    // Phase 0: confirm the system is in bootstrap mode, as the GUI
    // does before ever showing the wizard.
    iam::messaging::bootstrap_status_request status_req;
    auto status = do_request(out, session, status_req);
    if (!status)
        return;
    if (!status->is_in_bootstrap_mode) {
        fail(out) << "System is not in bootstrap mode: " << status->message << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Provisioning system: admin " << username << ", tenant "
                              << tenant_code;

    // Phase 1: create the initial admin account.
    out << "[1/3] Creating initial admin account '" << username << "'..." << std::endl;
    iam::messaging::create_initial_admin_request admin_req;
    admin_req.principal = username;
    admin_req.password = password;
    admin_req.email = email;
    auto admin = do_request(out, session, admin_req);
    if (!admin)
        return;
    if (!admin->success) {
        fail(out) << "Failed to create admin account: " << admin->error_message
                  << std::endl;
        return;
    }
    out << "  Account created (ID: " << admin->account_id << ")." << std::endl;

    // Phase 2: log in as the new admin, as the wizard does, so the
    // provision-tenant request is authenticated.
    out << "[2/3] Logging in as '" << username << "'..." << std::endl;
    accounts_commands::process_login(out, session, username, password);
    if (!session.is_logged_in())
        return;

    // Phase 3: provision the first tenant and its admin account.
    out << "[3/3] Provisioning tenant '" << tenant_code << "'..." << std::endl;
    iam::messaging::provision_tenant_request tenant_req;
    tenant_req.type = parsed->flag("tenant-type");
    tenant_req.code = tenant_code;
    tenant_req.name = parsed->flag("tenant-name");
    tenant_req.hostname = parsed->flag("tenant-hostname");
    tenant_req.description = parsed->flag("tenant-description");
    tenant_req.principal = tenant_admin;
    tenant_req.password = tenant_admin_password;
    tenant_req.email = tenant_admin_email;
    auto tenant = do_request(out, session, tenant_req, provision_timeout,
                             true /*authenticated*/);
    if (!tenant)
        return;
    if (!tenant->success) {
        fail(out) << "Failed to provision tenant: " << tenant->error_message << std::endl;
        return;
    }

    out << "✓ System provisioned. Tenant '" << tenant_req.name << "' (ID: "
        << tenant->tenant_id << "), admin '" << tenant_admin << "'." << std::endl;
    out << "Next: logout, then: login " << tenant_admin << "@"
        << tenant_req.hostname << " <password>  — the tenant is in bootstrap mode; "
        << "run provision tenant." << std::endl;
    BOOST_LOG_SEV(lg(), info) << "System provisioned; tenant " << tenant->tenant_id;
}

}
