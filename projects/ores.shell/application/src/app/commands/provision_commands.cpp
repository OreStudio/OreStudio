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
#include "ores.iam.api/messaging/tenant_provisioning_protocol.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/commands/accounts_commands.hpp"
#include "ores.shell/app/commands/workflow/workflow_run_commands.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <chrono>
#include <cli/cli.h>
#include <functional>
#include <optional>
#include <ostream>
#include <string>
#include <string_view>
#include <vector>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

/**
 * @brief The starting point a tenant is created from when the caller names none.
 *
 * The operational row, which creates the tenant's real data and no test data.
 * Both commands that create a tenant state their starting point as a flag, so a
 * caller that wants the demonstration says so.
 */
constexpr std::string_view default_profile_code = "empty_operational";

/**
 * @brief How long a command spends on the work it starts, all runs included.
 *
 * The budget covers the command, not each run it follows: a tenant is created
 * by one run handing its work to another, and a caller who asked for an hour
 * waits one hour at most. Every starting point publishes bundles whose nested
 * runs take minutes, and the longest of them publishes a dozen in one run, so
 * the wait is generous rather than tight: a command that gave up on work that
 * was still running would report a failure that never happened. A caller on a
 * slow machine raises it, and one that would rather not block lowers it and
 * follows the run with the workflow commands by the id the command prints.
 */
constexpr std::chrono::seconds default_run_timeout(3600);

/**
 * @brief How long the provision request itself may take to answer.
 *
 * The request answers with a run's id rather than the run's result, so it is
 * quick -- but not instant: creating a tenant copies the deployment's
 * registered data into it, its roles, its permissions and its lookup tables
 * included, before any step is dispatched. That work outlives the transport's
 * default request timeout, so the command states the budget the bootstrap verb
 * this request replaces used.
 */
constexpr std::chrono::seconds provision_request_timeout(120);
// Four times the shared 30s default, deliberately: this request starts a run
// whose first phase seeds tenants, accounts and reference data, so the
// responder's own work is minutes rather than milliseconds. A shorter deadline
// would report a healthy provisioning as a timeout.
static_assert(provision_request_timeout > ores::nats::service::default_request_timeout,
              "a provisioning request must outlast the shared default");

/// The flags both commands that create a tenant read. The description default
/// is the caller's, because the command that sets a system up is making a
/// single-tenant deployment and the one that adds a tenant is not.
std::vector<flag_spec> tenant_flag_specs(std::string description_default) {
    return {{.name = "profile",
             .requires_value = true,
             .default_value = std::string(default_profile_code)},
            {.name = "param", .requires_value = true, .default_value = "", .repeatable = true},
            {.name = "tenant-code", .requires_value = true, .default_value = "default"},
            {.name = "tenant-name", .requires_value = true, .default_value = "Default Tenant"},
            {.name = "tenant-hostname", .requires_value = true, .default_value = "localhost"},
            {.name = "tenant-description",
             .requires_value = true,
             .default_value = std::move(description_default)},
            {.name = "tenant-admin", .requires_value = true, .default_value = "tenant_admin"},
            {.name = "tenant-admin-email", .requires_value = true, .default_value = ""},
            {.name = "tenant-admin-password", .requires_value = true, .default_value = ""},
            {.name = "timeout",
             .requires_value = true,
             .default_value = std::to_string(default_run_timeout.count())}};
}

/**
 * @brief Reads the tenant fields a caller supplied.
 *
 * The command checks only what it can know by itself: that the administrator's
 * password was given, because the command offers no default for it, and the
 * derived email address, because the flag's default is computed from the code
 * rather than stored beside it. Every rule about what a code, a hostname, a
 * username or an address may be is the server's, so the shell states none of
 * them: a second copy of a rule here is a copy that drifts.
 *
 * @return the fields, or nothing after reporting what is missing.
 */
std::optional<provision_commands::tenant_fields> read_tenant_fields(std::ostream& out,
                                                                    const parsed_args& parsed) {
    provision_commands::tenant_fields fields;
    fields.code = parsed.flag("tenant-code");
    fields.name = parsed.flag("tenant-name");
    fields.hostname = parsed.flag("tenant-hostname");
    fields.description = parsed.flag("tenant-description");
    fields.admin_username = parsed.flag("tenant-admin");
    fields.admin_email = parsed.flag("tenant-admin-email");
    fields.admin_password = parsed.flag("tenant-admin-password");

    if (fields.admin_password.empty()) {
        fail(out) << "--tenant-admin-password is required: the administrator's first "
                     "password is not a value this command can invent."
                  << std::endl;
        return std::nullopt;
    }
    if (fields.admin_email.empty())
        fields.admin_email = "admin@" + fields.code + ".com";
    return fields;
}

/// The generic provision request a tenant's fields and a starting point make.
iam::messaging::provision_tenant_command
build_tenant_request(const parsed_args& parsed, const provision_commands::tenant_fields& fields) {
    iam::messaging::provision_tenant_command req;
    req.profile_code = parsed.flag("profile");
    req.tenant_code = fields.code;
    req.tenant_name = fields.name;
    req.tenant_hostname = fields.hostname;
    req.tenant_description = fields.description;
    req.admin_username = fields.admin_username;
    req.admin_email = fields.admin_email;
    req.admin_password = fields.admin_password;
    req.parameters = parsed.values("param");
    return req;
}

/**
 * @brief Sends one provision request and blocks until the run it starts ends.
 *
 * The request answers with the run's id rather than the run's result, because
 * the steps it orders take minutes. The run is then followed through the same
 * progress read the browser's journey renders, so both clients watch the same
 * record of the same work.
 *
 * @return the run's instance id after it finished, or nothing after reporting
 * why it did not.
 */
template <typename Request>
std::optional<std::string> run_and_wait(std::ostream& out,
                                        nats_client& session,
                                        const Request& req,
                                        std::chrono::seconds timeout) {
    auto started = do_request(out, session, req, provision_request_timeout, true);
    if (!started)
        return std::nullopt;
    if (!started->success) {
        fail(out) << started->message << std::endl;
        return std::nullopt;
    }

    const auto run = provision_commands::started_run(started->success, started->instance_id);
    if (!run) {
        fail(out) << "The provisioning request named no run to wait on." << std::endl;
        return std::nullopt;
    }

    out << "  Run started: " << *run << std::endl;
    if (!workflow_run_commands::wait_for_instance(out, session, *run, timeout))
        return std::nullopt;
    return *run;
}

/**
 * @brief Blocks until a tenant-owning run, and any run it hands work to, ends.
 *
 * The request starts a run that hands the tenant to its own setup run, which
 * the tenant's own administrator owns. Only that second run finishing makes the
 * tenant ready, so it is followed too.
 *
 * @p timeout is the budget for the whole command, not for each run: a caller
 * that asked for an hour is not asked to wait two. What the first run leaves is
 * what the run it hands work to gets.
 *
 * @return how the work ended, so the caller words its success for what it can
 * actually see: a run this session cannot read leaves the tenant's setup
 * continuing elsewhere, which is not the same as a tenant ready to work in.
 */
template <typename Request>
workflow_run_commands::follow_result run_and_follow(std::ostream& out,
                                                    nats_client& session,
                                                    const Request& req,
                                                    std::chrono::seconds timeout) {
    const auto started_at = std::chrono::steady_clock::now();
    const auto instance_id = run_and_wait(out, session, req, timeout);
    if (!instance_id)
        return workflow_run_commands::follow_result::failed;

    const auto spent = std::chrono::duration_cast<std::chrono::seconds>(
        std::chrono::steady_clock::now() - started_at);
    return workflow_run_commands::follow_child_instance(
        out, session, *instance_id, workflow_run_commands::remaining_budget(timeout, spent));
}

/// The timeout a command was given, or nothing after reporting a value it
/// cannot use.
std::optional<std::chrono::seconds> read_timeout(std::ostream& out, const parsed_args& parsed) {
    auto timeout = parse_positive_seconds(parsed.flag("timeout"));
    if (!timeout) {
        fail(out) << "Timeout must be a positive number of seconds: " << parsed.flag("timeout")
                  << std::endl;
        return std::nullopt;
    }
    return timeout;
}

}

std::optional<std::string> provision_commands::started_run(bool request_succeeded,
                                                           std::string_view instance_id) {
    if (!request_succeeded || instance_id.empty())
        return std::nullopt;
    return std::string(instance_id);
}

void provision_commands::report_tenant_outcome(std::ostream& out,
                                               const tenant_fields& fields,
                                               tenant_creation created,
                                               tenant_setup setup) {
    const bool is_setup = created == tenant_creation::system;
    const bool continues = setup == tenant_setup::continues_elsewhere;

    out << "✓ ";
    if (is_setup)
        out << "System provisioned. ";

    if (continues) {
        out << "Tenant '" << fields.name << "' created; its setup continues." << std::endl;
    } else if (is_setup) {
        out << "Tenant '" << fields.name << "'." << std::endl;
    } else {
        out << "Tenant '" << fields.name << "' provisioned." << std::endl;
    }

    out << "Next: logout, then: login " << fields.admin_username << "@" << fields.hostname
        << " <password>";
    if (continues) {
        out << ", and follow the setup run named above with: workflow wait <its instance id>"
            << std::endl;
        return;
    }
    out << "  — the tenant is ready to work in." << std::endl;
}

void provision_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    // The setup act belongs in the menu the generated bootstrap unit owns, so
    // the shell's bootstrap verbs read together: what the installation answers,
    // how it is given an administrator, and how it is brought to life. The
    // extension is recorded rather than inserted, because that unit registers
    // in its own call and the two halves of one menu are two of repl.cpp's.
    ores::shell::app::extend_menu(root_menu, "bootstrap", [&session](cli::Menu& menu) {
        menu.Insert("setup",
                    [&session](std::ostream& out, std::vector<std::string> args) {
                        process_setup(std::ref(out), std::ref(session), args);
                    },
                    "Set an empty installation up from a starting point: create the "
                    "system administrator, sign in as it, and provision the first "
                    "tenant",
                    {"<username> <password> <email> --tenant-admin-password <pw> "
                     "[--profile <code>] [--param <name=value>] [--tenant-code <c>] "
                     "[--tenant-name <n>] [--tenant-hostname <h>] [--tenant-description <d>] "
                     "[--tenant-admin <user>] [--tenant-admin-email <email>] "
                     "[--timeout <seconds>]"});
    });

    auto provision_menu = std::make_unique<cli::Menu>("provision");

    provision_menu->Insert(
        "tenant",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_tenant(std::ref(out), std::ref(session), args);
        },
        "Provision a tenant from a starting point, for every tenant after the "
        "first",
        {"--tenant-admin-password <pw> [--tenant-code <c>] [--tenant-name <n>] "
         "[--tenant-hostname <h>] [--tenant-description <d>] [--tenant-admin <user>] "
         "[--tenant-admin-email <email>] [--profile <code>] [--param <name=value>] "
         "[--timeout <seconds>]"});

    provision_menu->Insert("party",
                           [&session](std::ostream& out, std::vector<std::string> args) {
                               process_party(std::ref(out), std::ref(session), args);
                           },
                           "Provision a party of the signed-in tenant: publish the "
                           "reference data its starting point orders, activate it, "
                           "and join the administrator who asked to it",
                           {"<party-uuid-or-full-name> [--profile <code>] "
                            "[--timeout <seconds>]"});

    ores::shell::app::insert_menu(root_menu, std::move(provision_menu));
}
void provision_commands::process_setup(std::ostream& out,
                                       nats_client& session,
                                       const std::vector<std::string>& args) {
    auto parsed =
        parse_args(args, tenant_flag_specs("Default tenant for single-tenant deployment"));
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 3) {
        fail(out) << "Usage: bootstrap setup <username> <password> <email> "
                     "--tenant-admin-password <pw> [--profile <code>] "
                     "[--param <name=value>] [--tenant-* flags]"
                  << std::endl;
        return;
    }

    const auto& username = parsed->positionals[0];
    const auto& password = parsed->positionals[1];
    const auto& email = parsed->positionals[2];

    auto fields = read_tenant_fields(out, *parsed);
    if (!fields)
        return;
    auto timeout = read_timeout(out, *parsed);
    if (!timeout)
        return;

    if (session.has_user_login()) {
        fail(out) << "Already logged in; bootstrap setup runs against a fresh, "
                     "bootstrap-mode system. Log out first."
                  << std::endl;
        return;
    }

    // The first run's order, which is the browser's: the installation says
    // whether it still needs an administrator, the administrator is created
    // with no session, and only then is there a session to provision with.
    iam::messaging::bootstrap_status_request status_req;
    auto status = do_request(out, session, status_req);
    if (!status)
        return;
    if (!status->is_in_bootstrap_mode) {
        fail(out) << "System is not in bootstrap mode: " << status->message << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Provisioning system: admin " << username << ", tenant "
                              << fields->code;

    out << "[1/3] Creating initial admin account '" << username << "'..." << std::endl;
    iam::messaging::create_initial_admin_request admin_req;
    admin_req.principal = username;
    admin_req.password = password;
    admin_req.email = email;
    auto admin = do_request(out, session, admin_req);
    if (!admin)
        return;
    if (!admin->success) {
        fail(out) << "Failed to create admin account: " << admin->error_message << std::endl;
        return;
    }
    out << "  Account created (ID: " << admin->account_id << ")." << std::endl;

    out << "[2/3] Logging in as '" << username << "'..." << std::endl;
    accounts_commands::process_login(out, session, username, password);
    if (!session.is_logged_in())
        return;

    out << "[3/3] Provisioning tenant '" << fields->code << "' from '" << parsed->flag("profile")
        << "'..." << std::endl;
    const auto outcome =
        run_and_follow(out, session, build_tenant_request(*parsed, *fields), *timeout);
    if (outcome == workflow_run_commands::follow_result::failed)
        return;

    report_tenant_outcome(out,
                          *fields,
                          tenant_creation::system,
                          outcome == workflow_run_commands::follow_result::continues_elsewhere ?
                              tenant_setup::continues_elsewhere :
                              tenant_setup::finished);
    BOOST_LOG_SEV(lg(), info) << "System provisioned; tenant " << fields->code;
}

void provision_commands::process_tenant(std::ostream& out,
                                        nats_client& session,
                                        const std::vector<std::string>& args) {
    auto parsed = parse_args(args, tenant_flag_specs(""));
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!parsed->positionals.empty()) {
        fail(out) << "provision tenant takes no positional arguments; see help." << std::endl;
        return;
    }

    auto fields = read_tenant_fields(out, *parsed);
    if (!fields)
        return;
    auto timeout = read_timeout(out, *parsed);
    if (!timeout)
        return;

    if (!session.is_logged_in()) {
        fail(out) << "Not logged in. Log in as an administrator; the tenant is created "
                     "under the tenant that administrator works in."
                  << std::endl;
        return;
    }

    out << "Provisioning tenant '" << fields->code << "' from '" << parsed->flag("profile")
        << "'..." << std::endl;
    const auto outcome =
        run_and_follow(out, session, build_tenant_request(*parsed, *fields), *timeout);
    if (outcome == workflow_run_commands::follow_result::failed)
        return;

    report_tenant_outcome(out,
                          *fields,
                          tenant_creation::additional,
                          outcome == workflow_run_commands::follow_result::continues_elsewhere ?
                              tenant_setup::continues_elsewhere :
                              tenant_setup::finished);
    BOOST_LOG_SEV(lg(), info) << "Tenant provisioned; " << fields->code;
}

/**
 * @brief The starting point a party stage runs from when the caller names none.
 *
 * Empty, which asks the service for the deployment's own party starting point:
 * the first profile that orders a party step. Every profile orders the same
 * party stage, and a tenant administrator cannot read the profile rows at all,
 * so the service that holds them is the one that chooses.
 */
constexpr std::string_view default_party_profile_code = "";

void provision_commands::process_party(std::ostream& out,
                                       nats_client& session,
                                       const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "profile",
                               .requires_value = true,
                               .default_value = std::string(default_party_profile_code)},
                              {.name = "timeout",
                               .requires_value = true,
                               .default_value = std::to_string(default_run_timeout.count())}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: provision party <party-uuid-or-full-name> "
                     "[--profile <code>] [--timeout <seconds>]"
                  << std::endl;
        return;
    }
    auto timeout = read_timeout(out, *parsed);
    if (!timeout)
        return;

    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& party = parsed->positionals.front();
    const auto& profile = parsed->flag("profile");
    out << "Provisioning party '" << party << "' from '" << profile << "'..." << std::endl;

    iam::messaging::provision_party_command req;
    req.party = party;
    req.profile_code = profile;
    // A party run is the whole job: it hands nothing on, so there is no run to
    // follow and no result to read.
    if (!run_and_wait(out, session, req, *timeout))
        return;

    out << "✓ Party '" << party << "' provisioned." << std::endl;
    out << "Next: accounts set-default-party \"" << party << "\" to work in it." << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Party provisioned: " << party;
}

}
