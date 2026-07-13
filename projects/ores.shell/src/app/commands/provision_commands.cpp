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
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.dq.api/messaging/dataset_bundle_protocol.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.dq.api/messaging/report_definition_template_protocol.hpp"
#include "ores.iam.api/domain/account_party.hpp"
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.iam.api/messaging/bootstrap_protocol.hpp"
#include "ores.iam.api/messaging/tenant_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.reporting.api/messaging/report_definition_protocol.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/accounts_commands.hpp"
#include "ores.shell/app/commands/synthetic_commands.hpp"
#include "ores.shell/app/commands/workflow_commands.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.variability.api/messaging/system_settings_protocol.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
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

// The wizards' generous timeouts for publish dispatch and waits.
constexpr std::chrono::minutes publish_timeout(5);
constexpr std::chrono::seconds default_wait_timeout(300);

// The wizard's "slow" timeout for the provision-tenant request.
constexpr std::chrono::seconds provision_timeout(120);

// Validation rules lifted from the SystemProvisionerWizard pages.
const std::regex username_regex("^[a-zA-Z][a-zA-Z0-9_]{2,49}$");
const std::regex email_regex("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9._-]+\\.[a-zA-Z]{2,}$");
const std::regex tenant_code_regex("^[a-z][a-z0-9_]{0,49}$");
constexpr std::size_t min_password_length = 8;

template <typename Request>
std::optional<typename Request::response_type>
do_request(std::ostream& out,
           nats_client& session,
           const Request& req,
           std::chrono::milliseconds timeout = std::chrono::seconds(30),
           bool authenticated = false) {
    using Response = typename Request::response_type;
    try {
        const auto body = rfl::json::write(req);
        // The unauthenticated request overload has no timeout knob.
        auto reply =
            authenticated ?
                session.authenticated_request(std::string(req.nats_subject), body, timeout) :
                session.request(std::string(req.nats_subject), body);
        auto result = rfl::json::read<Response>(ores::nats::as_string_view(reply.data));
        if (!result) {
            fail(out) << "Failed to parse response: " << result.error().what() << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        fail(out) << "Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

bool validate_account(std::ostream& out,
                      std::string_view what,
                      const std::string& username,
                      const std::string& email,
                      const std::string& password) {
    if (!std::regex_match(username, username_regex)) {
        fail(out) << what
                  << " username must be 3-50 characters, starting with a "
                     "letter (letters, digits, underscore): "
                  << username << std::endl;
        return false;
    }
    if (!std::regex_match(email, email_regex)) {
        fail(out) << what << " email is not a valid address: " << email << std::endl;
        return false;
    }
    if (password.size() < min_password_length) {
        fail(out) << what << " password must be at least " << min_password_length << " characters."
                  << std::endl;
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

    provision_menu->Insert(
        "tenant",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_tenant(std::ref(out), std::ref(session), args);
        },
        "Provision the logged-in bootstrap-mode tenant: publish a bundle, "
        "optionally generate synthetic data, associate the admin with the "
        "parties and finalize",
        {"[--bundle <code>] [--source gleif|synthetic] [--root-lei <lei>] "
         "[--timeout <seconds>] [synthetic generation knobs — see synthetic generate]"});

    provision_menu->Insert("party",
                           [&session](std::ostream& out, std::vector<std::string> args) {
                               process_party(std::ref(out), std::ref(session), args);
                           },
                           "Provision a party: import counterparties, publish its organisation "
                           "bundle, create report definitions and activate it",
                           {"party-uuid-or-full-name [--dataset-size small|large] "
                            "[--reports all|none|<name,...>] [--timeout <seconds>]"});

    root_menu.Insert(std::move(provision_menu));
}

void provision_commands::process_system(std::ostream& out,
                                        nats_client& session,
                                        const std::vector<std::string>& args) {
    auto parsed = parse_args(
        args,
        {{.name = "tenant-admin-password", .requires_value = true, .default_value = ""},
         {.name = "tenant-code", .requires_value = true, .default_value = "default"},
         {.name = "tenant-name", .requires_value = true, .default_value = "Default Tenant"},
         {.name = "tenant-type", .requires_value = true, .default_value = "evaluation"},
         {.name = "tenant-hostname", .requires_value = true, .default_value = "localhost"},
         {.name = "tenant-description",
          .requires_value = true,
          .default_value = "Default tenant for single-tenant deployment"},
         {.name = "tenant-admin", .requires_value = true, .default_value = "tenant_admin"},
         {.name = "tenant-admin-email", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 3) {
        fail(out) << "Usage: provision system <username> <password> <email> "
                     "--tenant-admin-password <pw> [--tenant-* flags]"
                  << std::endl;
        return;
    }

    const auto& username = parsed->positionals[0];
    const auto& password = parsed->positionals[1];
    const auto& email = parsed->positionals[2];
    const auto& tenant_code = parsed->flag("tenant-code");
    const auto& tenant_admin = parsed->flag("tenant-admin");
    const auto& tenant_admin_password = parsed->flag("tenant-admin-password");
    // The wizard pre-fills the tenant admin email from the tenant code.
    const auto tenant_admin_email = parsed->flag("tenant-admin-email").empty() ?
                                        "admin@" + tenant_code + ".com" :
                                        parsed->flag("tenant-admin-email");

    // Validate everything before touching the backend, as the wizard
    // pages do.
    if (tenant_admin_password.empty()) {
        fail(out) << "--tenant-admin-password is required (no wizard default exists)." << std::endl;
        return;
    }
    // Tenant code first: the tenant admin email derives from it, so a
    // bad code must not surface as a confusing derived-email error.
    if (!std::regex_match(tenant_code, tenant_code_regex)) {
        fail(out) << "Tenant code must start with a lowercase letter (lowercase, "
                     "digits, underscore, max 50): "
                  << tenant_code << std::endl;
        return;
    }
    if (parsed->flag("tenant-name").empty() || parsed->flag("tenant-hostname").empty()) {
        fail(out) << "Tenant name and hostname must not be empty." << std::endl;
        return;
    }
    if (!validate_account(out, "Admin", username, email, password))
        return;
    if (!validate_account(
            out, "Tenant admin", tenant_admin, tenant_admin_email, tenant_admin_password))
        return;
    if (session.is_logged_in()) {
        fail(out) << "Already logged in; provision system runs against a fresh, "
                     "bootstrap-mode system. Log out first."
                  << std::endl;
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
        fail(out) << "Failed to create admin account: " << admin->error_message << std::endl;
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
    auto tenant = do_request(out, session, tenant_req, provision_timeout, true /*authenticated*/);
    if (!tenant)
        return;
    if (!tenant->success) {
        fail(out) << "Failed to provision tenant: " << tenant->error_message << std::endl;
        return;
    }

    out << "✓ System provisioned. Tenant '" << tenant_req.name << "' (ID: " << tenant->tenant_id
        << "), admin '" << tenant_admin << "'." << std::endl;
    out << "Next: logout, then: login " << tenant_admin << "@" << tenant_req.hostname
        << " <password>  — the tenant is in bootstrap mode; " << "run provision tenant."
        << std::endl;
    BOOST_LOG_SEV(lg(), info) << "System provisioned; tenant " << tenant->tenant_id;
}

void provision_commands::process_tenant(std::ostream& out,
                                        nats_client& session,
                                        const std::vector<std::string>& args) {
    auto specs = synthetic_commands::generate_flag_specs();
    specs.push_back({.name = "bundle", .requires_value = true, .default_value = ""});
    specs.push_back({.name = "source", .requires_value = true, .default_value = "gleif"});
    specs.push_back({.name = "root-lei", .requires_value = true, .default_value = ""});
    specs.push_back({.name = "timeout",
                     .requires_value = true,
                     .default_value = std::to_string(default_wait_timeout.count())});

    auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!parsed->positionals.empty()) {
        fail(out) << "provision tenant takes no positional arguments; see help." << std::endl;
        return;
    }

    const auto& source = parsed->flag("source");
    if (source != "gleif" && source != "synthetic") {
        fail(out) << "--source must be gleif or synthetic: " << source << std::endl;
        return;
    }
    if (source != "gleif" && !parsed->flag("root-lei").empty()) {
        fail(out) << "--root-lei only applies to --source gleif." << std::endl;
        return;
    }
    auto wait_timeout = parse_positive_seconds(parsed->flag("timeout"));
    if (!wait_timeout) {
        fail(out) << "Timeout must be a positive number of seconds: " << parsed->flag("timeout")
                  << std::endl;
        return;
    }
    // Build (and validate) the generation request up front even though
    // it only runs in synthetic mode: fail fast on bad knobs.
    auto generate_req = synthetic_commands::build_generate_request(out, *parsed);
    if (!generate_req)
        return;
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in. Log in as the tenant admin of the "
                     "bootstrap-mode tenant first."
                  << std::endl;
        return;
    }
    const auto username = session.auth().username;

    // Resolve the bundle: explicit --bundle, else the first available,
    // which is the wizard's default selection.
    auto bundle_code = parsed->flag("bundle");
    if (bundle_code.empty()) {
        dq::messaging::get_dataset_bundles_request bundles_req;
        auto bundles = do_request(out, session, bundles_req, std::chrono::seconds(30), true);
        if (!bundles)
            return;
        if (bundles->bundles.empty()) {
            fail(out) << "No dataset bundles are available to publish." << std::endl;
            return;
        }
        bundle_code = bundles->bundles.front().code;
        out << "Using bundle '" << bundle_code << "' (first available)." << std::endl;
    }

    BOOST_LOG_SEV(lg(), info) << "Provisioning tenant: bundle " << bundle_code << ", source "
                              << source;

    // Phase 1: publish the bundle and wait on its workflow.
    out << "[1/4] Publishing bundle '" << bundle_code << "'..." << std::endl;
    dq::messaging::publish_bundle_request publish_req;
    publish_req.bundle_code = bundle_code;
    publish_req.mode = dq::domain::publication_mode::upsert;
    publish_req.published_by = username;
    publish_req.atomic = true;
    if (!parsed->flag("root-lei").empty()) {
        dq::messaging::publish_bundle_params params;
        params.lei_parties =
            dq::messaging::lei_parties_params{.root_lei = parsed->flag("root-lei")};
        publish_req.params_json = dq::messaging::build_params_json(params);
    }
    auto published = do_request(out, session, publish_req, publish_timeout, true);
    if (!published)
        return;
    if (!published->success) {
        fail(out) << "Failed to publish bundle: " << published->error_message << std::endl;
        return;
    }
    out << "  Dispatched " << published->datasets_dispatched
        << " dataset(s); workflow instance: " << published->instance_id << std::endl;
    if (!workflow_commands::wait_for_instance(
            out,
            session,
            published->instance_id,
            *wait_timeout,
            static_cast<std::size_t>(published->datasets_dispatched)))
        return;

    // Phase 2: synthetic generation, when selected.
    if (source == "synthetic") {
        out << "[2/4] Generating synthetic organisation..." << std::endl;
        if (!synthetic_commands::generate(out, session, *generate_req))
            return;
    } else {
        out << "[2/4] GLEIF source: no generation step." << std::endl;
    }

    // Phase 3: associate the admin with every Operational party.
    // Non-fatal, exactly as the wizard treats it.
    out << "[3/4] Associating '" << username << "' with the operational parties..." << std::endl;
    int linked = 0;
    if (session.auth().account_id.empty()) {
        out << "⚠ No account id in the session; skipping party association. "
               "Re-login and use account-parties add to associate manually."
            << std::endl;
    } else {
        refdata::messaging::get_parties_request parties_req;
        parties_req.limit = 1000;
        auto parties = do_request(out, session, parties_req, std::chrono::seconds(30), true);
        if (parties) {
            iam::messaging::save_account_party_request assoc_req;
            try {
                const auto account_uuid =
                    boost::lexical_cast<boost::uuids::uuid>(session.auth().account_id);
                for (const auto& party : parties->parties) {
                    if (party.party_category != "Operational")
                        continue;
                    iam::domain::account_party association;
                    association.tenant_id = party.tenant_id.to_string();
                    association.account_id = account_uuid;
                    association.party_id = party.id;
                    association.modified_by = username;
                    association.performed_by = username;
                    association.change_reason_code =
                        std::string(dq::domain::change_reason_constants::codes::new_record);
                    association.change_commentary =
                        "Tenant provisioning: tenant admin associated with party";
                    assoc_req.account_parties.push_back(std::move(association));
                }
            } catch (const boost::bad_lexical_cast&) {
                out << "⚠ Session account id is not a UUID; skipping association." << std::endl;
            }
            if (!assoc_req.account_parties.empty()) {
                auto assoc = do_request(out, session, assoc_req, std::chrono::seconds(30), true);
                if (assoc && assoc->success)
                    linked = static_cast<int>(assoc_req.account_parties.size());
                else
                    out << "⚠ Party association failed; continuing (associate "
                           "manually with account-parties add)."
                        << std::endl;
            }
        } else {
            out << "⚠ Could not list parties; continuing without association." << std::endl;
        }
        // The association phase is non-fatal: clear any failure mark
        // its requests may have left so the script does not abort.
        command_feedback::reset();
    }
    out << "  " << linked << " part" << (linked == 1 ? "y" : "ies") << " associated." << std::endl;

    // Phase 4: complete provisioning (fatal). This clears
    // system.bootstrap_mode and sets onboarding.tenant = true server-side,
    // over its own NATS-routed, permission-check-free path — see
    // ores.iam.core/messaging/tenant_handler.hpp. No separate client-side
    // save_setting_request is needed (a prior version issued one
    // pre-emptively here, but it was redundant with the authoritative
    // server-side clear and risked writing a party-scoped duplicate row if
    // the acting user had a party selected).
    out << "[4/4] Finalizing tenant provisioning..." << std::endl;
    iam::messaging::complete_tenant_provisioning_command complete_req;
    auto completed = do_request(out, session, complete_req, std::chrono::seconds(30), true);
    if (!completed)
        return;
    if (!completed->success) {
        fail(out) << "Failed to complete tenant provisioning: " << completed->message << std::endl;
        return;
    }

    out << "✓ Tenant provisioned: bundle '" << bundle_code << "', " << linked << " part"
        << (linked == 1 ? "y" : "ies") << " associated." << std::endl;
    out << "Next: logout, then log back in — the party setup is per party; run "
           "provision party <party>."
        << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Tenant provisioned; bundle " << bundle_code;
}

void provision_commands::process_party(std::ostream& out,
                                       nats_client& session,
                                       const std::vector<std::string>& args) {
    auto parsed =
        parse_args(args,
                   {{.name = "dataset-size", .requires_value = true, .default_value = "small"},
                    {.name = "reports", .requires_value = true, .default_value = "all"},
                    {.name = "timeout",
                     .requires_value = true,
                     .default_value = std::to_string(default_wait_timeout.count())}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: provision party <party-uuid-or-full-name> "
                     "[--dataset-size small|large] [--reports all|none|<name,...>] "
                     "[--timeout <seconds>]"
                  << std::endl;
        return;
    }

    const auto& dataset_size = parsed->flag("dataset-size");
    if (dataset_size != "small" && dataset_size != "large") {
        fail(out) << "--dataset-size must be small or large: " << dataset_size << std::endl;
        return;
    }
    auto wait_timeout = parse_positive_seconds(parsed->flag("timeout"));
    if (!wait_timeout) {
        fail(out) << "Timeout must be a positive number of seconds: " << parsed->flag("timeout")
                  << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }
    const auto username = session.auth().username;
    const auto& party_ref = parsed->positionals.front();

    // Resolve the party by UUID or exact full name.
    auto find_party = [&](std::ostream& o) -> std::optional<refdata::domain::party> {
        refdata::messaging::get_parties_request req;
        req.limit = 1000;
        auto parties = do_request(o, session, req, std::chrono::seconds(30), true);
        if (!parties)
            return std::nullopt;
        std::optional<boost::uuids::uuid> ref_uuid;
        try {
            ref_uuid = boost::lexical_cast<boost::uuids::uuid>(party_ref);
        } catch (const boost::bad_lexical_cast&) {
        }
        for (const auto& party : parties->parties) {
            if ((ref_uuid && party.id == *ref_uuid) || (!ref_uuid && party.full_name == party_ref))
                return party;
        }
        fail(o) << "Party not found (by UUID or exact full name): " << party_ref << std::endl;
        return std::nullopt;
    };

    auto party = find_party(out);
    if (!party)
        return;
    out << "Provisioning party '" << party->full_name << "' (ID: " << party->id << ")."
        << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Provisioning party " << party->id << " (dataset " << dataset_size
                              << ")";

    // Phase 1: publish the counterparty dataset, as the wizard's
    // counterparty import does (bundle "base", opted-in GLEIF
    // counterparties dataset of the requested size).
    out << "[1/6] Importing counterparties (dataset " << dataset_size << ")..." << std::endl;
    {
        dq::messaging::publish_bundle_request req;
        req.bundle_code = "base";
        req.mode = dq::domain::publication_mode::upsert;
        req.published_by = username;
        req.atomic = true;
        dq::messaging::publish_bundle_params params;
        params.opted_in_datasets.push_back("gleif.lei_counterparties." + dataset_size);
        req.params_json = dq::messaging::build_params_json(params);
        auto published = do_request(out, session, req, publish_timeout, true);
        if (!published)
            return;
        if (!published->success) {
            fail(out) << "Failed to publish counterparties: " << published->error_message
                      << std::endl;
            return;
        }
        out << "  Dispatched " << published->datasets_dispatched
            << " dataset(s); workflow instance: " << published->instance_id << std::endl;
        if (!workflow_commands::wait_for_instance(
                out,
                session,
                published->instance_id,
                *wait_timeout,
                static_cast<std::size_t>(published->datasets_dispatched)))
            return;
    }

    // Phase 2: publish the organisation bundle for this party.
    out << "[2/6] Publishing organisation bundle for the party..." << std::endl;
    {
        dq::messaging::publish_bundle_request req;
        req.bundle_code = "organisation";
        req.mode = dq::domain::publication_mode::upsert;
        req.published_by = username;
        req.atomic = true;
        dq::messaging::publish_bundle_params params;
        params.party_id = boost::uuids::to_string(party->id);
        req.params_json = dq::messaging::build_params_json(params);
        auto published = do_request(out, session, req, publish_timeout, true);
        if (!published)
            return;
        if (!published->success) {
            fail(out) << "Failed to publish organisation bundle: " << published->error_message
                      << std::endl;
            return;
        }
        out << "  Dispatched " << published->datasets_dispatched
            << " dataset(s); workflow instance: " << published->instance_id << std::endl;
        if (!workflow_commands::wait_for_instance(
                out,
                session,
                published->instance_id,
                *wait_timeout,
                static_cast<std::size_t>(published->datasets_dispatched)))
            return;
    }

    // Phase 3: create the selected report definitions. The wizard
    // pre-checks every template; --reports none skips, a comma list
    // selects by exact name. Failures are fatal, as in the wizard.
    const auto& reports = parsed->flag("reports");
    if (reports == "none") {
        out << "[3/6] Skipping report definitions (--reports none)." << std::endl;
    } else {
        out << "[3/6] Creating report definitions..." << std::endl;
        dq::messaging::list_dq_report_definition_templates_request templates_req;
        auto templates = do_request(out, session, templates_req, std::chrono::seconds(30), true);
        if (!templates)
            return;
        if (!templates->success) {
            fail(out) << "Failed to list report templates: " << templates->message << std::endl;
            return;
        }

        std::vector<dq::messaging::dq_report_definition_template> selected;
        if (reports == "all") {
            selected = templates->templates;
        } else {
            for (const auto& name : [&] {
                     std::vector<std::string> names;
                     std::string current;
                     for (const char c : reports) {
                         if (c == ',') {
                             if (!current.empty())
                                 names.push_back(current);
                             current.clear();
                         } else {
                             current += c;
                         }
                     }
                     if (!current.empty())
                         names.push_back(current);
                     return names;
                 }()) {
                const auto i = std::find_if(templates->templates.begin(),
                                            templates->templates.end(),
                                            [&](const auto& tpl) { return tpl.name == name; });
                if (i == templates->templates.end()) {
                    fail(out) << "Unknown report template: " << name << std::endl;
                    return;
                }
                selected.push_back(*i);
            }
        }

        // Report definitions are owned by the System party, as the
        // wizard resolves it (fallback: the first party).
        refdata::messaging::get_parties_request parties_req;
        parties_req.limit = 1000;
        auto parties = do_request(out, session, parties_req, std::chrono::seconds(30), true);
        if (!parties || parties->parties.empty()) {
            fail(out) << "Could not resolve the System party for report ownership." << std::endl;
            return;
        }
        auto system_party =
            std::find_if(parties->parties.begin(), parties->parties.end(), [](const auto& p) {
                return p.party_category == "System";
            });
        const auto owner_id =
            system_party != parties->parties.end() ? system_party->id : parties->parties.front().id;

        boost::uuids::random_generator gen;
        for (const auto& tpl : selected) {
            reporting::messaging::save_report_definition_request req;
            req.definition.id = gen();
            req.definition.name = tpl.name;
            req.definition.description = tpl.description;
            req.definition.report_type = tpl.report_type;
            req.definition.schedule_expression = tpl.schedule_expression;
            req.definition.concurrency_policy = tpl.concurrency_policy;
            req.definition.party_id = owner_id;
            req.definition.modified_by = username;
            req.definition.performed_by = username;
            req.definition.change_reason_code =
                std::string(dq::domain::change_reason_constants::codes::new_record);
            req.definition.change_commentary = "Created during party provisioning";
            req.definition.recorded_at = std::chrono::system_clock::now();
            auto saved = do_request(out, session, req, std::chrono::seconds(30), true);
            if (!saved)
                return;
            if (!saved->success) {
                fail(out) << "Failed to create report definition '" << tpl.name
                          << "': " << saved->message << std::endl;
                return;
            }
            out << "  Created report definition '" << tpl.name << "'." << std::endl;
        }
    }

    // Phase 4: publish the synthetic FX spot config dataset — a member
    // of the synthetic_realistic bundle (alongside report definitions;
    // not the plain 2-pair ore_analytics fx_spot_configs starter, so
    // the party's driver feeds cover all 11 currency pairs the CRM
    // story's majors/exotics topologies need real, live ticks for),
    // opted in on its own so this doesn't re-trigger report creation
    // already handled by Phase 3.
    out << "[4/6] Publishing synthetic FX spot configs for the party..." << std::endl;
    {
        dq::messaging::publish_bundle_request req;
        req.bundle_code = "synthetic_realistic";
        req.mode = dq::domain::publication_mode::upsert;
        req.published_by = username;
        req.atomic = true;
        dq::messaging::publish_bundle_params params;
        params.party_id = boost::uuids::to_string(party->id);
        params.opted_in_datasets.push_back("synthetic.fx_spot_configs.realistic");
        req.params_json = dq::messaging::build_params_json(params);
        auto published = do_request(out, session, req, publish_timeout, true);
        if (!published)
            return;
        if (!published->success) {
            fail(out) << "Failed to publish synthetic FX spot configs: " << published->error_message
                      << std::endl;
            return;
        }
        out << "  Dispatched " << published->datasets_dispatched
            << " dataset(s); workflow instance: " << published->instance_id << std::endl;
        if (!workflow_commands::wait_for_instance(
                out,
                session,
                published->instance_id,
                *wait_timeout,
                static_cast<std::size_t>(published->datasets_dispatched)))
            return;
    }

    // Phase 5: publish the curated FX driver-rate dataset — the
    // marketdata.reference_vintage_2016_02_05 bundle's first member,
    // so the party has real market series/observations to browse
    // without a separate manual step.
    out << "[5/6] Publishing FX driver rates for the party..." << std::endl;
    {
        dq::messaging::publish_bundle_request req;
        req.bundle_code = "marketdata.reference_vintage_2016_02_05";
        req.mode = dq::domain::publication_mode::upsert;
        req.published_by = username;
        req.atomic = true;
        dq::messaging::publish_bundle_params params;
        params.party_id = boost::uuids::to_string(party->id);
        req.params_json = dq::messaging::build_params_json(params);
        auto published = do_request(out, session, req, publish_timeout, true);
        if (!published)
            return;
        if (!published->success) {
            fail(out) << "Failed to publish FX driver rates: " << published->error_message
                      << std::endl;
            return;
        }
        out << "  Dispatched " << published->datasets_dispatched
            << " dataset(s); workflow instance: " << published->instance_id << std::endl;
        if (!workflow_commands::wait_for_instance(
                out,
                session,
                published->instance_id,
                *wait_timeout,
                static_cast<std::size_t>(published->datasets_dispatched)))
            return;
    }

    // Phase 6: activate the party. Hard failure by design — a
    // completed run must mean a provisioned party (the wizard merely
    // warns here).
    out << "[6/6] Activating party '" << party->full_name << "'..." << std::endl;
    auto fresh = find_party(out);
    if (!fresh)
        return;
    fresh->status = "Active";
    fresh->modified_by = username;
    fresh->performed_by = username;
    fresh->change_reason_code = std::string(dq::domain::change_reason_constants::codes::new_record);
    fresh->change_commentary = "Party provisioning completed via shell";

    refdata::messaging::save_party_request save_req;
    save_req.data = std::move(*fresh);
    auto saved = do_request(out, session, save_req, std::chrono::seconds(30), true);
    if (!saved)
        return;
    if (!saved->success) {
        fail(out) << "Failed to activate party: " << saved->message << std::endl;
        return;
    }

    // Write onboarding.party = true, independent of the party's own status
    // — this is what login checks to decide whether to re-launch the party
    // wizard, not party.status. Warn-only: the wizard's own version of this
    // step is likewise non-fatal.
    variability::messaging::complete_party_onboarding_request onboarding_req;
    onboarding_req.party_id = boost::uuids::to_string(party->id);
    auto onboarding_result =
        do_request(out, session, onboarding_req, std::chrono::seconds(30), true);
    if (!onboarding_result || !onboarding_result->success) {
        out << "⚠ Could not record party onboarding completion; the party setup wizard may "
               "reappear on next login."
            << std::endl;
        command_feedback::reset();
    }

    out << "✓ Party '" << party->full_name << "' provisioned and active." << std::endl;
    out << "Next: logout and log back in to use the party." << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Party provisioned: " << party->id;
}

}
