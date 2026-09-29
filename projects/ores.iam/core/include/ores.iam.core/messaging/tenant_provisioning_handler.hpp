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
#ifndef ORES_IAM_MESSAGING_TENANT_PROVISIONING_HANDLER_HPP
#define ORES_IAM_MESSAGING_TENANT_PROVISIONING_HANDLER_HPP

#include "ores.assets.api/messaging/image_protocol.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.dq.api/messaging/dataset_protocol.hpp"
#include "ores.dq.api/messaging/party_provisioning_plan.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.dq.api/messaging/publish_params.hpp"
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/tenant_provisioning_protocol.hpp"
#include "ores.iam.api/workflow/provision_tenant_workflow.hpp"
#include "ores.iam.core/messaging/provision_step_arguments.hpp"
#include "ores.iam.core/repository/tenant_repository.hpp"
#include "ores.iam.core/service/account_party_service.hpp"
#include "ores.iam.core/service/account_service.hpp"
#include "ores.iam.core/service/internal_impersonation_service.hpp"
#include "ores.iam.core/service/internal_request_client.hpp"
#include "ores.iam.core/service/seed_profile_parameter_check.hpp"
#include "ores.iam.core/service/seed_profile_parameter_service.hpp"
#include "ores.iam.core/service/seed_profile_service.hpp"
#include "ores.iam.core/service/seed_profile_step_service.hpp"
#include "ores.iam.core/service/tenant_provisioning_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/feed_binding_protocol.hpp"
#include "ores.marketdata.api/messaging/market_feed_config_protocol.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.api/messaging/folder_protocol.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include "ores.variability.api/messaging/operations_protocol.hpp"
#include "ores.workflow.api/messaging/workflow_events.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <functional>
#include <optional>
#include <rfl/json.hpp>
#include <stdexcept>
#include <thread>
#include <unordered_map>
#include <utility>
#include <vector>

namespace ores::iam::messaging {

namespace {

inline auto& tenant_provisioning_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.iam.messaging.tenant_provisioning_handler");
    return instance;
}

/// The first field a provision request must carry and does not, or an empty
/// string when it carries them all. The field is named because the person who
/// left it out is the one who has to fill it in.
[[nodiscard]] inline std::string first_empty_field(const provision_tenant_command& req) {
    const std::vector<std::pair<std::string, const std::string&>> required{
        {"tenant_code", req.tenant_code},
        {"tenant_name", req.tenant_name},
        {"tenant_hostname", req.tenant_hostname},
        {"admin_username", req.admin_username},
        {"admin_email", req.admin_email},
        {"admin_password", req.admin_password}};
    for (const auto& [name, value] : required)
        if (value.empty())
            return "The field '" + name + "' has no value.";
    return {};
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using ores::database::service::tenant_context;
using ores::database::repository::execute_parameterized_string_query;
using ores::database::repository::execute_parameterized_command;
using ores::database::repository::execute_parameterized_multi_column_query;
using ores::iam::service::internal_impersonation_service;
using ores::iam::service::internal_request_client;
using namespace ores::logging;

class tenant_provisioning_handler {
public:
    tenant_provisioning_handler(ores::nats::service::client& nats,
                                ores::database::context ctx,
                                ores::security::jwt::jwt_authenticator signer,
                                ores::iam::service::internal_impersonation_service impersonation)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , signer_(std::move(signer))
        , impersonation_(std::move(impersonation)) {}

    void complete_provisioning(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(tenant_provisioning_handler_lg(), msg);
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }

            const auto actor = ctx_expected->actor();
            auto ids =
                execute_parameterized_string_query(*ctx_expected,
                                                   "SELECT ores_iam_current_tenant_id_fn()::text",
                                                   {},
                                                   tenant_provisioning_handler_lg(),
                                                   "complete_provisioning");
            if (ids.empty()) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), error)
                    << "complete_provisioning: no tenant ID in context";
                reply(nats_,
                      msg,
                      complete_tenant_provisioning_response{.success = false,
                                                            .message = "No tenant context"});
                return;
            }

            auto sys_ctx = tenant_context::with_system_tenant(ctx_);
            execute_parameterized_command(sys_ctx,
                                          "SELECT ores_iam_mark_tenant_active_fn($1::uuid, $2)",
                                          {ids.front(), actor},
                                          tenant_provisioning_handler_lg(),
                                          "complete_provisioning");

            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "Tenant marked active: " << ids.front();

            // Clear the bootstrap_mode flag via the variability service over
            // NATS, so the write happens under its own (correctly-granted) DB
            // role rather than IAM's. The variability-side handler has no
            // permission check on this subject — independent of the user
            // having variability::flags:create — so forwarding the original
            // bearer (scoping the tenant) is sufficient.
            try {
                using namespace ores::variability::messaging;
                const clear_bootstrap_mode_request req{};
                const auto& codec = ores::nats::default_wire_codec();
                const auto bytes = codec.encode(req);

                const auto resp_msg =
                    nats_.request_sync(clear_bootstrap_mode_request::nats_subject,
                                       bytes,
                                       ores::nats::service::forwarded_caller_headers(msg),
                                       std::chrono::seconds(5));
                const auto resp = codec.decode<clear_bootstrap_mode_response>(resp_msg.data);
                if (resp && resp->result.outcome == ores::utility::domain::outcome::ok) {
                    BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                        << "Bootstrap mode cleared for tenant: " << ids.front();
                } else {
                    BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                        << "Failed to clear bootstrap mode for tenant " << ids.front() << ": "
                        << (resp ? resp->result.message : "malformed response");
                }
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                    << "Failed to clear bootstrap mode for tenant " << ids.front() << ": "
                    << e.what();
            }

            reply(nats_, msg, complete_tenant_provisioning_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_,
                  msg,
                  complete_tenant_provisioning_response{.success = false, .message = e.what()});
        }
    }

    /**
     * @brief Provisions a tenant from a seed profile.
     *
     * A refusal happens before anything is created: an unknown profile code, a
     * tenant or administrator field the request leaves empty, and a parameter
     * the profile does not declare or whose value its data type or its choices
     * refuse. Only then are the tenant and its administrator created, and the
     * steps the profile orders are started as a workflow instance whose id the
     * answer carries.
     */
    void provision_tenant(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(tenant_provisioning_handler_lg(), msg);

        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        if (!has_permission(*ctx_expected, "iam::tenants:create")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto req = decode<provision_tenant_command>(msg);
        if (!req) {
            reply(nats_,
                  msg,
                  provision_tenant_command_response{.success = false,
                                                    .message = "Invalid request payload."});
            return;
        }

        try {
            const auto empty_field = first_empty_field(*req);
            if (!empty_field.empty()) {
                reply(nats_,
                      msg,
                      provision_tenant_command_response{.success = false, .message = empty_field});
                return;
            }

            // A profile is system-owned registered data, and the SQL provisioner
            // refuses to run outside the system tenant anyway, so the read and
            // the write share one context.
            auto sys_ctx = tenant_context::with_system_tenant(ctx_);
            const auto profiles =
                ores::iam::service::seed_profile_service(sys_ctx).list_seed_profiles(0, 1000);
            const auto profile =
                std::find_if(profiles.begin(), profiles.end(), [&](const auto& candidate) {
                    return candidate.code == req->profile_code;
                });
            if (profile == profiles.end()) {
                reply(nats_,
                      msg,
                      provision_tenant_command_response{.success = false,
                                                        .message = "The seed profile '" +
                                                                   req->profile_code +
                                                                   "' does not exist."});
                return;
            }

            const auto profile_id = boost::uuids::to_string(profile->id);
            const auto declared =
                ores::iam::service::seed_profile_parameter_service(sys_ctx)
                    .list_seed_profile_parameters_by_seed_profile_id(profile_id, 0, 1000);
            const auto checked = ores::iam::service::check_parameters(declared, req->parameters);
            if (!checked.accepted()) {
                reply(nats_,
                      msg,
                      provision_tenant_command_response{.success = false,
                                                        .message = checked.refusal});
                return;
            }

            // The profile's kinds are checked before the tenant exists. A run
            // that cannot execute one of them must refuse without leaving a
            // tenant behind in bootstrapping, and the workflow service's own
            // check cannot do it because it runs after this handler created the
            // row.
            const auto declared_steps =
                ores::iam::service::seed_profile_step_service(sys_ctx)
                    .list_seed_profile_steps_by_seed_profile_id(profile_id, 0, 1000);
            for (const auto& step : declared_steps) {
                if (ores::iam::workflow::is_executed_step_kind(step.step_kind))
                    continue;
                // A kind the catalogue does not know and a kind this build does
                // not execute are both refused, and the message says which:
                // a typo in a profile's row reads differently to an operator
                // than a kind that is real but unbuilt.
                const auto reason = ores::iam::workflow::is_declared_step_kind(step.step_kind) ?
                                        "', which this deployment does not execute." :
                                        "', which this deployment does not know.";
                reply(nats_,
                      msg,
                      provision_tenant_command_response{
                          .success = false,
                          .message = "The seed profile '" + profile->code +
                                     "' orders the step kind '" + step.step_kind + reason});
                return;
            }

            const auto created = ores::iam::service::tenant_provisioning_service(sys_ctx).provision(
                profile->tenant_type,
                req->tenant_code,
                req->tenant_name,
                req->tenant_hostname,
                req->tenant_description,
                req->admin_username,
                req->admin_email,
                req->admin_password,
                profile->force_password_change);

            ores::iam::workflow::provision_tenant_workflow_request run;
            run.profile_code = profile->code;
            run.tenant_id = created.tenant_id;
            run.tenant_code = req->tenant_code;
            run.tenant_hostname = req->tenant_hostname;
            run.admin_account_id = created.account_id;
            for (const auto& value : checked.values)
                run.parameters.push_back({value.name, value.value});
            for (const auto& step : declared_steps)
                run.steps.push_back({step.step_kind, step.arguments_json});

            // The instance id is minted here, so the answer names the run the
            // caller follows without waiting for the engine to create it. The
            // engine treats a repeat of the same id as the same run.
            boost::uuids::random_generator generate;
            const auto instance_id = boost::uuids::to_string(generate());

            ores::workflow::messaging::start_workflow_message start;
            start.type = std::string(ores::iam::workflow::provision_tenant_workflow_type);
            // The run belongs to the tenant that asked for it, not to the one
            // it creates. A run is a tenant's own work and the progress read
            // answers the tenant that owns it, so a run owned by the tenant
            // being created could not be followed by the person who asked for
            // it: their session is in another tenant. The tenant being
            // provisioned travels in the request and in every step command.
            start.tenant_id = ctx_expected->tenant_id().to_string();
            start.request_json = rfl::json::write(run);
            start.correlation_id = correlation_id;
            start.instance_id = instance_id;

            nats_.js_publish(ores::workflow::messaging::start_workflow_message::nats_subject,
                             ores::nats::default_wire_codec().encode(start),
                             ores::nats::service::forwarded_caller_headers(msg));

            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "Started " << start.type << " for tenant " << req->tenant_code
                << " (instance: " << instance_id << ")";

            reply(nats_,
                  msg,
                  provision_tenant_command_response{.success = true,
                                                    .instance_id = instance_id,
                                                    .tenant_id = created.tenant_id,
                                                    .account_id = created.account_id});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_,
                  msg,
                  provision_tenant_command_response{.success = false, .message = e.what()});
        }
    }

    /**
     * @brief Serves one step of a provision tenant run.
     *
     * The engine dispatches every step of a run to one subject and has no
     * caller token to forward, so this handler reads the step context from the
     * message headers, replays a recorded outcome when the engine has already
     * seen this step, and otherwise does the kind's work as the run's
     * administrator. A message that is not a workflow command is left alone:
     * somebody else's request on the shared subject is not this handler's to
     * answer.
     */
    void provision_step(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(tenant_provisioning_handler_lg(), msg);

        auto wf = ores::service::messaging::workflow_step_context::from_message(nats_, msg);
        if (!wf)
            return;

        try {
            if (const auto cached = ores::service::messaging::check_step_idempotency(
                    nats_, wf->step_id, wf->tenant_id)) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                    << "provision_step: replaying the recorded outcome of step " << wf->step_id;
                ores::service::messaging::publish_step_completion(nats_,
                                                                  wf->step_id,
                                                                  wf->instance_id,
                                                                  cached->outcome,
                                                                  cached->result_json,
                                                                  cached->error_message,
                                                                  cached->log);
                return;
            }

            const std::string_view payload(reinterpret_cast<const char*>(msg.data.data()),
                                           msg.data.size());
            auto parsed =
                rfl::json::read<ores::iam::workflow::provision_tenant_step_command>(payload);
            if (!parsed) {
                wf->fail("The provisioning step command could not be read: " +
                         std::string(parsed.error().what()));
                return;
            }

            execute_step(*wf, *parsed);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), error)
                << "provision_step failed: " << e.what();
            wf->fail(e.what());
        }
    }

    // Data-driven holding-group spec: which office maps to which bundle and
    // full_name, in publish order. Adding an office is a new row here plus
    // the bundle/dataset registrations in acme_bundle_populate.sql /
    // acme_dataset_populate.sql -- not a code change to the loop below.
    struct acme_office {
        std::string code;
        std::string bundle_code;
        std::string full_name;
    };

    static const std::vector<acme_office>& acme_offices() {
        static const std::vector<acme_office> offices{
            {"acme_uk", "acme_uk", "ACME Corporation UK plc"},
            {"acme_us", "acme_us", "ACME Corporation US Inc"},
            {"acme_hk", "acme_hk", "ACME Corporation HK Ltd"},
        };
        return offices;
    }

    void provision_acme(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(tenant_provisioning_handler_lg(), msg);
        provision_acme_tenant_response resp;
        resp.success = true;
        auto add_step = [&](std::string step, std::string action, std::uint64_t count = 1) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "provision_acme: " << step << ": " << action;
            resp.steps.push_back(provision_acme_tenant_step{
                .step = std::move(step), .action = std::move(action), .record_count = count});
        };
        // Abort-path helper (F11): flips the response to failure and carries
        // the DQ-side error detail publish_bundle recorded in the trailing
        // <bundle>.failed step (the publication error_message on a dispatch
        // failure, or the wait client's exact reason) into the top-level
        // message, so a failed provisioning never replies with success=true.
        auto fail_with = [&](const std::string& summary) {
            resp.success = false;
            resp.message = summary;
            for (auto it = resp.steps.rbegin(); it != resp.steps.rend(); ++it)
                if (it->step.ends_with(".failed")) {
                    resp.message += " -- " + it->action;
                    break;
                }
        };

        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }

            const auto bearer = ores::nats::service::extract_actor_bearer(msg);
            auto claims = signer_.validate(bearer);
            if (!claims) {
                reply(nats_,
                      msg,
                      provision_acme_tenant_response{.success = false,
                                                     .message = "Invalid or expired token"});
                return;
            }

            const auto tenant_id_str = claims->tenant_id.value_or("");
            if (tenant_id_str.empty()) {
                reply(nats_,
                      msg,
                      provision_acme_tenant_response{.success = false,
                                                     .message = "No tenant context"});
                return;
            }

            boost::uuids::string_generator sg;
            const auto account_id = sg(claims->subject);
            const auto caller_party_id = (claims->party_id && !claims->party_id->empty()) ?
                                             sg(*claims->party_id) :
                                             boost::uuids::nil_uuid();
            const auto username = claims->username.value_or("");

            auto mint = [&](const boost::uuids::uuid& party_id) {
                return impersonation_.mint_token(
                    *ctx_expected, tenant_id_str, account_id, party_id, username);
            };
            // Impersonation tokens are deliberately short-lived (see
            // internal_impersonation_service::mint_token()); every client
            // built here drives polling loops that routinely outlive that
            // TTL, so each is wired to re-mint for the same party on
            // expiry rather than fail once the token goes stale mid-wait.
            auto make_client = [&](const boost::uuids::uuid& party_id) {
                return internal_request_client(
                    nats_, mint(party_id), [&mint, party_id] { return mint(party_id); });
            };
            auto progress = [&](const std::string& step) {
                return [&, step](const std::string& line) {
                    add_step(step, line, 0);
                };
            };

            // Step 1: the generic 'base' bundle (countries, currencies,
            // calendars, fpml.* reference codes, GLEIF entities/
            // relationships, and the badge system) -- tenant-wide, same
            // bundle Barclays' generic provision-party flow publishes, so
            // Acme gets full reference-data parity instead of only the
            // Acme-specific datasets below. Must run before Step 2: the
            // fpml.business_center rows here are what acme_lei_import's
            // LEI-imported parties are addressed against. opted_in_datasets
            // pulls in real GLEIF counterparties (small -- ~13k rows, still
            // the slowest single step here), mirroring Barclays' own
            // opted_in_datasets. Best-effort: every party a tester actually
            // cares about is created in steps 3+ regardless of whether this
            // large, non-blocking dataset finishes within the wait window,
            // so a slow/failed import here is logged and does not abort
            // the rest of provisioning.
            {
                internal_request_client client = make_client(caller_party_id);
                add_step("Step 1: Publishing base reference data", "starting", 0);
                publish_bundle(client,
                               "base",
                               username,
                               R"({"opted_in_datasets": ["gleif.lei_counterparties.small"]})",
                               add_step,
                               progress("base"),
                               std::chrono::seconds{1500});
            }

            // Step 2: the four-party Acme Corporation LEI hierarchy --
            // tenant-wide, so scoped to the caller's own (already-active)
            // party rather than a not-yet-created one.
            {
                internal_request_client client = make_client(caller_party_id);
                add_step("Step 2: Importing Acme Corporation LEI hierarchy", "starting", 0);
                if (!publish_bundle(client,
                                    "acme_lei_import",
                                    username,
                                    lei_import_params(),
                                    add_step,
                                    progress("acme_lei_import"),
                                    std::chrono::seconds{600})) {
                    fail_with("Step 2 failed: acme_lei_import");
                    reply(nats_, msg, resp);
                    return;
                }
            }

            // Step 3: the holding company -- activation, logo, onboarding,
            // the tenant admin's default party, and its own group-level
            // staff (no desks/business units of its own, just a handful of
            // group-level roles like the Group CEO).
            {
                internal_request_client discover = make_client(caller_party_id);
                auto holding = find_party(discover, "Acme Corporation Plc");
                if (!holding) {
                    add_step("acme_group.skipped", "party_not_found", 0);
                } else {
                    internal_request_client client = make_client(holding->id);
                    add_step("Step 3: Activating Acme Corporation Plc", "starting", 0);
                    finish_party(client,
                                 *ctx_expected,
                                 tenant_id_str,
                                 account_id,
                                 username,
                                 *holding,
                                 /*set_default=*/true);
                    add_step("Step 3: Activating Acme Corporation Plc", "completed");

                    add_step("Step 3: Publishing group-level staff", "starting", 0);
                    dq::messaging::publish_bundle_params groupParams;
                    groupParams.party_id = boost::uuids::to_string(holding->id);
                    // Aborts on failure, like Step 2 -- unlike Step 1's best-
                    // effort base-bundle publish, this one is directly
                    // responsible for the Group CEO account every office's
                    // Country Head reports to; letting it fail silently would
                    // just reproduce the "three disconnected per-office
                    // trees" bug this dataset exists to fix, with no error
                    // surfaced anywhere in the step log.
                    if (!publish_bundle(client,
                                        "acme_group",
                                        username,
                                        dq::messaging::build_params_json(groupParams),
                                        add_step,
                                        progress("acme_group"))) {
                        fail_with("Step 3 failed: acme_group");
                        reply(nats_, msg, resp);
                        return;
                    }

                    add_step("acme_group.staff_photos", "starting", 0);
                    attach_staff_photos_best_effort(
                        client, *ctx_expected, tenant_id_str, "acme.acme_group.accounts");
                    add_step("acme_group.staff_photos", "completed");

                    // Market data for the holding party: only its own CRM
                    // topology (Cross-Rates Matrix). The synthetic theme and
                    // FX driver rates publish once, against the system party
                    // (see the system-party market-data step after the office
                    // loop below) -- offices and the holding consume the
                    // shared stream via per-party feed bindings instead of
                    // owning their own copies of the sim config. Without
                    // this, the holding party has no CRM (Cross-Rates Matrix)
                    // at all: a treasury user logged in at the
                    // default/holding-company party saw a blank matrix with
                    // no way to get FX visibility short of switching party
                    // to an office.
                    const std::vector<dq::messaging::party_bundle_publish_step> group_mkt_plan{
                        {"crm_topology", "CRM Cross-Rates Matrix topology"}};
                    std::string current_group_mkt_step;
                    dq::messaging::publish_party_provisioning_plan(
                        group_mkt_plan,
                        holding->id,
                        [&](const std::string& bundle_code, const std::string& params_json)
                            -> std::optional<dq::messaging::publish_bundle_response> {
                            dq::messaging::publish_bundle_request req;
                            req.bundle_code = bundle_code;
                            req.published_by = username;
                            req.atomic = true;
                            req.params_json = params_json;
                            auto pub = client.request(req);
                            const auto mkt_label = "acme_group." + bundle_code;
                            if (!pub.success) {
                                add_step(mkt_label + ".failed", pub.error_message, 0);
                                return std::nullopt;
                            }
                            add_step(mkt_label,
                                     "dispatched",
                                     static_cast<std::uint64_t>(pub.datasets_dispatched));
                            return pub;
                        },
                        [&](const std::string& instance_id, std::size_t expected) {
                            const auto mkt_label = "acme_group." + current_group_mkt_step;
                            auto wait_result =
                                client.wait_for_workflow_instance(instance_id,
                                                                  std::chrono::seconds{120},
                                                                  expected,
                                                                  progress(mkt_label));
                            if (!wait_result.success)
                                add_step(mkt_label + ".failed", wait_result.error, 0);
                            return wait_result.success;
                        },
                        [&](const auto& step) {
                            current_group_mkt_step = step.bundle_code;
                            add_step("acme_group." + step.bundle_code, "starting", 0);
                        });
                    // Best-effort, like the per-office market-data plan is
                    // NOT (that one aborts the whole office on failure) --
                    // deliberately less strict here since Step 3 already
                    // committed to activating the holding party and
                    // publishing its group-level staff by this point; a
                    // failed market-data publish shouldn't undo that.
                }
            }

            // Step 4+: per-office business units/portfolios/books/accounts,
            // then activation/logo/onboarding/membership for that party.
            // office_parties accumulates (code, party_id) as each office is
            // resolved, so the market-data and cross-entity steps below can
            // reuse these NATS-resolved IDs rather than re-querying
            // ores_refdata_parties_tbl directly.
            std::vector<std::pair<std::string, boost::uuids::uuid>> office_parties;
            int step_num = 4;
            for (const auto& office : acme_offices()) {
                internal_request_client discover = make_client(caller_party_id);
                auto party = find_party(discover, office.full_name);
                if (!party) {
                    add_step(office.code + ".skipped", "party_not_found", 0);
                    continue;
                }
                office_parties.emplace_back(office.code, party->id);

                internal_request_client client = make_client(party->id);
                const auto label =
                    "Step " + std::to_string(step_num++) + ": Publishing " + office.full_name;
                add_step(label, "starting", 0);
                dq::messaging::publish_bundle_params params;
                params.party_id = boost::uuids::to_string(party->id);
                if (!publish_bundle(client,
                                    office.bundle_code,
                                    username,
                                    dq::messaging::build_params_json(params),
                                    add_step,
                                    progress(label)))
                    continue;
                add_step(label, "completed");

                finish_party(client,
                             *ctx_expected,
                             tenant_id_str,
                             account_id,
                             username,
                             *party,
                             /*set_default=*/false);
                add_step(office.code + ".onboarding", "completed");

                // Market data for the office: only its own CRM topology
                // (Cross-Rates Matrix). The synthetic theme and FX driver
                // rates are not published per office -- they publish once,
                // against the system party (see the system-party market-data
                // step after this loop), and this office consumes the shared
                // stream via feed bindings created by that same step. Its
                // series materialize per party from the stream, not from a
                // per-office copy of the config. The plan-iteration loop
                // itself (publish + wait per bundle) is shared with
                // ores.shell's "provision party" command -- see
                // publish_party_provisioning_plan()'s doc comment for why
                // it's a template rather than a shared class.
                const std::vector<dq::messaging::party_bundle_publish_step> mkt_plan{
                    {"crm_topology", "CRM Cross-Rates Matrix topology"}};
                std::string current_mkt_step;
                // on_step always runs immediately before its matching
                // publish/wait pair (guaranteed by the helper's loop body),
                // so wait can safely rely on current_mkt_step set just above.
                if (!dq::messaging::publish_party_provisioning_plan(
                        mkt_plan,
                        party->id,
                        [&](const std::string& bundle_code, const std::string& params_json)
                            -> std::optional<dq::messaging::publish_bundle_response> {
                            dq::messaging::publish_bundle_request req;
                            req.bundle_code = bundle_code;
                            req.published_by = username;
                            req.atomic = true;
                            req.params_json = params_json;
                            auto pub = client.request(req);
                            const auto mkt_label = office.code + "." + bundle_code;
                            if (!pub.success) {
                                add_step(mkt_label + ".failed", pub.error_message, 0);
                                return std::nullopt;
                            }
                            add_step(mkt_label,
                                     "dispatched",
                                     static_cast<std::uint64_t>(pub.datasets_dispatched));
                            return pub;
                        },
                        [&](const std::string& instance_id, std::size_t expected) {
                            const auto mkt_label = office.code + "." + current_mkt_step;
                            auto wait_result =
                                client.wait_for_workflow_instance(instance_id,
                                                                  std::chrono::seconds{120},
                                                                  expected,
                                                                  progress(mkt_label));
                            if (!wait_result.success)
                                add_step(mkt_label + ".failed", wait_result.error, 0);
                            return wait_result.success;
                        },
                        [&](const auto& step) {
                            current_mkt_step = step.bundle_code;
                            add_step(office.code + "." + step.bundle_code, "starting", 0);
                        }))
                    continue;

                const auto photo_label = office.code + ".staff_photos";
                add_step(photo_label, "starting", 0);
                attach_staff_photos_best_effort(
                    client, *ctx_expected, tenant_id_str, "acme." + office.code + ".accounts");
                add_step(photo_label, "completed");
            }

            // Step 7: simulated market data, owned by the system party
            // (consistent-world semantics -- every party sees the same
            // market; see doc/llm/specs/simulated-market-data-strategy.allium
            // and the F15 task). The two themes' configs and the FX
            // driver-rate vintage publish once, against the system party;
            // the system party, the holding and every office each get
            // per-party feed bindings on the Live workspace, and the
            // theme's feeds are started from the system party's folders.
            // Offices publish no theme and start no feeds. The bindings
            // come first so the ingest loop (which reacts to binding
            // changes via the notify trigger) is subscribed before the
            // first tick lands.
            {
                internal_request_client discover = make_client(caller_party_id);
                auto system_party = find_system_party(discover);
                if (!system_party) {
                    add_step("system_market_data.skipped", "system_party_not_found", 0);
                } else {
                    internal_request_client system_client = make_client(system_party->id);
                    const std::vector<dq::messaging::party_bundle_publish_step> system_mkt_plan{
                        {"synthetic_realistic_2026", "synthetic market data configuration (2026)"},
                        {"synthetic_ore_samples_2016",
                         "synthetic market data configuration (legacy ORE Samples)"},
                        {"marketdata.reference_vintage_2026_05_05", "FX driver rates"}};
                    std::string current_sys_mkt_step;
                    // Best-effort like the per-office market-data plan used
                    // to be: every party is fully provisioned by this point,
                    // so a failure here surfaces in the response's steps
                    // rather than undoing the whole tenant.
                    const bool mkt_ok = dq::messaging::publish_party_provisioning_plan(
                        system_mkt_plan,
                        system_party->id,
                        [&](const std::string& bundle_code, const std::string& params_json)
                            -> std::optional<dq::messaging::publish_bundle_response> {
                            dq::messaging::publish_bundle_request req;
                            req.bundle_code = bundle_code;
                            req.published_by = username;
                            req.atomic = true;
                            req.params_json = params_json;
                            auto pub = system_client.request(req);
                            const auto mkt_label = "system_market_data." + bundle_code;
                            if (!pub.success) {
                                add_step(mkt_label + ".failed", pub.error_message, 0);
                                return std::nullopt;
                            }
                            add_step(mkt_label,
                                     "dispatched",
                                     static_cast<std::uint64_t>(pub.datasets_dispatched));
                            return pub;
                        },
                        [&](const std::string& instance_id, std::size_t expected) {
                            const auto mkt_label = "system_market_data." + current_sys_mkt_step;
                            auto wait_result =
                                system_client.wait_for_workflow_instance(instance_id,
                                                                         std::chrono::seconds{120},
                                                                         expected,
                                                                         progress(mkt_label));
                            if (!wait_result.success)
                                add_step(mkt_label + ".failed", wait_result.error, 0);
                            return wait_result.success;
                        },
                        [&](const auto& step) {
                            current_sys_mkt_step = step.bundle_code;
                            add_step("system_market_data." + step.bundle_code, "starting", 0);
                        });

                    if (mkt_ok) {
                        // Per-party consumption: one binding per (tenant,
                        // party, workspace=Live, source) for the system
                        // party, the holding and every office, so each
                        // materializes its own observations from the shared
                        // stream (per-party series, identical values). Each
                        // party's bindings are saved through a client
                        // impersonating that party: feed-binding saves stamp
                        // the binding's party from the authenticated context
                        // (a security boundary), so one privileged client
                        // cannot create bindings on another party's behalf.
                        // Resolution of the theme's configs runs through the
                        // system party's client, because the synthetic
                        // tables are party-isolated by RLS and the configs
                        // live under the system party.
                        const std::string bind_label = "system_market_data.bindings";
                        add_step(bind_label, "starting", 0);
                        std::vector<std::pair<std::string, boost::uuids::uuid>> binding_parties;
                        binding_parties.emplace_back("system", system_party->id);
                        if (auto holding = find_party(discover, "Acme Corporation Plc"))
                            binding_parties.emplace_back("holding", holding->id);
                        for (const auto& [code, party_id] : office_parties)
                            binding_parties.emplace_back(code, party_id);

                        bool bindings_ok = true;
                        for (const auto& [code, party_id] : binding_parties) {
                            internal_request_client party_client = make_client(party_id);
                            if (!create_theme_feed_bindings(system_client,
                                                            party_client,
                                                            "synthetic.themes.realistic_2026",
                                                            boost::uuids::to_string(party_id))) {
                                add_step(bind_label + "." + code + ".failed",
                                         "see service log for details",
                                         0);
                                bindings_ok = false;
                            }
                        }
                        if (bindings_ok)
                            add_step(bind_label, "completed");

                        // Start the theme's feeds from the system party's
                        // folders via one folder-scoped request the server
                        // cascades across asset classes, so provisioning and
                        // manual start share one path. The folder cascade
                        // starts no feeds for offices -- they have no
                        // configs of their own.
                        const std::string feeds_label = "system_market_data.synthetic_feeds";
                        add_step(feeds_label, "starting", 0);
                        if (start_synthetic_theme_feeds(system_client,
                                                        "synthetic.themes.realistic_2026"))
                            add_step(feeds_label, "completed");
                        else
                            add_step(feeds_label + ".failed", "see service log for details", 0);
                    }
                }
            }

            // Step 8: cross-entity access for the "follow the sun" global-
            // book / risk-oversight roles -- deliberately narrow: only Desk
            // Heads on the two genuinely 24h-traded desks (IR Swaps, FX
            // Rates) get remote-booking membership on the London ("global
            // book") party, and only Market Risk staff get group-wide
            // risk-oversight membership across every operating company.
            // Credit Trading and Middle Office stay local-only, matching
            // how real multi-entity banks restrict cross-border booking
            // access to a narrow, individually-registered subset of staff
            // while granting risk oversight much more broadly. See
            // doc/knowledge/domains/acme_corporation_setup.org.
            {
                add_step("Step 7: Granting cross-entity access", "starting", 0);
                grant_cross_entity_access(*ctx_expected, tenant_id_str, office_parties);
                add_step("Step 8: Granting cross-entity access", "completed");
            }

            // Attaching the Barclays demo logo removed from here -- see the
            // "GLEIF import should natively attach counterparty logos when
            // available" cleanup task. The previous approach (a best-effort
            // poll for up to 8 minutes waiting for the GLEIF import to land
            // BARCLAYS PLC) added an unbounded-feeling silent tail to every
            // provision_acme call for a demo-only cosmetic touch; this
            // belongs in the GLEIF import itself, not bolted on afterwards.

            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "Acme tenant provisioned: " << tenant_id_str << " (" << resp.steps.size()
                << " step(s))";
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(
                nats_, msg, provision_acme_tenant_response{.success = false, .message = e.what()});
        }
    }

private:
    /// Who a provisioning step acts as. The engine sends a step without the
    /// caller's token, so the handler mints one for the run's administrator.
    struct step_actor {
        boost::uuids::uuid account_id;
        boost::uuids::uuid party_id;
        std::string username;
    };

    /// The result a step reports, so the run's record says what it acted on.
    struct provision_step_result {
        std::string kind;
        std::vector<std::string> bundles;
        std::string root_lei;
        std::vector<std::string> parties;
        std::vector<std::string> images;
        std::vector<std::string> datasets;
        std::string tenant_id;
    };

    /// How long a nested publication may take before the step gives up. The
    /// base bundle is the slowest the ACME path published, and it waited 1500s.
    static constexpr std::chrono::seconds step_publish_timeout{1500};

    /**
     * @brief Resolves the administrator a step acts as.
     *
     * The command names the account; its username and its party go into the
     * minted token, because a token's party is what scopes the visible set a
     * request runs under. The provisioner links the administrator to the
     * tenant's system party, so that link is the fallback when no default
     * party is set yet.
     */
    step_actor
    resolve_step_actor(const ores::iam::workflow::provision_tenant_step_command& command) {
        boost::uuids::string_generator parse;
        step_actor actor;
        actor.account_id = parse(command.admin_account_id);

        auto tenant_ctx = tenant_context::with_tenant(ctx_, command.tenant_id);
        ores::iam::service::account_service accounts(tenant_ctx);
        if (const auto account = accounts.get_account(actor.account_id)) {
            actor.username = account->username;
            if (account->default_party_id)
                actor.party_id = *account->default_party_id;
        }
        if (actor.party_id.is_nil()) {
            ores::iam::service::account_party_service links(tenant_ctx);
            const auto parties = links.list_account_parties_by_account(actor.account_id);
            if (!parties.empty())
                actor.party_id = parties.front().party_id;
        }
        if (actor.username.empty())
            actor.username = command.tenant_code;
        return actor;
    }

    /// A client that drives the real request pipeline as the acting account,
    /// re-minting the short-lived token when a wait outlives it.
    internal_request_client make_step_client(const std::string& tenant_id,
                                             const boost::uuids::uuid& account_id,
                                             const boost::uuids::uuid& party_id,
                                             const std::string& username) {
        auto mint = [this, tenant_id, account_id, party_id, username] {
            return impersonation_.mint_token(ctx_, tenant_id, account_id, party_id, username);
        };
        return internal_request_client(nats_, mint(), [mint] { return mint(); });
    }

    /// Dispatches a decoded step command to the action its kind names. Every
    /// kind this build does not execute is refused by name, never half-done.
    void execute_step(const ores::service::messaging::workflow_step_context& wf,
                      const ores::iam::workflow::provision_tenant_step_command& command) {
        switch (classify_step_kind(command.kind)) {
            case provision_step_action::complete_provisioning:
                complete_provisioning_step(wf, command);
                return;
            case provision_step_action::publish_bundle:
                publish_bundle_step(wf, command, resolve_step_actor(command));
                return;
            case provision_step_action::import_lei_hierarchy:
                import_lei_hierarchy_step(wf, command, resolve_step_actor(command));
                return;
            case provision_step_action::provision_party:
                provision_party_step(wf, command, resolve_step_actor(command));
                return;
            case provision_step_action::load_staff:
                load_staff_step(wf, command, resolve_step_actor(command));
                return;
            case provision_step_action::attach_photos:
                attach_photos_step(wf, command, resolve_step_actor(command));
                return;
            case provision_step_action::start_market_feeds:
                start_market_feeds_step(wf, command, resolve_step_actor(command));
                return;
            case provision_step_action::refuse:
                wf.fail("The step kind '" + command.kind +
                        "' is not one this deployment executes.");
                return;
        }
    }

    /// Publishes one bundle, follows its nested run, and throws the reason the
    /// publication was refused or the run did not finish. The label names the
    /// publication in the progress lines, because a step may publish for more
    /// than one party and the lines would otherwise not say which.
    void publish_bundle_or_throw(internal_request_client& client,
                                 const std::string& bundle_code,
                                 const std::string& username,
                                 const std::string& params_json,
                                 const std::string& label) {
        std::string failure;
        auto record = [&failure](std::string step, std::string action, std::uint64_t) {
            if (step.ends_with(".failed"))
                failure = std::move(action);
        };
        auto progress = [&label](const std::string& line) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "provision_step " << label << ": " << line;
        };
        if (!publish_bundle(client,
                            bundle_code,
                            username,
                            params_json,
                            record,
                            progress,
                            step_publish_timeout)) {
            throw std::runtime_error(
                failure.empty() ? "The bundle '" + bundle_code + "' did not publish." : failure);
        }
    }

    /// Publishes each bundle the step names, one nested run each, and reports
    /// only once every run has finished.
    void publish_bundle_step(const ores::service::messaging::workflow_step_context& wf,
                             const ores::iam::workflow::provision_tenant_step_command& command,
                             const step_actor& actor) {
        const auto bundles = parse_step_bundles(command.arguments_json);
        auto client =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);

        dq::messaging::publish_bundle_params params;
        const auto params_json = dq::messaging::build_params_json(params);
        for (const auto& bundle_code : bundles)
            publish_bundle_or_throw(client, bundle_code, actor.username, params_json, command.kind);

        wf.complete(
            rfl::json::write(provision_step_result{.kind = command.kind, .bundles = bundles}));
    }

    /// Publishes the bundles that carry the LEI hierarchy, each with the root
    /// LEI the step's arguments or the run's parameters name.
    ///
    /// A run that names no legal entity imports nothing, and the step says so
    /// as a warning rather than stopping: a starting point may leave the entity
    /// out, and the tenant is then built from the party it creates for itself.
    /// The publication has the same branch for the same reason.
    void
    import_lei_hierarchy_step(const ores::service::messaging::workflow_step_context& wf,
                              const ores::iam::workflow::provision_tenant_step_command& command,
                              const step_actor& actor) {
        const auto arguments =
            parse_lei_hierarchy_arguments(command.arguments_json, command.parameters);
        if (arguments.root_lei.empty()) {
            wf.warn(rfl::json::write(provision_step_result{
                        .kind = command.kind, .bundles = arguments.bundles, .root_lei = ""}),
                    {ores::workflow::messaging::step_log_entry{
                        .level = ores::workflow::messaging::step_log_level::warn,
                        .message =
                            "The starting point names no legal entity, so there was nothing to "
                            "import.",
                        .context = command.tenant_id}});
            return;
        }

        auto client =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);

        dq::messaging::publish_bundle_params params;
        params.lei_parties = dq::messaging::lei_parties_params{.root_lei = arguments.root_lei};
        const auto params_json = dq::messaging::build_params_json(params);
        for (const auto& bundle_code : arguments.bundles)
            publish_bundle_or_throw(client, bundle_code, actor.username, params_json, command.kind);

        wf.complete(rfl::json::write(provision_step_result{
            .kind = command.kind, .bundles = arguments.bundles, .root_lei = arguments.root_lei}));
    }

    /// Publishes each party's bundles, once per party the tenant holds. Each
    /// party's publication runs as that party and is followed to its end, so
    /// the step completes only once every party's data is in place.
    void provision_party_step(const ores::service::messaging::workflow_step_context& wf,
                              const ores::iam::workflow::provision_tenant_step_command& command,
                              const step_actor& actor) {
        const auto bundles = parse_step_bundles(command.arguments_json);

        auto discover =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);

        std::vector<std::string> provisioned;
        // Every party the tenant holds, one page at a time: a single page with
        // a fixed limit would provision the first thousand and report success
        // for the rest.
        std::uint32_t offset = 0;
        constexpr std::uint32_t page_size = 1000;
        while (true) {
            ores::refdata::messaging::list_parties_request request;
            request.offset = offset;
            request.limit = page_size;
            const auto page = discover.request(request).parties;
            for (const auto& party : page) {
                const auto party_id = boost::uuids::to_string(party.id);
                auto client =
                    make_step_client(command.tenant_id, actor.account_id, party.id, actor.username);
                dq::messaging::publish_bundle_params params;
                params.party_id = party_id;
                const auto params_json = dq::messaging::build_params_json(params);
                for (const auto& bundle_code : bundles)
                    publish_bundle_or_throw(
                        client, bundle_code, actor.username, params_json, party_id);
                provisioned.push_back(party_id);
            }
            if (page.size() < page_size)
                break;
            offset += page_size;
        }

        wf.complete(rfl::json::write(provision_step_result{
            .kind = command.kind, .bundles = bundles, .parties = provisioned}));
    }

    /// Resolves each party the step names, publishes the bundles that entry
    /// names against it, and leaves the party ready for the staff those
    /// bundles carry.
    void load_staff_step(const ores::service::messaging::workflow_step_context& wf,
                         const ores::iam::workflow::provision_tenant_step_command& command,
                         const step_actor& actor) {
        const auto assignments = parse_staff_assignments(command.arguments_json);
        auto discover =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);

        std::vector<std::string> parties;
        for (const auto& assignment : assignments) {
            const auto party = find_party(discover, assignment.party_name);
            if (!party)
                throw std::runtime_error("The tenant holds no party named '" +
                                         assignment.party_name + "'.");

            auto client =
                make_step_client(command.tenant_id, actor.account_id, party->id, actor.username);
            dq::messaging::publish_bundle_params params;
            params.party_id = boost::uuids::to_string(party->id);
            const auto params_json = dq::messaging::build_params_json(params);
            for (const auto& bundle_code : assignment.bundles)
                publish_bundle_or_throw(
                    client, bundle_code, actor.username, params_json, assignment.party_name);

            activate_party(client, *party);
            complete_party_onboarding(client, party->id);
            associate_account_with_party(client, actor.account_id, party->id);
            if (assignment.is_default)
                set_account_default_party(client, party->id);
            parties.push_back(boost::uuids::to_string(party->id));
        }

        wf.complete(
            rfl::json::write(provision_step_result{.kind = command.kind, .parties = parties}));
    }

    /// Attaches each named party's staff photos, read from the dataset that
    /// entry names, and the party logo the step names.
    void attach_photos_step(const ores::service::messaging::workflow_step_context& wf,
                            const ores::iam::workflow::provision_tenant_step_command& command,
                            const step_actor& actor) {
        const auto arguments = parse_photo_arguments(command.arguments_json);
        auto discover =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);
        // The images the kind copies are the tenant's, so the direct reads that
        // decide what to copy run under the tenant's context.
        auto tenant_ctx = tenant_context::with_tenant(ctx_, command.tenant_id);

        std::vector<std::string> images;
        std::vector<std::string> parties;
        for (const auto& assignment : arguments.parties) {
            auto party = find_party(discover, assignment.party_name);
            if (!party)
                throw std::runtime_error("The tenant holds no party named '" +
                                         assignment.party_name + "'.");

            auto client =
                make_step_client(command.tenant_id, actor.account_id, party->id, actor.username);
            attach_staff_photos(client, tenant_ctx, command.tenant_id, assignment.dataset, images);

            // The party is read here, after any earlier step that wrote it, so
            // the version the logo's write states is the version the store
            // holds. Giving the logo its own read is what keeps that true
            // whatever order the profile states the kinds in.
            if (!arguments.party_logo.empty() && !party->image_id)
                attach_party_logo(
                    client, tenant_ctx, *party, arguments.party_logo, command.tenant_id, images);
            parties.push_back(boost::uuids::to_string(party->id));
        }

        wf.complete(rfl::json::write(
            provision_step_result{.kind = command.kind, .parties = parties, .images = images}));
    }

    /// Publishes the configuration bundles against the tenant's system party,
    /// binds every party the tenant holds to the theme the step names, and
    /// starts that theme's feeds.
    void start_market_feeds_step(const ores::service::messaging::workflow_step_context& wf,
                                 const ores::iam::workflow::provision_tenant_step_command& command,
                                 const step_actor& actor) {
        const auto arguments = parse_market_feed_arguments(command.arguments_json);
        auto discover =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);

        const auto system_party = find_system_party(discover);
        if (!system_party)
            throw std::runtime_error(
                "The tenant holds no system party to publish its market configuration against.");

        auto system_client =
            make_step_client(command.tenant_id, actor.account_id, system_party->id, actor.username);
        dq::messaging::publish_bundle_params params;
        params.party_id = boost::uuids::to_string(system_party->id);
        const auto params_json = dq::messaging::build_params_json(params);
        for (const auto& bundle_code : arguments.bundles)
            publish_bundle_or_throw(
                system_client, bundle_code, actor.username, params_json, system_party->full_name);

        // Every party the tenant holds consumes the shared stream through its
        // own bindings, which is how the consistent world reaches each party.
        std::vector<std::string> bound;
        std::uint32_t offset = 0;
        constexpr std::uint32_t page_size = 1000;
        while (true) {
            ores::refdata::messaging::list_parties_request request;
            request.offset = offset;
            request.limit = page_size;
            const auto page = discover.request(request).parties;
            for (const auto& party : page) {
                const auto party_id = boost::uuids::to_string(party.id);
                auto party_client =
                    make_step_client(command.tenant_id, actor.account_id, party.id, actor.username);
                if (!create_theme_feed_bindings(
                        system_client, party_client, arguments.theme, party_id))
                    throw std::runtime_error("The theme '" + arguments.theme +
                                             "' was not bound for the party '" + party_id + "'.");
                bound.push_back(party_id);
            }
            if (page.size() < page_size)
                break;
            offset += page_size;
        }

        if (!start_synthetic_theme_feeds(system_client, arguments.theme))
            throw std::runtime_error("The theme '" + arguments.theme + "' feeds were not started.");

        wf.complete(rfl::json::write(provision_step_result{.kind = command.kind,
                                                           .bundles = arguments.bundles,
                                                           .parties = bound,
                                                           .datasets = {arguments.theme}}));
    }

    /// Marks the tenant active and clears bootstrap mode, the two operations
    /// the completing step performs. The tenant is active once it holds its
    /// data, so a flag that will not clear is a warning rather than a failure.
    void
    complete_provisioning_step(const ores::service::messaging::workflow_step_context& wf,
                               const ores::iam::workflow::provision_tenant_step_command& command) {
        const auto actor = resolve_step_actor(command);

        auto sys_ctx = tenant_context::with_system_tenant(ctx_);
        execute_parameterized_command(sys_ctx,
                                      "SELECT ores_iam_mark_tenant_active_fn($1::uuid, $2)",
                                      {command.tenant_id, actor.username},
                                      tenant_provisioning_handler_lg(),
                                      "complete_provisioning_step");
        BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
            << "Tenant marked active: " << command.tenant_id;

        auto client =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);
        const auto result = rfl::json::write(
            provision_step_result{.kind = command.kind, .tenant_id = command.tenant_id});
        const auto warn = [&](const std::string& message) {
            wf.warn(result,
                    {ores::workflow::messaging::step_log_entry{
                        .level = ores::workflow::messaging::step_log_level::warn,
                        .message = message,
                        .context = command.tenant_id}});
        };

        try {
            using namespace ores::variability::messaging;
            const auto resp = client.request(clear_bootstrap_mode_request{});
            if (resp.result.outcome == ores::utility::domain::outcome::ok) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                    << "Bootstrap mode cleared for tenant: " << command.tenant_id;
                wf.complete(result);
                return;
            }
            warn("The bootstrap mode flag was not cleared: " + resp.result.message);
        } catch (const std::exception& e) {
            warn(std::string("The bootstrap mode flag was not cleared: ") + e.what());
        }
    }

    static std::string lei_import_params() {
        dq::messaging::publish_bundle_params params;
        params.lei_parties = dq::messaging::lei_parties_params{.root_lei = "9695ACMEGROUP0000030"};
        return dq::messaging::build_params_json(params);
    }

    // Publishes one bundle and waits for it to complete, reporting
    // human-readable step progress via add_step/on_progress rather than raw
    // internal step/action codes. Returns false (already recorded via
    // add_step) on a dispatch failure or an incomplete/failed workflow.
    static bool
    publish_bundle(internal_request_client& client,
                   const std::string& bundle_code,
                   const std::string& username,
                   const std::string& params_json,
                   const std::function<void(std::string, std::string, std::uint64_t)>& add_step,
                   const std::function<void(const std::string&)>& on_progress,
                   std::chrono::seconds timeout = std::chrono::seconds{120}) {
        dq::messaging::publish_bundle_request req;
        req.bundle_code = bundle_code;
        req.published_by = username;
        req.atomic = true;
        req.params_json = params_json;

        auto pub = client.request(req);
        if (!pub.success) {
            add_step(bundle_code + ".failed", pub.error_message, 0);
            return false;
        }
        add_step(bundle_code, "dispatched", static_cast<std::uint64_t>(pub.datasets_dispatched));

        auto result =
            client.wait_for_workflow_instance(pub.instance_id,
                                              timeout,
                                              static_cast<std::size_t>(pub.datasets_dispatched),
                                              on_progress);
        if (!result.success) {
            // The wait result carries the exact reason (failed step error,
            // instance-level start failure, or the timeout detail) so a
            // failed provisioning reports what actually went wrong instead
            // of the generic "workflow did not complete".
            add_step(bundle_code + ".failed", result.error, 0);
            return false;
        }
        return true;
    }

    // Finds a party by full_name via the real refdata.v1.parties.list
    // request (paginated up to 1000, matching accounts_commands.cpp's
    // process_set_default_party -- there is no server-side filter-by-name).
    static std::optional<ores::refdata::domain::party> find_party(internal_request_client& client,
                                                                  const std::string& full_name) {
        ores::refdata::messaging::list_parties_request req;
        req.limit = 1000;
        auto resp = client.request(req);
        for (auto& p : resp.parties)
            if (p.full_name == full_name)
                return p;
        return std::nullopt;
    }

    // Starts every feed under the calling party's (client's) theme
    // collection folder for the given dq dataset code (e.g.
    // "synthetic.themes.realistic_2026") via one folder-scoped request
    // the server cascades across asset classes, resolved server-side from
    // synthetic_publish_from_dq's container-per-(tenant, party, dataset)
    // convention rather than by matching on display name. Best-effort: logs
    // and returns false on any resolution miss (dataset/config/folder not
    // found, or a list request itself failing server-side) rather than
    // failing provisioning over a cosmetic follow-on step -- the party
    // itself is already fully provisioned by this point. Returns whether
    // resolution succeeded far enough to attempt starting feeds, so the
    // caller can surface a miss in its own step list rather than reporting
    // "completed" for a no-op.
    static bool start_synthetic_theme_feeds(internal_request_client& client,
                                            const std::string& dataset_code) {
        std::optional<boost::uuids::uuid> dataset_id;
        {
            dq::messaging::list_datasets_request req;
            req.limit = 1000;
            auto resp = client.request(req);
            if (resp.result.outcome != ores::utility::domain::outcome::ok) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                    << "start_synthetic_theme_feeds: list datasets failed: " << resp.result.message;
                return false;
            }
            for (auto& d : resp.datasets)
                if (d.code == dataset_code) {
                    dataset_id = d.id;
                    break;
                }
        }
        if (!dataset_id) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                << "start_synthetic_theme_feeds: dataset not found: " << dataset_code;
            return false;
        }

        std::optional<boost::uuids::uuid> config_id;
        {
            synthetic::messaging::list_market_data_generation_configs_request req;
            req.limit = 1000;
            auto resp = client.request(req);
            if (resp.result.outcome != ores::utility::domain::outcome::ok) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                    << "start_synthetic_theme_feeds: list market_data_generation_configs "
                       "failed: "
                    << resp.result.message;
                return false;
            }
            for (auto& c : resp.market_data_generation_configs)
                if (c.dataset_id == dataset_id) {
                    config_id = c.id;
                    break;
                }
        }
        if (!config_id) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                << "start_synthetic_theme_feeds: no market_data_generation_config for dataset "
                << dataset_code;
            return false;
        }

        std::optional<boost::uuids::uuid> folder_id;
        {
            synthetic::messaging::list_folders_request req;
            req.limit = 1000;
            auto resp = client.request(req);
            if (resp.result.outcome != ores::utility::domain::outcome::ok) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                    << "start_synthetic_theme_feeds: list folders failed: " << resp.result.message;
                return false;
            }
            for (auto& f : resp.folders)
                if (f.kind == "collection" && f.collection_id == config_id) {
                    folder_id = f.id;
                    break;
                }
        }
        if (!folder_id) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                << "start_synthetic_theme_feeds: no collection folder for dataset " << dataset_code;
            return false;
        }
        const auto folder_id_str = boost::uuids::to_string(*folder_id);

        // One folder-scoped request cascades every feed under the subtree
        // server-side, across all asset classes.
        marketdata::messaging::start_feeds_under_folder_request req;
        req.folder_id = folder_id_str;
        auto resp = client.request(req);
        if (!resp.success) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                << "start_synthetic_theme_feeds: start folder failed: " << resp.message;
            return false;
        }
        return true;
    }

    // Finds the tenant's system party (party_category == "System", created
    // once per tenant by the IAM provisioner) -- the owner of the simulated
    // market config in the consistent world.
    static std::optional<ores::refdata::domain::party>
    find_system_party(internal_request_client& client) {
        ores::refdata::messaging::list_parties_request req;
        req.limit = 1000;
        auto resp = client.request(req);
        for (auto& p : resp.parties)
            if (p.party_category == "System")
                return p;
        return std::nullopt;
    }

    // Creates one feed binding per enabled FX source of the dq dataset
    // (theme) resolved by @p dataset_code, for @p party_id_str -- the
    // consumption contract of the consistent world (see the
    // simulated-market-data strategy): the theme's config lives once under
    // the system party, every party consumes the shared stream through its
    // own bindings, and the marketdata ingest loop materializes each
    // party's observations from it. Two clients are needed: the synthetic
    // tables are party-isolated by RLS and the theme's configs live under
    // the system party, so resolution runs through @p resolve_client (the
    // system party's); the bindings themselves are saved through
    // @p save_client impersonating the consuming party, because
    // feed-binding saves stamp the binding's party from the authenticated
    // context (a security boundary). Sources already bound for the party
    // are skipped, so the step is re-runnable: a party that already holds
    // every source returns true without writing, and a party that holds
    // some of them writes only the rest. Workspace defaults to the
    // Live sentinel. IR sources are deliberately not bound: IR producers
    // publish on synthetic.v1.curve_family.<source>, a subject the ingest
    // loop never listens to -- binding them would claim ingestion for a
    // stream that never arrives. Best-effort, like
    // start_synthetic_theme_feeds: logs and returns false on any
    // resolution miss rather than failing provisioning; individual save
    // failures are logged and skipped so one bad source does not drop the
    // remaining bindings.
    static bool create_theme_feed_bindings(internal_request_client& resolve_client,
                                           internal_request_client& save_client,
                                           const std::string& dataset_code,
                                           const std::string& party_id_str) {
        std::optional<boost::uuids::uuid> dataset_id;
        {
            dq::messaging::list_datasets_request req;
            req.limit = 1000;
            auto resp = resolve_client.request(req);
            if (resp.result.outcome != ores::utility::domain::outcome::ok) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                    << "create_theme_feed_bindings: list datasets failed: " << resp.result.message;
                return false;
            }
            for (auto& d : resp.datasets)
                if (d.code == dataset_code) {
                    dataset_id = d.id;
                    break;
                }
        }
        if (!dataset_id) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                << "create_theme_feed_bindings: dataset not found: " << dataset_code;
            return false;
        }

        std::optional<boost::uuids::uuid> config_id;
        {
            synthetic::messaging::list_market_data_generation_configs_request req;
            req.limit = 1000;
            auto resp = resolve_client.request(req);
            if (resp.result.outcome != ores::utility::domain::outcome::ok) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                    << "create_theme_feed_bindings: list market_data_generation_configs "
                       "failed: "
                    << resp.result.message;
                return false;
            }
            for (auto& c : resp.market_data_generation_configs)
                if (c.dataset_id == dataset_id) {
                    config_id = c.id;
                    break;
                }
        }
        if (!config_id) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                << "create_theme_feed_bindings: no market_data_generation_config for dataset "
                << dataset_code;
            return false;
        }

        // Each pair is (source_name, ore_key).
        std::vector<std::pair<std::string, std::string>> sources;
        {
            synthetic::messaging::list_fx_spot_generation_configs_request req;
            req.limit = 1000;
            auto resp = resolve_client.request(req);
            if (resp.result.outcome != ores::utility::domain::outcome::ok) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                    << "create_theme_feed_bindings: list fx_spot_generation_configs failed: "
                    << resp.result.message;
                return false;
            }
            for (auto& c : resp.fx_spot_generation_configs)
                if (c.enabled && c.config_id == config_id)
                    sources.emplace_back(c.source_name, c.ore_key);
        }

        // Active bindings already exist per natural key (tenant, party,
        // ore_key, source_name); skip them so re-provisioning does not trip
        // the unique index. The list is tenant-scoped, so filter down to
        // this party's rows.
        std::vector<std::string> existing;
        {
            marketdata::messaging::list_feed_bindings_request req;
            req.limit = 1000;
            auto resp = save_client.request(req);
            if (resp.result.outcome != ores::utility::domain::outcome::ok) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                    << "create_theme_feed_bindings: list feed_bindings failed: "
                    << resp.result.message;
                return false;
            }
            for (const auto& b : resp.feed_bindings)
                if (boost::uuids::to_string(b.party_id) == party_id_str)
                    existing.push_back(b.ore_key + "|" + b.source_name);
        }

        boost::uuids::random_generator uuid_gen;
        boost::uuids::string_generator sg;
        bool all_saved = true;
        for (const auto& [source_name, ore_key] : sources) {
            if (std::find(existing.begin(), existing.end(), ore_key + "|" + source_name) !=
                existing.end()) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                    << "create_theme_feed_bindings: binding " << source_name << " for party "
                    << party_id_str << " already exists; skipping";
                continue;
            }
            marketdata::messaging::put_feed_binding_request req;
            req.change.write.id = uuid_gen();
            req.change.write.ore_key = ore_key;
            req.change.write.source_name = source_name;
            req.change.write.asset_class = "fx";
            req.change.write.enabled = true;
            req.change.write.party_id = sg(party_id_str);
            // The write states no expectation, because the store's
            // must-not-exist claim is keyed on the ORE key alone while the
            // binding's natural key is the party with the ORE key and the
            // source. A must-not-exist claim therefore refuses the second
            // party's binding for a source the first party already holds.
            // The freshness check above is what keeps this idempotent, and
            // the natural key's unique index
            // (feed_bindings_party_id_ore_key_source_name_uniq_idx) is what
            // keeps it honest.
            req.change.precondition.kind = ores::utility::domain::precondition_kind::any;
            req.intent.reason_code = "system.new_record";
            req.intent.commentary =
                "Created by ACME provisioning: consumes the system-party simulated market "
                "stream";
            auto resp = save_client.request(req);
            if (resp.result.outcome != ores::utility::domain::outcome::ok) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                    << "create_theme_feed_bindings: save binding " << source_name << " for party "
                    << party_id_str << " failed: " << resp.result.message;
                all_saved = false;
            }
        }
        if (sources.empty())
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "create_theme_feed_bindings: no enabled FX sources for dataset " << dataset_code;
        return all_saved;
    }

    // Copies the system-tenant "key" template image into p_tenant_id (if not
    // already copied) via the real assets.v1.images.save path, returning
    // the per-tenant image_id. The template read itself stays a direct SQL
    // read (not a NATS round-trip): unlike the write-side activation logic
    // this rework replaces, a read has no versioning/validation behaviour
    // to duplicate incorrectly, and reading the system tenant's own data
    // needs no impersonation of an unrelated identity.
    std::optional<boost::uuids::uuid> copy_template_image(internal_request_client& client,
                                                          ores::database::context& ctx,
                                                          const std::string& tenant_id,
                                                          const std::string& key) {
        auto existing = execute_parameterized_string_query(
            ctx,
            "SELECT id::text FROM ores_assets_images_tbl WHERE tenant_id = $1::uuid AND "
            "code = $2 AND valid_to = ores_utility_infinity_timestamp_fn()",
            {tenant_id, key},
            tenant_provisioning_handler_lg(),
            "copy_template_image");
        if (!existing.empty())
            return boost::uuids::string_generator{}(existing.front());

        auto rows = execute_parameterized_multi_column_query(
            ctx,
            "SELECT description, mime_type, data FROM ores_assets_get_template_image_fn($1)",
            {key},
            tenant_provisioning_handler_lg(),
            "copy_template_image");
        if (rows.empty() || rows.front().size() < 3 || !rows.front()[0] || !rows.front()[1] ||
            !rows.front()[2]) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                << "No system-tenant template image: " << key;
            return std::nullopt;
        }

        // A write carries the user-owned fields alone. The tenant, the actor
        // and the provenance are the assets service's to derive from the
        // authenticated context, and the version is the database's.
        ores::assets::messaging::put_image_request req;
        const auto new_image_id = boost::uuids::random_generator{}();
        req.change.write.id = new_image_id;
        req.change.write.code = key;
        req.change.write.description = *rows.front()[0];
        req.change.write.mime_type = *rows.front()[1];
        // The template function returns the column as stored: base64 text.
        // The write record carries raw bytes, so decode that hop here.
        req.change.write.data = ores::utility::convert::base64_converter::convert(*rows.front()[2]);
        req.change.precondition.kind = ores::utility::domain::precondition_kind::must_not_exist;
        req.intent.reason_code = "system.external_data_import";
        req.intent.commentary = "Copied from system-tenant template: " + key;

        auto resp = client.request(req);
        if (resp.result.outcome != ores::utility::domain::outcome::ok) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                << "Failed to copy template image " << key << ": " << resp.result.message;
            return std::nullopt;
        }
        return new_image_id;
    }

    /// The legacy synchronous handler treats a staff photo as cosmetic: it
    /// never read the outcome of the work, so a missing dataset or a template
    /// that would not copy was skipped rather than reported. A step kind
    /// reports it instead, because it has an instance to report to. This
    /// wrapper is where that difference lives until the handler retires.
    void attach_staff_photos_best_effort(internal_request_client& client,
                                         ores::database::context& ctx,
                                         const std::string& tenant_id,
                                         const std::string& dataset_code) {
        try {
            std::vector<std::string> images;
            attach_staff_photos(client, ctx, tenant_id, dataset_code, images);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                << "attach_staff_photos did not complete for " << dataset_code << ": " << e.what();
        }
    }

    // Attaches a profile picture to every staff account the named dataset
    // carries a photo_key for and that doesn't already have an image_id. Reads
    // (username, photo_key) directly from the DQ artefact table (not modeled in
    // the account NATS API, same reason grant_cross_entity_access's
    // account_ids_for does the same for business_unit_code/role), then copies
    // each account's own template image and re-saves the account with every
    // other field echoed back unchanged (update_account_request has no
    // partial-update semantics -- omitting a field would clear it).
    void attach_staff_photos(internal_request_client& client,
                             ores::database::context& ctx,
                             const std::string& tenant_id,
                             const std::string& dataset_code,
                             std::vector<std::string>& images) {
        auto dataset = execute_parameterized_string_query(
            ctx,
            "SELECT id::text FROM ores_dq_datasets_tbl WHERE code = $1 "
            "AND valid_to = ores_utility_infinity_timestamp_fn()",
            {dataset_code},
            tenant_provisioning_handler_lg(),
            "attach_staff_photos");
        if (dataset.empty())
            throw std::runtime_error("The dataset '" + dataset_code +
                                     "' does not exist, so its staff photos cannot be attached.");

        auto rows = execute_parameterized_multi_column_query(
            ctx,
            "SELECT username, photo_key FROM ores_dq_accounts_artefact_tbl "
            "WHERE dataset_id = $1::uuid AND photo_key IS NOT NULL",
            {dataset.front()},
            tenant_provisioning_handler_lg(),
            "attach_staff_photos");
        if (rows.empty())
            return;

        std::unordered_map<std::string, std::string> photo_key_by_username;
        for (const auto& row : rows) {
            if (row.size() < 2 || !row[0] || !row[1])
                continue;
            photo_key_by_username[*row[0]] = *row[1];
        }

        iam::messaging::list_accounts_request accounts_req;
        accounts_req.limit = 10'000;
        auto accounts_resp = client.request(accounts_req);
        for (const auto& a : accounts_resp.accounts) {
            const auto it = photo_key_by_username.find(a.username);
            if (it == photo_key_by_username.end() || a.image_id.has_value())
                continue;

            auto image_id = copy_template_image(client, ctx, tenant_id, it->second);
            if (!image_id)
                throw std::runtime_error("The template image '" + it->second +
                                         "' was not copied into the tenant.");

            iam::messaging::update_account_request update_req;
            update_req.account_id = boost::uuids::to_string(a.id);
            update_req.email = a.email;
            update_req.full_name = a.full_name;
            update_req.default_party_id =
                a.default_party_id ? boost::uuids::to_string(*a.default_party_id) : "";
            update_req.job_title = a.job_title;
            update_req.reports_to_account_id =
                a.reports_to_account_id ? boost::uuids::to_string(*a.reports_to_account_id) : "";
            update_req.image_id = boost::uuids::to_string(*image_id);
            update_req.change_reason_code = "system.external_data_import";
            update_req.change_commentary = "Attached staff photo during provisioning";
            const auto resp = client.request(update_req);
            if (!resp.success)
                throw std::runtime_error("The photo was not attached to the account '" +
                                         a.username + "': " + resp.message);
            images.push_back(boost::uuids::to_string(*image_id));
        }
    }

    /// Copies the named template image into the tenant and saves it on the
    /// party, which the caller read first so the write states its version.
    void attach_party_logo(internal_request_client& client,
                           ores::database::context& ctx,
                           const ores::refdata::domain::party& party,
                           const std::string& template_key,
                           const std::string& tenant_id,
                           std::vector<std::string>& images) {
        const auto image_id = copy_template_image(client, ctx, tenant_id, template_key);
        if (!image_id)
            throw std::runtime_error("The template image '" + template_key +
                                     "' was not copied into the tenant.");
        save_party(client,
                   party,
                   party.status,
                   *image_id,
                   "Attached the party logo while provisioning the tenant");
        images.push_back(boost::uuids::to_string(*image_id));
    }

    /// Saves a party with the state the caller decided. The caller read the
    /// party first, so the write states the version it read and the store
    /// refuses a row that moved on.
    static void save_party(internal_request_client& client,
                           const ores::refdata::domain::party& party,
                           const std::string& status,
                           const std::optional<boost::uuids::uuid>& image_id,
                           const std::string& commentary) {
        ores::refdata::messaging::put_party_request save_req;
        save_req.change.write = {.id = party.id,
                                 .short_code = party.short_code,
                                 .full_name = party.full_name,
                                 .codename = party.codename,
                                 .transliterated_name = party.transliterated_name,
                                 .party_category = party.party_category,
                                 .party_type = party.party_type,
                                 .parent_party_id = party.parent_party_id,
                                 .business_center_code = party.business_center_code,
                                 .status = status,
                                 .image_id = image_id};
        save_req.change.precondition.kind =
            ores::utility::domain::precondition_kind::must_match_version;
        save_req.change.precondition.version = static_cast<std::uint32_t>(party.version);
        save_req.intent.reason_code = "system.external_data_import";
        save_req.intent.commentary = commentary;

        const auto resp = client.request(save_req);
        if (resp.result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("The party '" + party.full_name +
                                     "' was not saved: " + resp.result.message);
    }

    /// Saves the party active when the import left it inactive.
    static void activate_party(internal_request_client& client,
                               const ores::refdata::domain::party& party) {
        if (party.status == "Active")
            return;
        save_party(client, party, "Active", party.image_id, "Activated during provisioning");
    }

    /// Marks the party's onboarding wizard complete, which is what makes the
    /// party usable rather than a half-set-up one.
    static void complete_party_onboarding(internal_request_client& client,
                                          const boost::uuids::uuid& party_id) {
        ores::variability::messaging::complete_party_onboarding_request req;
        req.party_id = party_id;
        const auto resp = client.request(req);
        if (resp.result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("The party's onboarding was not completed: " +
                                     resp.result.message);
    }

    /// Associates an account with a party, which is what lets that account work
    /// in the party.
    static void associate_account_with_party(internal_request_client& client,
                                             const boost::uuids::uuid& account_id,
                                             const boost::uuids::uuid& party_id) {
        ores::iam::messaging::put_many_account_parties_request req;
        ores::iam::messaging::account_party_change change;
        change.write.account_id = account_id;
        change.write.party_id = party_id;
        change.precondition.kind = ores::utility::domain::precondition_kind::any;
        req.changes.push_back(std::move(change));
        req.intent.reason_code = "system.external_data_import";
        req.intent.commentary = "Associated during provisioning";

        const auto resp = client.request(req);
        if (resp.result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("The administrator was not associated with the party: " +
                                     resp.result.message);
    }

    /// Sets the acting account's default party.
    static void set_account_default_party(internal_request_client& client,
                                          const boost::uuids::uuid& party_id) {
        ores::iam::messaging::set_my_default_party_request req;
        req.party_id = boost::uuids::to_string(party_id);
        const auto resp = client.request(req);
        if (!resp.success)
            throw std::runtime_error("The party was not set as the acting account's default: " +
                                     resp.message);
    }

    // Activates the party (if Inactive), attaches its logo (if unset),
    // marks its onboarding wizard complete, and associates the tenant
    // admin with it -- the same effects the generic shell/wizard
    // "provision party" flow's final phase produces, driven here via the
    // real save_party/complete_party_onboarding/save_account_party
    // requests instead of hand-written SQL. Best-effort, because the
    // synchronous Acme handler this serves never read these responses;
    // the step kinds report their failures instead.
    void finish_party(internal_request_client& client,
                      ores::database::context& ctx,
                      const std::string& tenant_id,
                      const boost::uuids::uuid& account_id,
                      [[maybe_unused]] const std::string& username,
                      ores::refdata::domain::party party,
                      bool set_default) {
        try {
            const auto status = party.status == "Active" ? party.status : std::string("Active");
            auto image_id = party.image_id;
            if (!image_id)
                image_id = copy_template_image(client, ctx, tenant_id, "acme_party_logo");
            if (status != party.status || image_id != party.image_id)
                save_party(client,
                           party,
                           status,
                           image_id,
                           "Activated (and logo attached) during Acme provisioning");

            complete_party_onboarding(client, party.id);
            associate_account_with_party(client, account_id, party.id);
            if (set_default)
                set_account_default_party(client, party.id);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                << "finish_party did not complete for " << party.full_name << ": " << e.what();
        }
    }


    // Grants the narrow set of cross-entity ores_iam_account_parties_tbl
    // associations that simulate a real multi-entity bank's follow-the-sun
    // model: Desk Heads on the two genuinely global 24h books (IR Swaps,
    // FX Rates) get remote-booking membership on London (the "global
    // book" entity) if they aren't already there, and every office's
    // Market Risk staff get risk-oversight membership on every other
    // office's party. Reads role/business_unit_code straight from the DQ
    // accounts artefact tables already published by the office loop above
    // (a read has no versioning/validation behaviour to duplicate
    // incorrectly, same rationale as copy_template_image's direct read) --
    // ores_iam_accounts_tbl itself carries no business-unit reference to
    // query this from post-publish.
    void grant_cross_entity_access(
        ores::database::context& ctx,
        const std::string& tenant_id,
        const std::vector<std::pair<std::string, boost::uuids::uuid>>& office_parties) {
        struct office_info {
            std::string code;
            std::string party_id;
        };
        std::vector<office_info> offices;
        offices.reserve(office_parties.size());
        for (const auto& [code, party_id] : office_parties)
            offices.push_back({code, boost::uuids::to_string(party_id)});

        const auto london = std::ranges::find(offices, "acme_uk", &office_info::code);
        if (london == offices.end())
            return;

        auto account_ids_for = [&](const std::string& office_code,
                                   const std::string& business_unit_code,
                                   const std::optional<std::string>& role) {
            auto dataset = execute_parameterized_string_query(
                ctx,
                "SELECT id::text FROM ores_dq_datasets_tbl WHERE code = $1 "
                "AND valid_to = ores_utility_infinity_timestamp_fn()",
                {"acme." + office_code + ".accounts"},
                tenant_provisioning_handler_lg(),
                "grant_cross_entity_access");
            if (dataset.empty())
                return std::vector<std::string>{};
            auto usernames =
                role ? execute_parameterized_string_query(
                           ctx,
                           "SELECT username FROM ores_dq_accounts_artefact_tbl WHERE dataset_id = "
                           "$1::uuid AND business_unit_code = $2 AND role = $3",
                           {dataset.front(), business_unit_code, *role},
                           tenant_provisioning_handler_lg(),
                           "grant_cross_entity_access") :
                       execute_parameterized_string_query(
                           ctx,
                           "SELECT username FROM ores_dq_accounts_artefact_tbl WHERE dataset_id = "
                           "$1::uuid AND business_unit_code = $2",
                           {dataset.front(), business_unit_code},
                           tenant_provisioning_handler_lg(),
                           "grant_cross_entity_access");
            std::vector<std::string> account_ids;
            for (const auto& u : usernames) {
                auto acct = execute_parameterized_string_query(
                    ctx,
                    "SELECT id::text FROM ores_iam_accounts_tbl WHERE tenant_id = $1::uuid "
                    "AND username = $2 AND valid_to = ores_utility_infinity_timestamp_fn()",
                    {tenant_id, u},
                    tenant_provisioning_handler_lg(),
                    "grant_cross_entity_access");
                if (!acct.empty())
                    account_ids.push_back(acct.front());
            }
            return account_ids;
        };

        auto grant = [&](const std::string& account_id, const std::string& party_id) {
            execute_parameterized_command(
                ctx,
                "INSERT INTO ores_iam_account_parties_tbl (account_id, tenant_id, party_id, "
                "version, modified_by, performed_by, change_reason_code, change_commentary) "
                "SELECT $1::uuid, $2::uuid, $3::uuid, 0, "
                "coalesce(ores_iam_current_service_fn(), current_user), current_user, "
                "'system.external_data_import', "
                "'Cross-entity access granted during Acme provisioning' "
                "WHERE NOT EXISTS (SELECT 1 FROM ores_iam_account_parties_tbl WHERE "
                "tenant_id = $2::uuid AND account_id = $1::uuid AND party_id = $3::uuid AND "
                "valid_to = ores_utility_infinity_timestamp_fn())",
                {account_id, tenant_id, party_id},
                tenant_provisioning_handler_lg(),
                "grant_cross_entity_access");
        };

        for (const auto& office : offices) {
            if (office.code == "acme_uk")
                continue;
            for (const auto* desk : {"ir_swaps", "fx_rates"})
                for (const auto& account_id :
                     account_ids_for(office.code, office.code + "." + desk, "Desk Head"))
                    grant(account_id, london->party_id);
        }

        for (const auto& office : offices) {
            const auto risk_staff =
                account_ids_for(office.code, office.code + ".market_risk", std::nullopt);
            for (const auto& other : offices) {
                if (other.code == office.code)
                    continue;
                for (const auto& account_id : risk_staff)
                    grant(account_id, other.party_id);
            }
        }
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
    ores::iam::service::internal_impersonation_service impersonation_;
};

} // namespace ores::iam::messaging
#endif
