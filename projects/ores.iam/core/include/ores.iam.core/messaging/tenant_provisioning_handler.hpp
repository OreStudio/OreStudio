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

#include "ores.assets.api/messaging/image_operations_protocol.hpp"
#include "ores.assets.api/messaging/image_protocol.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.dq.api/messaging/dataset_protocol.hpp"
#include "ores.dq.api/messaging/party_provisioning_plan.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.dq.api/messaging/publish_params.hpp"
#include "ores.iam.api/domain/role_codes.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
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
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/party_identifier_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.api/messaging/folder_protocol.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include "ores.variability.api/messaging/operations_protocol.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
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
            // What the run acts on, so the tenant it creates can be followed
            // from the tenant list after whoever asked for it has left the
            // screen the run was started from.
            start.target_kind = std::string(ores::iam::workflow::provision_tenant_target_kind);
            start.target_id = created.tenant_id;
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
     * @brief Provisions one party of the caller's own tenant.
     *
     * The party stage as one request. The starting point's row states the
     * bundles a party is published from, so the request names a starting point
     * and a party, and the run it starts publishes them, activates the party,
     * marks its onboarding complete and associates the caller with it.
     *
     * Everything that can refuse does so before the run exists: a profile code
     * no row answers, a starting point that orders no party step, and a party
     * the tenant does not hold each leave no run behind, because a run that
     * cannot do what it was asked is worse than a refusal that says why.
     */
    void provision_party(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(tenant_provisioning_handler_lg(), msg);

        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        // Starting a party's provisioning run is a party-level capability and
        // not a tenant-level one: a tenant administrator runs it in the tenant
        // they already work in, and reaching for the verb that creates tenants
        // would have made every caller who may provision a party into a caller
        // who may create a tenant.
        if (!has_permission(*ctx_expected, "iam::parties:provision")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto req = decode<provision_party_command>(msg);
        if (!req) {
            reply(nats_,
                  msg,
                  provision_party_command_response{.success = false,
                                                   .message = "Invalid request payload."});
            return;
        }

        const auto refuse = [&](const std::string& reason) {
            reply(
                nats_, msg, provision_party_command_response{.success = false, .message = reason});
        };

        try {
            if (req->party.empty()) {
                refuse("The field 'party' has no value.");
                return;
            }

            const auto tenant_id = ctx_expected->tenant_id().to_string();

            // The engine sends a step without the caller's token, so the run
            // names the account its steps act as. The request context names the
            // caller in words -- its name, its tenant and its party -- and no
            // account identifier, so the identifier comes from the token's
            // subject, which is the session's account.
            const auto claims = signer_.validate(ores::service::messaging::bearer_token(msg));
            if (!claims) {
                refuse("The authorization token could not be read.");
                return;
            }

            // A profile is system-owned registered data, and the SQL provisioner
            // refuses to run outside the system tenant anyway, so the read and
            // the write share one context.
            auto sys_ctx = tenant_context::with_system_tenant(ctx_);
            ores::iam::service::seed_profile_service profile_svc(sys_ctx);
            ores::iam::service::seed_profile_step_service step_svc(sys_ctx);

            const auto declared_steps_of = [&](const ores::iam::domain::seed_profile& candidate) {
                return step_svc.list_seed_profile_steps_by_seed_profile_id(
                    boost::uuids::to_string(candidate.id), 0, 1000);
            };
            const auto party_step_of =
                [](const std::vector<ores::iam::domain::seed_profile_step>& declared) {
                    return std::find_if(
                        declared.begin(), declared.end(), [](const auto& candidate) {
                            return candidate.step_kind ==
                                   ores::iam::workflow::provision_party_step_kind;
                        });
                };

            // The party stage is the starting point's, so the row that orders it
            // is what says which bundles a party is published from. A starting
            // point that orders no party step has no party stage to run, and
            // saying so is better than running a run whose one step cannot read
            // its own arguments.
            //
            // A caller that names no starting point gets the deployment's own:
            // the first one, in the order the deployment lists them, that
            // orders a party step. A tenant administrator cannot name one --
            // the profiles are the system tenant's rows and a tenant reads only
            // its own -- so choosing here is what lets that caller add a party
            // at all.
            const auto profiles = profile_svc.list_seed_profiles(0, 1000);

            std::optional<ores::iam::domain::seed_profile> profile;
            std::optional<ores::iam::domain::seed_profile_step> party_step;
            if (!req->profile_code.empty()) {
                const auto named =
                    std::find_if(profiles.begin(), profiles.end(), [&](const auto& candidate) {
                        return candidate.code == req->profile_code;
                    });
                if (named == profiles.end()) {
                    refuse("The seed profile '" + req->profile_code + "' does not exist.");
                    return;
                }
                const auto declared = declared_steps_of(*named);
                const auto declared_party_step = party_step_of(declared);
                if (declared_party_step == declared.end()) {
                    refuse("The seed profile '" + named->code +
                           "' orders no party step, so it has no party stage to run.");
                    return;
                }
                profile = *named;
                party_step = *declared_party_step;
            } else {
                auto ordered = profiles;
                std::sort(ordered.begin(), ordered.end(), [](const auto& left, const auto& right) {
                    return left.display_order == right.display_order ?
                               left.code < right.code :
                               left.display_order < right.display_order;
                });
                for (const auto& candidate : ordered) {
                    const auto declared = declared_steps_of(candidate);
                    const auto declared_party_step = party_step_of(declared);
                    if (declared_party_step != declared.end()) {
                        profile = candidate;
                        party_step = *declared_party_step;
                        break;
                    }
                }
                if (!profile || !party_step) {
                    refuse("This deployment orders no party step in any of its seed "
                           "profiles, so it has no party stage to run.");
                    return;
                }
            }

            // The tenant every step acts on is the caller's own: a party is a
            // tenant's, and the request reaches the party through the tenant the
            // session is already in.
            // The party the request names: its identifier when it is one, and
            // its exact full name when it is not. The reference is parsed here
            // rather than compared as text, so both spellings of one identifier
            // reach the same row, which is what the step's own lookup does.
            auto tenant_ctx = tenant_context::with_tenant(ctx_, tenant_id);
            std::optional<boost::uuids::uuid> wanted_id;
            try {
                wanted_id = boost::lexical_cast<boost::uuids::uuid>(req->party);
            } catch (const boost::bad_lexical_cast&) {
            }
            const auto found =
                wanted_id ?
                    execute_parameterized_string_query(
                        tenant_ctx,
                        "SELECT id::text FROM ores_refdata_parties_tbl WHERE tenant_id = $1::uuid "
                        "AND valid_to = ores_utility_infinity_timestamp_fn() AND id = $2::uuid",
                        {tenant_id, boost::uuids::to_string(*wanted_id)},
                        tenant_provisioning_handler_lg(),
                        "provision_party") :
                    execute_parameterized_string_query(
                        tenant_ctx,
                        "SELECT id::text FROM ores_refdata_parties_tbl WHERE tenant_id = $1::uuid "
                        "AND valid_to = ores_utility_infinity_timestamp_fn() "
                        "AND full_name = $2",
                        {tenant_id, req->party},
                        tenant_provisioning_handler_lg(),
                        "provision_party");
            if (found.empty()) {
                refuse("The tenant holds no party named '" + req->party + "'.");
                return;
            }
            // A legal name is not unique in the register, so two of a tenant's
            // parties can share one. A run acts on one party, so a reference
            // that names two is refused rather than guessed at.
            if (found.size() > 1) {
                refuse("The tenant holds more than one party named '" + req->party +
                       "'; name it by its identifier.");
                return;
            }

            // The run's record says what it acted on, so it carries the tenant's
            // own code and hostname beside the identifier.
            auto code = tenant_id;
            auto hostname = std::string{};
            const auto tenant_row = execute_parameterized_multi_column_query(
                sys_ctx,
                "SELECT code, hostname FROM ores_iam_tenants_tbl WHERE id = $1::uuid "
                "AND valid_to = ores_utility_infinity_timestamp_fn()",
                {tenant_id},
                tenant_provisioning_handler_lg(),
                "provision_party");
            if (!tenant_row.empty() && tenant_row.front().size() >= 2 && tenant_row.front()[0] &&
                tenant_row.front()[1]) {
                code = *tenant_row.front()[0];
                hostname = *tenant_row.front()[1];
            }

            ores::iam::workflow::provision_tenant_workflow_request run;
            run.profile_code = profile->code;
            run.tenant_id = tenant_id;
            run.tenant_code = code;
            run.tenant_hostname = hostname;
            run.admin_account_id = claims->subject;

            // The party is the step's own argument: a kind reads what it acts on
            // from its arguments, and the row that orders the kind is the one
            // that says which bundles it acts with. The identifier the refusal
            // above resolved travels in the argument, so the step acts on the
            // party this answer names rather than resolving the reference a
            // second time and possibly reaching another row.
            auto arguments = detail::read_step_arguments(party_step->arguments_json);
            arguments["party"] = found.front();
            /*
             * The entity the person chose travels with the step, and the step
             * records it against the party: a party identifier carries the
             * party its writing session acts in, and this caller works in
             * another one, so the run is the only client that can write it.
             */
            if (!req->lei.empty())
                arguments["lei"] = req->lei;
            run.steps.push_back({party_step->step_kind, rfl::json::write(arguments)});

            boost::uuids::random_generator generate;
            const auto instance_id = boost::uuids::to_string(generate());

            ores::workflow::messaging::start_workflow_message start;
            start.type = std::string(ores::iam::workflow::provision_party_workflow_type);
            start.tenant_id = tenant_id;
            // The party the run acts on, by the identifier the refusal above
            // resolved, so the run can be found from the party it works on.
            start.target_kind = std::string(ores::iam::workflow::provision_party_target_kind);
            start.target_id = found.front();
            start.request_json = rfl::json::write(run);
            start.correlation_id = correlation_id;
            start.instance_id = instance_id;

            nats_.js_publish(ores::workflow::messaging::start_workflow_message::nats_subject,
                             ores::nats::default_wire_codec().encode(start),
                             ores::nats::service::forwarded_caller_headers(msg));

            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "Started " << start.type << " for party " << found.front() << " of tenant "
                << code << " (instance: " << instance_id << ")";

            reply(nats_,
                  msg,
                  provision_party_command_response{
                      .success = true, .instance_id = instance_id, .party_id = found.front()});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_,
                  msg,
                  provision_party_command_response{.success = false, .message = e.what()});
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

    /**
     * @brief Serves iam.v1.ops.attach_account_pictures.
     *
     * The reconcile the attach_photos step runs, reachable on its own so an
     * operator can re-run it from a client and so a scope that is not a
     * provisioning run can be reconciled too.
     */
    void attach_account_pictures(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(tenant_provisioning_handler_lg(), msg);

        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        auto req = decode<ores::iam::messaging::attach_account_pictures_request>(msg);
        if (!req) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        ores::iam::messaging::attach_account_pictures_response response;
        try {
            auto caller_ctx = *ctx_expected;
            ores::iam::service::account_service accounts(caller_ctx);
            const auto caller = accounts.get_account_by_username(caller_ctx.actor());
            if (!caller) {
                response.result.outcome = ores::utility::domain::outcome::invalid;
                response.result.code = "caller_unknown";
                response.result.message = "The caller holds no account to act as.";
                reply(nats_, msg, response);
                return;
            }
            const auto tenant_id = caller_ctx.tenant_id().to_string();
            auto client = make_step_client(tenant_id,
                                           caller->id,
                                           caller->default_party_id.value_or(boost::uuids::uuid{}),
                                           caller->username);
            std::vector<std::string> images;
            attach_staff_photos(client, caller_ctx, tenant_id, images);
            response.image_ids = std::move(images);
            response.result.outcome = ores::utility::domain::outcome::ok;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            response.result.outcome = ores::utility::domain::outcome::failed;
            response.result.code = "internal_error";
            response.result.message = e.what();
            reply(nats_, msg, response);
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

    /**
     * @brief How long a step waits on the publication it started.
     *
     * A backstop for a workflow service that has gone, and not a deadline for
     * the work: the engine fails a step that outlives the budget its definition
     * states, so this patience sits above every budget a definition states and
     * the engine's deadline is always the one that fails. The value it replaces
     * was read as a timetable -- "the base bundle is the slowest the ACME path
     * published, and it waited 1500s" -- when 1500s was the length of a hang
     * that a dead service produced, which is how a timeout came to name a step
     * instead of a cause.
     */
    static constexpr std::chrono::seconds step_publish_timeout{7200};

    /// The image codes of the default administrator pictures. The
    /// assets.system_avatars dataset publishes rows under these codes, and
    /// the attach_photos step binds each picture by code.
    static constexpr std::string_view super_admin_avatar_key{"super_admin_avatar"};
    static constexpr std::string_view tenant_admin_avatar_key{"tenant_admin_avatar"};

    /**
     * @brief Resolves the administrator a step acts as.
     *
     * The command names the account; its username and its party go into the
     * minted token, because a token's party is what scopes the visible set a
     * request runs under. A step whose administrator is gone still acts as
     * the account the command names, with the tenant's code as the username.
     */
    step_actor
    resolve_step_actor(const ores::iam::workflow::provision_tenant_step_command& command) {
        boost::uuids::string_generator parse;
        const auto account_id = parse(command.admin_account_id);
        auto tenant_ctx = tenant_context::with_tenant(ctx_, command.tenant_id);
        ores::iam::service::account_service accounts(tenant_ctx);
        const auto account = accounts.get_account(account_id);

        step_actor actor;
        if (account)
            actor = actor_of(tenant_ctx, *account);
        else
            actor.account_id = account_id;
        if (actor.username.empty())
            actor.username = command.tenant_code;
        return actor;
    }

    /// The acting identity of an account the caller read: the username and
    /// the default party, falling back to the account's first party link.
    /// The bootstrap administrator carries no default party, only the link
    /// the initial admin function writes, so the fallback is what scopes its
    /// token.
    static step_actor actor_of(ores::database::context& ctx,
                               const ores::iam::domain::account& account) {
        step_actor actor;
        actor.account_id = account.id;
        actor.username = account.username;
        if (account.default_party_id) {
            actor.party_id = *account.default_party_id;
        } else {
            ores::iam::service::account_party_service links(ctx);
            const auto parties = links.list_account_parties_by_account(account.id);
            if (!parties.empty())
                actor.party_id = parties.front().party_id;
        }
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
            case provision_step_action::system_provision:
                system_provision_step(wf, command);
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

    /// Publishes the system's own datasets into the system tenant, which is
    /// where the installation is read from: a coding-scheme validator resolves
    /// its scheme there, a template image is read from there across the
    /// tenant-isolation policy, and several reference reads treat its rows as a
    /// shared overlay on the tenant's own.
    ///
    /// The step runs before the tenant's own steps, so the tenant is built on a
    /// system that already makes sense. It acts as a system administrator rather
    /// than as the run's actor, because the rows belong to the system tenant and
    /// the account that writes them must be one of its own.
    void system_provision_step(const ores::service::messaging::workflow_step_context& wf,
                               const ores::iam::workflow::provision_tenant_step_command& command) {
        auto sys_ctx = tenant_context::with_system_tenant(ctx_);
        ores::iam::service::account_service system_accounts(sys_ctx);
        boost::uuids::string_generator parse;

        dq::messaging::publish_bundle_params params;
        const auto params_json = dq::messaging::build_params_json(params);
        const auto bundle_code = std::string(ores::iam::workflow::system_core_bundle_code);

        for (const auto& id : super_admin_account_ids(sys_ctx)) {
            const auto account = system_accounts.get_account(parse(id));
            if (!account)
                continue;
            const auto super_actor = actor_of(sys_ctx, *account);
            auto admin_client = make_step_client(tenant_context::system_tenant_id,
                                                 super_actor.account_id,
                                                 super_actor.party_id,
                                                 super_actor.username);
            publish_bundle_or_throw(
                admin_client, bundle_code, super_actor.username, params_json, command.kind);
            wf.complete(rfl::json::write(
                provision_step_result{.kind = command.kind, .bundles = {bundle_code}}));
            return;
        }
        wf.fail("The system tenant holds no administrator to publish '" + bundle_code + "' as.");
    }

    /// Publishes each bundle the step names, one nested run each, and reports
    /// only once every run has finished. The datasets the profile opts in
    /// travel with the publication, so a bundle that publishes a member only
    /// when a starting point asks for it publishes it here.
    void publish_bundle_step(const ores::service::messaging::workflow_step_context& wf,
                             const ores::iam::workflow::provision_tenant_step_command& command,
                             const step_actor& actor) {
        const auto arguments =
            parse_publish_bundle_arguments(command.arguments_json, command.parameters);
        auto client =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);

        dq::messaging::publish_bundle_params params;
        params.opted_in_datasets = arguments.opted_in_datasets;
        // A bundle may publish party-scoped rows — the market data observations
        // a synthetic theme carries, for one — and those publications read the
        // party from here. The step acts for the tenant's system party, which
        // is where a shared stream belongs.
        if (!actor.party_id.is_nil())
            params.party_id = boost::uuids::to_string(actor.party_id);
        const auto params_json = dq::messaging::build_params_json(params);
        for (const auto& bundle_code : arguments.bundles)
            publish_bundle_or_throw(client, bundle_code, actor.username, params_json, command.kind);

        wf.complete(rfl::json::write(
            provision_step_result{.kind = command.kind, .bundles = arguments.bundles}));
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
            wf.warn(
                rfl::json::write(provision_step_result{
                    .kind = command.kind, .bundles = arguments.bundles, .root_lei = ""}),
                {ores::workflow::messaging::step_log_entry{
                    .level = ores::workflow::messaging::step_log_level::warn,
                    .message = "The starting point names no legal entity, so there was nothing to "
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

    /// The whole party stage: publishes each bundle the step names against the
    /// party, activates it, marks its onboarding complete, and associates the
    /// run's administrator with it.
    ///
    /// The step acts on every party the tenant holds when its arguments name
    /// none, which is what a tenant's own run asks for, and on the one party a
    /// request names when it names one. Both readings run the same code, so an
    /// administrator who adds a party later gets what the tenant's first
    /// provisioning got, and the engine gives the stage a progress record and a
    /// retry either way.
    void provision_party_step(const ores::service::messaging::workflow_step_context& wf,
                              const ores::iam::workflow::provision_tenant_step_command& command,
                              const step_actor& actor) {
        const auto arguments = parse_provision_party_arguments(command.arguments_json);

        auto discover =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);

        std::vector<ores::refdata::domain::party> parties;
        if (!arguments.party.empty()) {
            auto found = resolve_party(discover, arguments.party);
            if (!found)
                throw std::runtime_error("The tenant holds no party named '" + arguments.party +
                                         "'.");
            parties.push_back(std::move(*found));
        } else {
            parties = list_all_parties(discover);
        }

        std::vector<std::string> provisioned;
        for (const auto& party : parties) {
            const auto party_id = boost::uuids::to_string(party.id);
            auto client =
                make_step_client(command.tenant_id, actor.account_id, party.id, actor.username);
            dq::messaging::publish_bundle_params params;
            params.party_id = party_id;
            const auto params_json = dq::messaging::build_params_json(params);
            for (const auto& bundle_code : arguments.bundles)
                publish_bundle_or_throw(client, bundle_code, actor.username, params_json, party_id);

            activate_party(client, party);
            complete_party_onboarding(client, party.id);
            associate_account_with_party(client, actor.account_id, party.id);

            /*
             * Last, because it is the one write here that touches the party
             * itself: an identifier's insert bumps the version of the party it
             * belongs to, and a party the step has already saved would then be
             * refused as one that moved on.
             */
            if (!arguments.lei.empty())
                record_party_lei(client, party, arguments.lei);

            provisioned.push_back(party_id);
        }

        wf.complete(rfl::json::write(provision_step_result{
            .kind = command.kind, .bundles = arguments.bundles, .parties = provisioned}));
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

        // The administrators' pictures are the product's own, so they are
        // attached every time the kind runs, not from the profile's data.
        attach_administrator_photos(discover, tenant_ctx, command, actor, images);

        for (const auto& assignment : arguments.parties) {
            auto party = find_party(discover, assignment.party_name);
            if (!party)
                throw std::runtime_error("The tenant holds no party named '" +
                                         assignment.party_name + "'.");

            auto client =
                make_step_client(command.tenant_id, actor.account_id, party->id, actor.username);
            attach_staff_photos(client, tenant_ctx, command.tenant_id, images);

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

    /// Binds every party the tenant holds to the theme the step names, through
    /// the configurations the tenant's bundle publication already stored, and
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
                "The tenant holds no system party to resolve its market configuration against.");

        auto system_client =
            make_step_client(command.tenant_id, actor.account_id, system_party->id, actor.username);

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

        wf.complete(rfl::json::write(provision_step_result{
            .kind = command.kind, .parties = bound, .datasets = {arguments.theme}}));
    }

    /// Marks the tenant active and clears bootstrap mode, the two operations
    /// the completing step performs. The tenant is active once it holds its
    /// data, so a flag that will not clear is a warning rather than a failure.
    /// The step acts as the tenant's system party, because that is the scope
    /// the flags it writes belong to and the scope the write reads them back
    /// under; see the body.
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

        auto discover =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);
        // The flags this step writes belong to the tenant and live under its
        // system party, so the step acts as that party and not as the
        // administrator's own. The write reads the row it replaces back under
        // the scope it writes in, and an administrator whose default party is
        // one the provisioning created cannot see the system party's row from
        // their own: the clear is then refused as a duplicate create, and the
        // tenant is left reporting bootstrap mode.
        const auto settings_party = find_system_party(discover);
        if (!settings_party)
            throw std::runtime_error("The tenant holds no system party to hold its own settings.");
        auto client = make_step_client(
            command.tenant_id, actor.account_id, settings_party->id, actor.username);

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

    /// Every party the tenant holds, one page at a time. A single page with a
    /// fixed limit would see the first thousand and report the rest missing.
    static std::vector<ores::refdata::domain::party>
    list_all_parties(internal_request_client& client) {
        std::vector<ores::refdata::domain::party> parties;
        std::uint32_t offset = 0;
        constexpr std::uint32_t page_size = 1000;
        while (true) {
            ores::refdata::messaging::list_parties_request request;
            request.offset = offset;
            request.limit = page_size;
            const auto page = client.request(request).parties;
            for (const auto& party : page)
                parties.push_back(party);
            if (page.size() < page_size)
                return parties;
            offset += page_size;
        }
    }

    /// The party a reference names, by its identifier when it is one and by its
    /// exact full name when it is not.
    ///
    /// A person types the name they know a party by, and a script states the
    /// identifier it read; both reach the same party through one read, because
    /// the party read is what answers either.
    static std::optional<ores::refdata::domain::party>
    resolve_party(internal_request_client& client, const std::string& reference) {
        std::optional<boost::uuids::uuid> wanted_id;
        try {
            wanted_id = boost::lexical_cast<boost::uuids::uuid>(reference);
        } catch (const boost::bad_lexical_cast&) {
        }

        std::vector<ores::refdata::domain::party> matches;
        for (const auto& party : list_all_parties(client)) {
            if (wanted_id ? party.id == *wanted_id : party.full_name == reference)
                matches.push_back(party);
        }
        // A legal name is not unique, so a name that matches two parties is
        // refused rather than guessed at: a step acts on one party.
        if (matches.size() > 1)
            throw std::runtime_error("The tenant holds more than one party named '" + reference +
                                     "'; name it by its identifier.");
        if (matches.empty())
            return std::nullopt;
        return matches.front();
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
    // market config in the consistent world, and the scope a tenant's own
    // settings live under.
    static std::optional<ores::refdata::domain::party>
    find_system_party(internal_request_client& client) {
        for (const auto& party : list_all_parties(client))
            if (party.party_category == "System")
                return party;
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

        // Every feed the config starts, FX and IR alike, is bound by its source:
        // a tick names its own datum, so a binding names only who consumes it.
        std::vector<std::string> sources;
        const auto collect = [&](auto req, const char* what, auto configs_of) {
            req.limit = 1000;
            auto resp = resolve_client.request(req);
            if (resp.result.outcome != ores::utility::domain::outcome::ok) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                    << "create_theme_feed_bindings: list " << what
                    << " failed: " << resp.result.message;
                return false;
            }
            for (const auto& c : configs_of(resp))
                if (c.enabled && c.config_id == config_id)
                    sources.push_back(c.source_name);
            return true;
        };
        if (!collect(synthetic::messaging::list_fx_spot_generation_configs_request{},
                     "fx_spot_generation_configs",
                     [](const auto& r) -> const auto& { return r.fx_spot_generation_configs; }) ||
            !collect(synthetic::messaging::list_ir_curve_generation_configs_request{},
                     "ir_curve_generation_configs",
                     [](const auto& r) -> const auto& { return r.ir_curve_generation_configs; }))
            return false;

        // Active bindings already exist per natural key (tenant, party, source);
        // skip them so re-provisioning does not trip the unique index. The list
        // is tenant-scoped, so filter down to this party's rows.
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
                    existing.push_back(b.source_name);
        }

        boost::uuids::random_generator uuid_gen;
        boost::uuids::string_generator sg;
        bool all_saved = true;
        for (const auto& source_name : sources) {
            if (std::find(existing.begin(), existing.end(), source_name) != existing.end()) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                    << "create_theme_feed_bindings: binding " << source_name << " for party "
                    << party_id_str << " already exists; skipping";
                continue;
            }
            marketdata::messaging::put_feed_binding_request req;
            req.change.write.id = uuid_gen();
            req.change.write.source_name = source_name;
            // These bindings name the theme's simulated streams, so the series
            // the ingest loop stamps from them are generated, not observed.
            req.change.write.producer_kind = "SYNTHETIC";
            req.change.write.enabled = true;
            req.change.write.party_id = sg(party_id_str);
            // The stream these bindings consume is the tenant's own simulated
            // one, not a real feed.
            req.change.write.producer_kind = "SYNTHETIC";
            // The write states no expectation, because the store's
            // must-not-exist claim is keyed on the source alone while the
            // binding's natural key is the party with the source. A
            // must-not-exist claim would refuse the second party's binding for
            // a source the first party already holds. The freshness check above
            // keeps this idempotent, and the natural key's unique index keeps
            // it honest.
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
            } else {
                // The freshness list is read once, so a source this loop just bound
                // has to join it: two enabled configs naming one source would
                // otherwise both pass the check and the second would meet the
                // unique index instead of the skip above.
                existing.push_back(source_name);
            }
        }
        if (sources.empty())
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "create_theme_feed_bindings: no enabled feed sources for dataset "
                << dataset_code;
        return all_saved;
    }

    // Ensures the caller tenant holds the image with this code, copying the
    // system tenant's template through the assets service when it does not,
    // and answers the tenant image's id. The tenant and the actor are the
    // session's, so the copy lands where the work is happening; the assets
    // service owns both the cross-tenant template read and the write, so
    // neither is repeated here.
    std::optional<boost::uuids::uuid> copy_template_image(internal_request_client& client,
                                                          const std::string& key) {
        ores::assets::messaging::ensure_image_request req;
        req.code = key;
        auto resp = client.request(req);
        if (resp.result.outcome != ores::utility::domain::outcome::ok || resp.image_id.empty()) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                << "No tenant image for '" << key << "': " << resp.result.message;
            return std::nullopt;
        }
        return boost::uuids::string_generator{}(resp.image_id);
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
            attach_staff_photos(client, ctx, tenant_id, images);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                << "attach_staff_photos did not complete for " << dataset_code << ": " << e.what();
        }
    }

    // Attaches a profile picture to every account that names one it does not
    // have. The wanted code rides the account, so the reconcile reads it from
    // the account rather than from a DQ staging table, and the scope is simply
    // "the accounts this tenant holds". Each picture goes on through
    // attach_account_photo, which leaves an account that already has one alone.
    void attach_staff_photos(internal_request_client& client,
                             [[maybe_unused]] ores::database::context& ctx,
                             [[maybe_unused]] const std::string& tenant_id,
                             std::vector<std::string>& images) {
        iam::messaging::list_accounts_request accounts_req;
        accounts_req.limit = 10'000;
        auto accounts_resp = client.request(accounts_req);
        for (const auto& a : accounts_resp.accounts) {
            if (a.picture_code.empty())
                continue;
            attach_account_photo(client,
                                 ctx,
                                 tenant_id,
                                 a,
                                 a.picture_code,
                                 "Attached staff photo during provisioning",
                                 images);
        }
    }

    /// Copies the named template image into the tenant and saves it on the
    /// account, which the caller read first. An account that already carries
    /// a picture is left untouched, which is what makes a repeated run leave
    /// it unchanged.
    void attach_account_photo(internal_request_client& client,
                              [[maybe_unused]] ores::database::context& ctx,
                              [[maybe_unused]] const std::string& tenant_id,
                              const ores::iam::domain::account& account,
                              const std::string& template_key,
                              const std::string& commentary,
                              std::vector<std::string>& images) {
        if (account.image_id)
            return;

        auto image_id = copy_template_image(client, template_key);
        if (!image_id)
            throw std::runtime_error("The template image '" + template_key +
                                     "' was not copied into the tenant.");

        iam::messaging::update_account_request update_req;
        update_req.account_id = boost::uuids::to_string(account.id);
        update_req.email = account.email;
        update_req.full_name = account.full_name;
        update_req.default_party_id =
            account.default_party_id ? boost::uuids::to_string(*account.default_party_id) : "";
        update_req.job_title = account.job_title;
        update_req.reports_to_account_id =
            account.reports_to_account_id ?
                boost::uuids::to_string(*account.reports_to_account_id) :
                "";
        update_req.image_id = boost::uuids::to_string(*image_id);
        update_req.change_reason_code = "system.external_data_import";
        update_req.change_commentary = commentary;
        const auto resp = client.request(update_req);
        if (!resp.success)
            throw std::runtime_error("The picture was not attached to the account '" +
                                     account.username + "': " + resp.message);
        images.push_back(boost::uuids::to_string(*image_id));
    }

    /// Attaches the two administrators' pictures: the tenant administrator
    /// the run acts as, and every super administrator of the system tenant,
    /// each written through a token minted for that account. The template
    /// keys are constants here rather than step arguments because these
    /// pictures are the product's own; the parties' datasets stay arguments
    /// because those are the profile's data.
    void
    attach_administrator_photos(internal_request_client& client,
                                ores::database::context& tenant_ctx,
                                const ores::iam::workflow::provision_tenant_step_command& command,
                                const step_actor& actor,
                                std::vector<std::string>& images) {
        ores::iam::service::account_service tenant_accounts(tenant_ctx);
        if (const auto admin = tenant_accounts.get_account(actor.account_id))
            attach_account_photo(client,
                                 tenant_ctx,
                                 command.tenant_id,
                                 *admin,
                                 std::string(tenant_admin_avatar_key),
                                 "Attached the tenant administrator's picture during provisioning",
                                 images);

        // The system tenant needs no copy: its published row is the tenant's
        // own picture, and copy_template_image finds it by code, so the
        // super administrators share that one row deliberately.
        auto sys_ctx = tenant_context::with_system_tenant(ctx_);
        ores::iam::service::account_service system_accounts(sys_ctx);
        boost::uuids::string_generator parse;
        for (const auto& id : super_admin_account_ids(sys_ctx)) {
            const auto account = system_accounts.get_account(parse(id));
            if (!account)
                continue;
            const auto super_actor = actor_of(sys_ctx, *account);
            auto super_client = make_step_client(tenant_context::system_tenant_id,
                                                 super_actor.account_id,
                                                 super_actor.party_id,
                                                 super_actor.username);
            attach_account_photo(super_client,
                                 sys_ctx,
                                 tenant_context::system_tenant_id,
                                 *account,
                                 std::string(super_admin_avatar_key),
                                 "Attached the super administrator's picture during provisioning",
                                 images);

            // A service account holds only the permissions its own domain
            // needs, and attaching a picture is not one of them, so an
            // administrator acts here as it does for its own picture.
            attach_service_account_pictures(super_client, sys_ctx, images);
        }
    }

    /// The registry names a picture for each service account, and the seed
    /// writes that name onto the account. Turning the name into a picture is
    /// what gives a seeded account its own icon, and it also repairs an
    /// account that was seeded before its picture existed. The scope is the
    /// system tenant, because that is where the service accounts live, and
    /// attach_account_photo leaves an account that already carries a picture
    /// alone, so this never overwrites the administrators.
    void attach_service_account_pictures(internal_request_client& client,
                                         ores::database::context& sys_ctx,
                                         std::vector<std::string>& images) {
        ores::iam::service::account_service system_accounts(sys_ctx);
        for (const auto& account : system_accounts.list_accounts(0, 10'000)) {
            if (account.picture_code.empty() || account.image_id)
                continue;
            attach_account_photo(client,
                                 sys_ctx,
                                 tenant_context::system_tenant_id,
                                 account,
                                 account.picture_code,
                                 "Attached the service account's picture during provisioning",
                                 images);
        }
    }

    /// The accounts of the system tenant that hold the SuperAdmin role, which
    /// is the rule the bootstrap service uses to find the deployment's
    /// administrators.
    static std::vector<std::string> super_admin_account_ids(ores::database::context& sys_ctx) {
        return execute_parameterized_string_query(
            sys_ctx,
            "SELECT DISTINCT a.id::text FROM ores_iam_accounts_tbl a "
            "JOIN ores_iam_account_roles_tbl ar ON ar.account_id = a.id "
            "AND ar.tenant_id = a.tenant_id "
            "AND ar.valid_to = ores_utility_infinity_timestamp_fn() "
            "JOIN ores_iam_roles_tbl r ON r.id = ar.role_id "
            "AND r.tenant_id = ar.tenant_id "
            "AND r.valid_to = ores_utility_infinity_timestamp_fn() "
            "WHERE a.tenant_id = $1::uuid "
            "AND a.valid_to = ores_utility_infinity_timestamp_fn() AND r.name = $2",
            {tenant_context::system_tenant_id, ores::iam::domain::roles::super_admin},
            tenant_provisioning_handler_lg(),
            "attach_administrator_photos");
    }

    /// Copies the named template image into the tenant and saves it on the
    /// party, which the caller read first so the write states its version.
    void attach_party_logo(internal_request_client& client,
                           [[maybe_unused]] ores::database::context& ctx,
                           const ores::refdata::domain::party& party,
                           const std::string& template_key,
                           [[maybe_unused]] const std::string& tenant_id,
                           std::vector<std::string>& images) {
        const auto image_id = copy_template_image(client, template_key);
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

    /**
     * Records the legal entity a party was built from, as the party's LEI.
     *
     * The party's own datum, written for the party: the caller's client acts in
     * the party it named, which is what a party identifier is stamped with. A
     * party that already carries an LEI is left alone, so a retried run meets
     * the row it wrote and moves on rather than refusing itself.
     */
    static void record_party_lei(internal_request_client& client,
                                 const ores::refdata::domain::party& party,
                                 const std::string& lei) {
        ores::refdata::messaging::list_by_party_id_party_identifiers_request list_req;
        list_req.party_id = party.id;
        list_req.limit = 1000;
        const auto existing = client.request(list_req);
        for (const auto& identifier : existing.party_identifiers)
            if (identifier.id_scheme == "LEI")
                return;

        boost::uuids::random_generator generate;
        ores::refdata::messaging::put_party_identifier_request req;
        req.change.write.id = generate();
        req.change.write.party_id = party.id;
        req.change.write.id_scheme = "LEI";
        req.change.write.id_value = lei;
        req.change.precondition.kind = ores::utility::domain::precondition_kind::must_not_exist;
        req.intent.reason_code = "system.external_data_import";
        req.intent.commentary = "Recorded from the legal entity the party was built from";

        const auto resp = client.request(req);
        if (resp.result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("The LEI of '" + party.full_name +
                                     "' was not recorded: " + resp.result.message);
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
                      [[maybe_unused]] ores::database::context& ctx,
                      [[maybe_unused]] const std::string& tenant_id,
                      const boost::uuids::uuid& account_id,
                      [[maybe_unused]] const std::string& username,
                      ores::refdata::domain::party party,
                      bool set_default) {
        try {
            const auto status = party.status == "Active" ? party.status : std::string("Active");
            auto image_id = party.image_id;
            if (!image_id)
                image_id = copy_template_image(client, "acme_party_logo");
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
