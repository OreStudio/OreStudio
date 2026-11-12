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
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.dq.api/messaging/dataset_protocol.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.dq.api/messaging/publish_params.hpp"
#include "ores.iam.api/domain/role_codes.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/tenant_provisioning_protocol.hpp"
#include "ores.iam.api/workflow/provision_tenant_workflow.hpp"
#include "ores.iam.core/messaging/party_curve_blueprints.hpp"
#include "ores.iam.core/messaging/party_surface_blueprints.hpp"
#include "ores.iam.core/messaging/party_surface_hold_back.hpp"
#include "ores.iam.core/messaging/population_knobs.hpp"
#include "ores.iam.core/messaging/provision_step_arguments.hpp"
#include "ores.iam.core/messaging/report_schedule_selection.hpp"
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
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/catalogue_ids.hpp"
#include "ores.marketdata.api/messaging/curve_publish_operations_protocol.hpp"
#include "ores.marketdata.api/messaging/feed_protocol.hpp"
#include "ores.marketdata.api/messaging/fx_forward_operation_protocol.hpp"
#include "ores.marketdata.api/messaging/fx_vol_surface_operation_protocol.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.api/messaging/published_curves_operations_protocol.hpp"
#include "ores.marketdata.api/messaging/route_protocol.hpp"
#include "ores.marketdata.api/messaging/subscription_protocol.hpp"
#include "ores.nats/domain/delivery.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/calendar_materialisation_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/currency_pair_convention_protocol.hpp"
#include "ores.refdata.api/messaging/fx_vol_surface_blueprint_protocol.hpp"
#include "ores.refdata.api/messaging/fx_vol_surface_pillar_protocol.hpp"
#include "ores.refdata.api/messaging/fx_vol_surface_pillar_source_protocol.hpp"
#include "ores.refdata.api/messaging/ir_curve_bootstrap_config_protocol.hpp"
#include "ores.refdata.api/messaging/ir_curve_bootstrap_pillar_protocol.hpp"
#include "ores.refdata.api/messaging/party_counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/party_identifier_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.reporting.api/messaging/report_definition_protocol.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/domain/trade_population_plan.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.api/messaging/folder_protocol.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_book_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_counterparty_protocol.hpp"
#include "ores.synthetic.api/messaging/trade_generation_trade_type_protocol.hpp"
#include "ores.synthetic.core/service/trade_population_planner.hpp"
#include "ores.synthetic.core/service/trade_population_service.hpp"
#include "ores.synthetic.core/trading/trading_trade_population_writer.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
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
#include <map>
#include <memory>
#include <optional>
#include <ranges>
#include <set>
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
            start.request_json = ores::utility::codec::json::write(run);
            start.correlation_id = correlation_id;
            start.instance_id = instance_id;

            nats_.js_publish(
                ores::workflow::messaging::start_workflow_message::nats_subject,
                ores::nats::default_wire_codec().encode(start),
                ores::nats::headers::with_publish_identity(
                    ores::nats::service::forwarded_caller_headers(msg), start.instance_id));

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
                    ores::database::query_builder::execute_parameterized_string_query(
                        tenant_ctx,
                        "SELECT id::text FROM ores_refdata_parties_tbl WHERE tenant_id = $1::uuid "
                        "AND valid_to = ores_utility_infinity_timestamp_fn() AND id = $2::uuid",
                        {tenant_id, boost::uuids::to_string(*wanted_id)},
                        tenant_provisioning_handler_lg(),
                        "provision_party") :
                    ores::database::query_builder::execute_parameterized_string_query(
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
            const auto tenant_row =
                ores::database::query_builder::execute_parameterized_multi_column_query(
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
            arguments.root().set("party", found.front());
            /*
             * The entity the person chose travels with the step, and the step
             * records it against the party: a party identifier carries the
             * party its writing session acts in, and this caller works in
             * another one, so the run is the only client that can write it.
             */
            if (!req->lei.empty())
                arguments.root().set("lei", req->lei);
            run.steps.push_back({party_step->step_kind, arguments.write()});

            boost::uuids::random_generator generate;
            const auto instance_id = boost::uuids::to_string(generate());

            ores::workflow::messaging::start_workflow_message start;
            start.type = std::string(ores::iam::workflow::provision_party_workflow_type);
            start.tenant_id = tenant_id;
            // The party the run acts on, by the identifier the refusal above
            // resolved, so the run can be found from the party it works on.
            start.target_kind = std::string(ores::iam::workflow::provision_party_target_kind);
            start.target_id = found.front();
            start.request_json = ores::utility::codec::json::write(run);
            start.correlation_id = correlation_id;
            start.instance_id = instance_id;

            nats_.js_publish(
                ores::workflow::messaging::start_workflow_message::nats_subject,
                ores::nats::default_wire_codec().encode(start),
                ores::nats::headers::with_publish_identity(
                    ores::nats::service::forwarded_caller_headers(msg), start.instance_id));

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
    void provision_step(ores::nats::delivery delivery) {
        const auto& msg = delivery.msg();
        const auto correlation_id = log_handler_entry(tenant_provisioning_handler_lg(), msg);

        auto wf = ores::service::messaging::workflow_step_context::from_message(nats_, msg);
        if (!wf) {
            // The engine can only see that no answer came back, so a command
            // discarded here in silence is indistinguishable from one that was
            // never delivered. Say so where the discard happens: the engine has
            // no other way to learn that the command did reach a service.
            // A command that cannot be read will not read any better on a
            // second delivery, and it names no step to fail, so it is discarded
            // rather than retried.
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), error)
                << "provision_step: the step command could not be read, so the step is left "
                   "unanswered and the run will expire it. subject="
                << msg.subject;
            std::move(delivery).reject();
            return;
        }

        BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
            << "provision_step: step_id=" << wf->step_id << " instance=" << wf->instance_id
            << " tenant=" << wf->tenant_id;

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
                std::move(delivery).commit();
                return;
            }

            const std::string_view payload(reinterpret_cast<const char*>(msg.data.data()),
                                           msg.data.size());
            auto parsed = ores::utility::codec::json::read_expected<
                ores::iam::workflow::provision_tenant_step_command>(payload);
            if (!parsed) {
                // The step is failed, so the delivery is done with: re-running
                // it would only report the same failure again.
                wf->fail("The provisioning step command could not be read: " +
                         std::string(parsed.error()));
                std::move(delivery).commit();
                return;
            }

            const auto started = std::chrono::steady_clock::now();
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "provision_step " << parsed->kind << " started: step_id=" << wf->step_id
                << " instance=" << wf->instance_id;
            execute_step(*wf, *parsed, correlation_id);
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "provision_step " << parsed->kind << " returned after "
                << std::chrono::duration<double>(std::chrono::steady_clock::now() - started).count()
                << "s: step_id=" << wf->step_id;
            std::move(delivery).commit();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), error)
                << "provision_step failed: step_id=" << wf->step_id << ": " << e.what();
            wf->fail(e.what());
            std::move(delivery).commit();
        } catch (...) {
            // An exception of no standard type would otherwise leave the step with no answer and
            // no line saying why, so the run would wait out its deadline in silence.
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), error)
                << "provision_step failed with an exception of no standard type: step_id="
                << wf->step_id;
            wf->fail("The step failed with an exception of no standard type.");
            std::move(delivery).commit();
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
public:
    struct provision_step_result {
        /// What one party's report definitions came to when the step scheduled
        /// them. Read together, the entries say how much of the catalogue a
        /// run put on the scheduler and how much it left for a person.
        struct party_report_schedule {
            std::string party;
            std::uint32_t scheduled = 0;
            std::uint32_t already_scheduled = 0;
            std::uint32_t on_demand = 0;
            std::uint32_t not_a_cron = 0;
        };

        std::string kind;
        std::vector<std::string> bundles;
        std::string root_lei;
        std::vector<std::string> parties;
        /// The account usernames a step acted on, for the kinds whose work is
        /// per account rather than per party.
        std::vector<std::string> accounts;
        std::vector<std::string> images;
        std::vector<std::string> datasets;
        std::string tenant_id;
        /// The instance id of the run this step started, when the kind hands
        /// the tenant to another run. Empty for every other kind.
        std::string child_instance_id;
        /// The trade generation configs a generate_trades step recorded, and
        /// what the population it wrote came to.
        std::vector<std::string> configs;
        std::uint32_t trades_planned = 0;
        std::uint32_t trades_written = 0;
        std::uint32_t trades_already_present = 0;
        /// What a schedule_reports step did for each party it named.
        std::vector<party_report_schedule> report_schedules;
        /// The curves a publish_system_curves step published, by config name,
        /// and the ones it could not, each with the reason it was last refused.
        std::vector<std::string> curves;
        std::vector<std::string> curves_not_published;
        /// The forward sets a publish_party_forwards step published, the ones a party had
        /// already, the pairs a party cannot build with the service's reason, and the ones it
        /// raised and could not publish, each as "party: pair: detail".
        std::vector<std::string> forwards;
        std::vector<std::string> forwards_already_published;
        std::vector<std::string> forwards_skipped;
        std::vector<std::string> forwards_not_published;
        /// The surfaces a publish_party_surfaces step published, the ones a party had already,
        /// the ones held back because they failed an arbitrage or error check, and the ones it
        /// could not calibrate or publish, each as "party: blueprint: detail".
        std::vector<std::string> surfaces;
        std::vector<std::string> surfaces_already_published;
        std::vector<std::string> surfaces_held_back;
        std::vector<std::string> surfaces_not_published;
    };

private:
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
    /// The correlation id travels with the command so a step that starts
    /// another run keeps the trace of the run that started it.
    void execute_step(const ores::service::messaging::workflow_step_context& wf,
                      const ores::iam::workflow::provision_tenant_step_command& command,
                      const std::string& correlation_id) {
        switch (classify_step_kind(command.kind)) {
            case provision_step_action::complete_provisioning:
                complete_provisioning_step(wf, command);
                return;
            case provision_step_action::start_tenant_setup:
                start_tenant_setup_step(wf, command, correlation_id);
                return;
            case provision_step_action::system_provision:
                system_provision_step(wf, command);
                return;
            case provision_step_action::publish_system_curves:
                publish_system_curves_step(wf, command);
                return;
            case provision_step_action::publish_bundle:
                publish_bundle_step(wf, command, resolve_step_actor(command));
                return;
            case provision_step_action::start_auto_approval:
                set_auto_approval_step(wf,
                                       command,
                                       resolve_step_actor(command),
                                       true,
                                       "Provisioning runs without waiting on approvals.");
                return;
            case provision_step_action::end_auto_approval:
                set_auto_approval_step(wf,
                                       command,
                                       resolve_step_actor(command),
                                       false,
                                       "Provisioning is finished, so people decide from here on.");
                return;
            case provision_step_action::build_calendars:
                build_calendars_step(wf, command, resolve_step_actor(command));
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
            case provision_step_action::set_staff_passwords:
                set_staff_passwords_step(wf, command, resolve_step_actor(command));
                return;
            case provision_step_action::attach_photos:
                attach_photos_step(wf, command, resolve_step_actor(command));
                return;
            case provision_step_action::subscribe_market_data:
                subscribe_market_data_step(wf, command, resolve_step_actor(command));
                return;
            case provision_step_action::publish_party_curves:
                publish_party_curves_step(wf, command, resolve_step_actor(command));
                return;
            case provision_step_action::give_party_surface_blueprints:
                give_party_surface_blueprints_step(wf, command, resolve_step_actor(command));
                return;
            case provision_step_action::publish_party_forwards:
                publish_party_forwards_step(wf, command, resolve_step_actor(command));
                return;
            case provision_step_action::publish_party_surfaces:
                publish_party_surfaces_step(wf, command, resolve_step_actor(command));
                return;
            case provision_step_action::generate_trades:
                generate_trades_step(wf, command, resolve_step_actor(command));
                return;
            case provision_step_action::schedule_reports:
                schedule_reports_step(wf, command, resolve_step_actor(command));
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
    /// shared overlay on the tenant's own. It then records that the system setup
    /// finished.
    ///
    /// The step runs before the tenant's own steps, so the tenant is built on a
    /// system that already makes sense. It acts as a system administrator rather
    /// than as the run's actor, because the rows belong to the system tenant and
    /// the account that writes them must be one of its own.
    void system_provision_step(const ores::service::messaging::workflow_step_context& wf,
                               const ores::iam::workflow::provision_tenant_step_command& command) {
        dq::messaging::publish_bundle_params params;
        const auto params_json = dq::messaging::build_params_json(params);
        const auto bundle_code = std::string(ores::iam::workflow::system_core_bundle_code);

        const auto super_actor = system_administrator();
        if (!super_actor) {
            wf.fail("The system tenant holds no administrator to publish '" + bundle_code +
                    "' as.");
            return;
        }
        auto admin_client = make_step_client(tenant_context::system_tenant_id,
                                             super_actor->account_id,
                                             super_actor->party_id,
                                             super_actor->username);
        publish_bundle_or_throw(
            admin_client, bundle_code, super_actor->username, params_json, command.kind);
        complete_system_onboarding(admin_client);
        wf.complete(ores::utility::codec::json::write(
            provision_step_result{.kind = command.kind, .bundles = {bundle_code}}));
    }

    /// The first super administrator of the system tenant, as the actor a
    /// system-scoped step writes as. None when the tenant holds no such account.
    std::optional<step_actor> system_administrator() {
        auto sys_ctx = tenant_context::with_system_tenant(ctx_);
        ores::iam::service::account_service system_accounts(sys_ctx);
        boost::uuids::string_generator parse;
        for (const auto& id : super_admin_account_ids(sys_ctx)) {
            const auto account = system_accounts.get_account(parse(id));
            if (account)
                return actor_of(sys_ctx, *account);
        }
        return std::nullopt;
    }

    /// What one round of curve publishes did, a line for each curve.
    struct curve_publish_outcome {
        std::vector<std::string> published;
        std::vector<std::string> not_published;
    };

    /**
     * @brief Previews and publishes each curve, and asks again while its quotes have not arrived.
     *
     * A curve whose preview fails is asked again every ten seconds for two minutes, because its
     * feeds may not have ticked yet. A curve whose request waits for a person is not asked again,
     * since a second ask would raise a second request for one act.
     *
     * @param curves Each curve's bootstrap config id and name.
     * @param label Names the step in the logs.
     */
    curve_publish_outcome
    publish_curves(internal_request_client& client,
                   const std::vector<std::pair<std::string, std::string>>& curves,
                   std::string_view reason,
                   std::string_view label,
                   std::vector<ores::workflow::messaging::step_log_entry>& step_log) {
        constexpr auto curve_quote_patience = std::chrono::minutes{2};
        constexpr auto curve_retry_interval = std::chrono::seconds{10};

        std::map<std::string, std::string> pending;
        std::map<std::string, std::string> names;
        for (const auto& [id, name] : curves) {
            pending.emplace(id, "not yet asked");
            names.emplace(id, name);
        }

        curve_publish_outcome outcome;
        const auto started = std::chrono::steady_clock::now();
        const auto give_up = started + curve_quote_patience;
        while (!pending.empty()) {
            for (auto it = pending.begin(); it != pending.end();) {
                // The as-of moves with each attempt, so quotes a feed writes
                // while the step waits are inside the snapshot the next attempt
                // reads. A preview and its publish state the same instant.
                const auto as_of = std::chrono::system_clock::now();

                ores::marketdata::messaging::compute_curve_request preview;
                preview.bootstrap_config_id = it->first;
                preview.as_of = as_of;
                const auto computed = client.request(preview);
                if (!computed.success) {
                    it->second = computed.message;
                    ++it;
                    continue;
                }

                ores::marketdata::messaging::publish_curve_request publish;
                publish.bootstrap_config_id = it->first;
                publish.as_of = as_of;
                publish.reason = std::string(reason);
                const auto published = client.request(publish);
                if (published.success && published.published_at) {
                    BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                        << label << ": published " << names.at(it->first) << " at "
                        << ores::platform::time::datetime::to_iso8601_utc(*published.published_at)
                        << " by " << published.published_by << ", " << computed.points.size()
                        << " point(s)";
                    outcome.published.push_back(
                        names.at(it->first) + " as of " +
                        ores::platform::time::datetime::to_iso8601_utc(as_of));
                } else if (published.success) {
                    BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                        << label << ": " << names.at(it->first) << " waits for approval: request "
                        << published.approval_request_id << " is " << published.state_code;
                    outcome.not_published.push_back(
                        names.at(it->first) + ": the request " + published.approval_request_id +
                        " is " + published.state_code + " and no person has answered it");
                } else {
                    // The preview passed and the publish did not, which no wait
                    // puts right: the reason is reported and the curve is done.
                    BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                        << label << ": did not publish " << names.at(it->first) << ": "
                        << published.message;
                    outcome.not_published.push_back(names.at(it->first) + ": " + published.message);
                }
                it = pending.erase(it);
            }
            if (pending.empty() || std::chrono::steady_clock::now() >= give_up)
                break;
            const auto waited = std::chrono::duration_cast<std::chrono::seconds>(
                                    std::chrono::steady_clock::now() - started)
                                    .count();
            const auto waiting = std::to_string(pending.size()) + " curve(s) have no quotes yet (" +
                                 std::to_string(waited) + "s of " +
                                 std::to_string(curve_quote_patience.count() * 60) +
                                 "s), asking again in " +
                                 std::to_string(curve_retry_interval.count()) + "s";
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info) << label << ": " << waiting;
            step_log.push_back({.level = ores::workflow::messaging::step_log_level::info,
                                .message = waiting,
                                .context = {}});
            std::this_thread::sleep_for(curve_retry_interval);
        }

        for (const auto& [id, last_reason] : pending) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), warn)
                << label << ": did not publish " << names.at(id) << ": " << last_reason;
            outcome.not_published.push_back(names.at(id) + ": " + last_reason);
        }
        return outcome;
    }

    /**
     * @brief Bootstraps and publishes each IR curve the system tenant holds a
     * config for.
     *
     * The curves read the quotes of the synthetic feeds the system data starts,
     * and a feed's first quotes arrive some time after it starts, so a curve the
     * marketdata service cannot bootstrap yet is asked again every
     * curve_retry_interval until curve_quote_patience has passed. A curve still
     * refused then is named with its last reason, and the step completes: one
     * currency's missing quotes do not hold back the system's other curves or the
     * tenant's own setup. Each round of waiting is recorded in the step's own log,
     * so the run says why the step is taking the time it takes rather than looking
     * stalled.
     *
     * A curve is published and not merely republished: the publish goes through
     * marketdata.v1.ops.publish_curve, so the set is recorded against the request
     * that authorised it, and the published-curves list and the reads a surface or
     * a price take see it. The raise is the publish in the system tenant, which
     * is the installation, so no person has to answer first. Each attempt
     * previews the bootstrap with
     * marketdata.v1.ops.compute_curve and publishes only what that preview
     * accepted, so a retry costs no approval request.
     */
    void
    publish_system_curves_step(const ores::service::messaging::workflow_step_context& wf,
                               const ores::iam::workflow::provision_tenant_step_command& command) {
        constexpr std::string_view publish_reason =
            "The provisioning system run bootstraps the system's curves from the quotes the "
            "system's own feeds publish, so forwards can be read at once.";

        const auto admin = system_administrator();
        if (!admin) {
            wf.fail("The system tenant holds no administrator to publish its curves as.");
            return;
        }
        auto client = make_step_client(
            tenant_context::system_tenant_id, admin->account_id, admin->party_id, admin->username);

        ores::refdata::messaging::list_ir_curve_bootstrap_configs_request list;
        list.limit = 1000;
        const auto configs = client.request(list).bootstrap_configs;
        BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
            << "publish_system_curves: " << configs.size() << " curve config(s) to publish";

        std::vector<std::pair<std::string, std::string>> curves;
        for (const auto& config : configs)
            curves.emplace_back(boost::uuids::to_string(config.id), config.name);

        provision_step_result result;
        result.kind = command.kind;
        std::vector<ores::workflow::messaging::step_log_entry> step_log;
        auto outcome =
            publish_curves(client, curves, publish_reason, "publish_system_curves", step_log);
        result.curves = std::move(outcome.published);
        result.curves_not_published = std::move(outcome.not_published);

        const auto summary = std::string{"published "} + std::to_string(result.curves.size()) +
                             " of " + std::to_string(configs.size()) + " curve(s)";
        BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
            << "publish_system_curves: " << summary;
        for (const auto& not_published : result.curves_not_published)
            step_log.push_back({.level = ores::workflow::messaging::step_log_level::warn,
                                .message = not_published,
                                .context = {}});
        step_log.push_back({.level = ores::workflow::messaging::step_log_level::info,
                            .message = summary,
                            .context = {}});

        // A curve that was not published leaves the installation without the
        // forwards a surface reads, so the step says so in its own status rather
        // than passing as a clean run. The run still advances.
        if (result.curves_not_published.empty())
            wf.complete(ores::utility::codec::json::write(result), step_log);
        else
            wf.warn(ores::utility::codec::json::write(result), step_log);
    }

    /// The system tenant's curve blueprints, with every series they name.
    system_curve_blueprints read_system_curve_blueprints(internal_request_client& system_client) {
        system_curve_blueprints system;
        ores::refdata::messaging::list_ir_curve_bootstrap_configs_request configs;
        configs.limit = 1000;
        system.configs = system_client.request(configs).bootstrap_configs;
        if (system.configs.empty())
            return system;

        std::vector<boost::uuids::uuid> config_ids;
        std::vector<boost::uuids::uuid> series_ids;
        for (const auto& c : system.configs) {
            config_ids.push_back(c.id);
            series_ids.push_back(c.output_series_id);
        }
        ores::refdata::messaging::list_ir_curve_bootstrap_pillars_request pillars;
        pillars.limit = 5000;
        pillars.filter = ores::refdata::messaging::ir_curve_bootstrap_pillars_filter{
            .bootstrap_config_id = std::nullopt,
            .id_one_of = std::nullopt,
            .bootstrap_config_id_one_of = config_ids};
        system.pillars = system_client.request(pillars).pillars;
        for (const auto& p : system.pillars)
            series_ids.push_back(p.market_series_id);
        std::ranges::sort(series_ids);
        series_ids.erase(std::unique(series_ids.begin(), series_ids.end()), series_ids.end());

        ores::marketdata::messaging::list_market_series_request series;
        series.limit = 5000;
        series.filter =
            ores::marketdata::messaging::market_series_filter{.provider_code = std::nullopt,
                                                              .id_one_of = series_ids,
                                                              .provider_code_one_of = std::nullopt};
        system.series = system_client.request(series).market_series;
        return system;
    }

    /// The series and curve configs one party holds.
    party_curve_holdings read_party_curve_holdings(internal_request_client& party_client,
                                                   const boost::uuids::uuid& party_id) {
        party_curve_holdings holdings;
        holdings.party_id = party_id;
        ores::marketdata::messaging::list_market_series_request series;
        series.limit = 10000;
        holdings.series = party_client.request(series).market_series;

        ores::refdata::messaging::list_ir_curve_bootstrap_configs_request configs;
        configs.limit = 1000;
        for (const auto& c : party_client.request(configs).bootstrap_configs)
            if (c.party_id == party_id)
                holdings.config_ids.insert(c.id);
        return holdings;
    }

    /// Writes the rows a plan names, series first, because a config names its output series
    /// and a pillar names its config.
    static void write_party_curve_plan(internal_request_client& party_client,
                                       const party_curve_plan& plan,
                                       const std::string& party_name) {
        constexpr std::string_view commentary =
            "Created by provisioning: the party's own copy of the system's curve blueprint";
        if (!plan.series_to_write.empty()) {
            ores::marketdata::messaging::put_many_market_series_request put;
            for (const auto& s : plan.series_to_write)
                put.changes.push_back(
                    {.write = {.id = s.id,
                               .party_id = s.party_id,
                               .oresmd_uri = s.oresmd_uri,
                               .provider_code = s.provider_code,
                               .series_subclass = s.series_subclass,
                               .derivation_kind = s.derivation_kind,
                               .derivation_config_id = s.derivation_config_id,
                               .derivation_config_version = s.derivation_config_version},
                     .precondition = {.kind = ores::utility::domain::precondition_kind::any}});
            put.intent.reason_code = "system.new_record";
            put.intent.commentary = std::string(commentary);
            const auto saved = party_client.request(put);
            if (saved.result.outcome != ores::utility::domain::outcome::ok)
                throw std::runtime_error("The curve output series of '" + party_name +
                                         "' were not saved: " + saved.result.message);
        }
        if (!plan.configs_to_write.empty()) {
            ores::refdata::messaging::put_many_ir_curve_bootstrap_configs_request put;
            for (const auto& c : plan.configs_to_write)
                put.changes.push_back(
                    {.write = {.id = c.id,
                               .output_series_id = c.output_series_id,
                               .name = c.name,
                               .currency_code = c.currency_code,
                               .index_tenor = c.index_tenor,
                               .curve_family_role = c.curve_family_role,
                               .discount_curve_config_id = c.discount_curve_config_id,
                               .interpolation_method = c.interpolation_method,
                               .day_count_convention = c.day_count_convention,
                               .tenor_convention_code = c.tenor_convention_code,
                               .split_tenor_code = c.split_tenor_code},
                     .precondition = {.kind = ores::utility::domain::precondition_kind::any}});
            put.intent.reason_code = "system.new_record";
            put.intent.commentary = std::string(commentary);
            const auto saved = party_client.request(put);
            if (saved.result.outcome != ores::utility::domain::outcome::ok)
                throw std::runtime_error("The curve configs of '" + party_name +
                                         "' were not saved: " + saved.result.message);
        }
        if (!plan.pillars_to_write.empty()) {
            ores::refdata::messaging::put_many_ir_curve_bootstrap_pillars_request put;
            for (const auto& p : plan.pillars_to_write)
                put.changes.push_back(
                    {.write = {.id = p.id,
                               .party_id = p.party_id,
                               .bootstrap_config_id = p.bootstrap_config_id,
                               .sequence_index = p.sequence_index,
                               .start_tenor_code = p.start_tenor_code,
                               .end_tenor_code = p.end_tenor_code,
                               .curve_role_code = p.curve_role_code,
                               .market_series_id = p.market_series_id},
                     .precondition = {.kind = ores::utility::domain::precondition_kind::any}});
            put.intent.reason_code = "system.new_record";
            put.intent.commentary = std::string(commentary);
            const auto saved = party_client.request(put);
            if (saved.result.outcome != ores::utility::domain::outcome::ok)
                throw std::runtime_error("The curve pillars of '" + party_name +
                                         "' were not saved: " + saved.result.message);
        }
    }

    /// The bootstrap configs a party already has a current published curve for.
    static std::set<std::string> published_configs_of(internal_request_client& party_client,
                                                      const boost::uuids::uuid& party_id) {
        ores::marketdata::messaging::list_published_curves_request request;
        request.party_id = boost::uuids::to_string(party_id);
        request.limit = 1000;
        std::set<std::string> out;
        for (const auto& set : party_client.request(request).sets)
            out.insert(set.bootstrap_config_id);
        return out;
    }

    /**
     * @brief Gives each party the step names its own funding curves.
     *
     * A curve is derived data, so it belongs to the party that bootstraps it and not to the
     * system. The party copies every blueprint the system tenant holds, and each pillar reads
     * the party's own series of the same item from the same provider. The step waits up to three
     * minutes for a party's quotes to reach its series, then publishes each curve the party has
     * not published yet. Under the tenant's auto-approval the publish closes at once.
     *
     * A curve that does not publish fails the step, named with its party. A second run writes no
     * row twice and publishes only the curves that are still missing.
     */
    void
    publish_party_curves_step(const ores::service::messaging::workflow_step_context& wf,
                              const ores::iam::workflow::provision_tenant_step_command& command,
                              const step_actor& actor) {
        constexpr auto quote_patience = std::chrono::minutes{3};
        constexpr auto retry_interval = std::chrono::seconds{10};
        constexpr std::string_view publish_reason =
            "Provisioning bootstraps the party's own funding curve from the quotes its "
            "subscriptions deliver.";

        const auto parties = parse_party_curves_arguments(command.arguments_json);
        const auto admin = system_administrator();
        if (!admin) {
            wf.fail("The system tenant holds no administrator to read the curve blueprints as.");
            return;
        }
        auto system_client = make_step_client(
            tenant_context::system_tenant_id, admin->account_id, admin->party_id, admin->username);
        const auto system = read_system_curve_blueprints(system_client);
        if (system.configs.empty()) {
            wf.fail("The system tenant holds no curve blueprint to copy.");
            return;
        }

        auto discover =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);
        provision_step_result result;
        result.kind = command.kind;
        std::vector<ores::workflow::messaging::step_log_entry> step_log;
        for (const auto& party_name : parties) {
            const auto party = find_party(discover, party_name);
            if (!party)
                throw std::runtime_error("The tenant holds no party named '" + party_name + "'.");
            auto party_client =
                make_step_client(command.tenant_id, actor.account_id, party->id, actor.username);

            const auto started = std::chrono::steady_clock::now();
            auto plan =
                plan_party_curves(system, read_party_curve_holdings(party_client, party->id));
            while (!plan.waiting.empty() &&
                   std::chrono::steady_clock::now() < started + quote_patience) {
                const auto line = party_name + ": " + std::to_string(plan.waiting.size()) +
                                  " curve(s) wait for their quotes, asking again in " +
                                  std::to_string(retry_interval.count()) + "s";
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                    << "publish_party_curves: " << line;
                step_log.push_back({.level = ores::workflow::messaging::step_log_level::info,
                                    .message = line,
                                    .context = {}});
                std::this_thread::sleep_for(retry_interval);
                plan =
                    plan_party_curves(system, read_party_curve_holdings(party_client, party->id));
            }
            write_party_curve_plan(party_client, plan, party_name);
            for (const auto& waiting : plan.waiting)
                result.curves_not_published.push_back(party_name + ": " + waiting);

            const auto published = published_configs_of(party_client, party->id);
            std::vector<std::pair<std::string, std::string>> curves;
            for (const auto& curve : plan.ready) {
                const auto id = boost::uuids::to_string(curve.config_id);
                if (!published.contains(id))
                    curves.emplace_back(id, curve.name);
            }
            auto outcome = publish_curves(party_client,
                                          curves,
                                          publish_reason,
                                          "publish_party_curves " + party_name,
                                          step_log);
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "publish_party_curves: " << party_name << ": published "
                << outcome.published.size() << ", already published "
                << plan.ready.size() - curves.size() << ", not published "
                << outcome.not_published.size() + plan.waiting.size();
            for (auto& line : outcome.published)
                result.curves.push_back(party_name + ": " + line);
            for (auto& line : outcome.not_published)
                result.curves_not_published.push_back(party_name + ": " + line);
            result.parties.push_back(boost::uuids::to_string(party->id));
        }

        for (const auto& not_published : result.curves_not_published)
            step_log.push_back({.level = ores::workflow::messaging::step_log_level::warn,
                                .message = not_published,
                                .context = {}});
        if (!result.curves_not_published.empty()) {
            std::string reason = std::to_string(result.curves_not_published.size()) +
                                 " party curve(s) were not published. A rerun is safe: it "
                                 "writes no row twice and publishes only the missing curves:";
            for (const auto& line : result.curves_not_published)
                reason += " " + line + ";";
            wf.fail(reason);
            return;
        }
        wf.complete(ores::utility::codec::json::write(result), step_log);
    }

    /// The pair codes of every currency pair convention the tenant holds.
    static std::vector<std::string> pair_convention_codes(internal_request_client& client) {
        std::vector<std::string> out;
        ores::refdata::messaging::list_currency_pair_conventions_request request;
        request.limit = 1000;
        for (;;) {
            const auto page = client.request(request);
            for (const auto& c : page.conventions)
                out.push_back(c.pair_code);
            if (page.conventions.empty() || out.size() >= page.total)
                break;
            request.offset = static_cast<std::uint32_t>(out.size());
        }
        std::ranges::sort(out);
        return out;
    }

    /**
     * @brief Publishes each party's FX forward sets.
     *
     * A forward set is derived data, so it belongs to the party whose spot and funding curves it
     * reads. Each party is asked for every currency pair convention the tenant holds, and the
     * forward service decides which it can build: a pair it refuses before raising a request is
     * skipped, with its reason. Under the tenant's auto-approval the raise closes and the set
     * publishes at once.
     *
     * A set raised and not published fails the step, named with its party. A second run publishes
     * only the pairs that are still missing.
     */
    void
    publish_party_forwards_step(const ores::service::messaging::workflow_step_context& wf,
                                const ores::iam::workflow::provision_tenant_step_command& command,
                                const step_actor& actor) {
        constexpr std::string_view publish_reason =
            "Provisioning publishes the party's FX forward set from its spot and its published "
            "funding curves.";

        const auto parties = parse_party_forwards_arguments(command.arguments_json);
        auto discover =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);
        const auto pairs = pair_convention_codes(discover);
        if (pairs.empty()) {
            wf.fail("The tenant holds no currency pair convention to publish a forward set for.");
            return;
        }

        provision_step_result result;
        result.kind = command.kind;
        std::vector<ores::workflow::messaging::step_log_entry> step_log;
        for (const auto& party_name : parties) {
            const auto party = find_party(discover, party_name);
            if (!party)
                throw std::runtime_error("The tenant holds no party named '" + party_name + "'.");
            auto party_client =
                make_step_client(command.tenant_id, actor.account_id, party->id, actor.username);

            std::size_t published = 0;
            std::size_t held = 0;
            for (const auto& pair : pairs) {
                const auto as_of = std::chrono::system_clock::now();
                const auto line = party_name + ": " + pair;

                // The read answers the newest set at or before the as-of, so a set an
                // earlier run published is found by a later one.
                ores::marketdata::messaging::get_published_fx_forward_request read;
                read.pair_code = pair;
                read.as_of = as_of;
                if (party_client.request(read).success) {
                    ++held;
                    result.forwards_already_published.push_back(line);
                    continue;
                }

                ores::marketdata::messaging::publish_fx_forward_request publish;
                publish.pair_code = pair;
                publish.as_of = as_of;
                publish.reason = std::string(publish_reason);
                const auto answer = party_client.request(publish);
                if (answer.success && answer.published_at) {
                    ++published;
                    result.forwards.push_back(
                        line + ": as of " + ores::platform::time::datetime::to_iso8601_utc(as_of));
                } else if (answer.approval_request_id.empty()) {
                    result.forwards_skipped.push_back(line + ": " + answer.message);
                } else if (answer.success) {
                    result.forwards_not_published.push_back(
                        line + ": the request " + answer.approval_request_id + " is " +
                        answer.state_code + " and no person has answered it");
                } else {
                    result.forwards_not_published.push_back(line + ": " + answer.message);
                }
            }
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "publish_party_forwards: " << party_name << ": published " << published << " of "
                << pairs.size() << " pair(s)";
            // A refusal reads as a skip, so a service that refuses every pair for a reason of
            // its own, such as a permission, would otherwise pass the step with nothing published.
            if (published == 0 && held == 0)
                result.forwards_not_published.push_back(
                    party_name + ": no pair could be built, and the party holds no forward set");
            result.parties.push_back(boost::uuids::to_string(party->id));
        }

        for (const auto& skipped : result.forwards_skipped)
            step_log.push_back({.level = ores::workflow::messaging::step_log_level::info,
                                .message = "skipped " + skipped,
                                .context = {}});
        for (const auto& not_published : result.forwards_not_published)
            step_log.push_back({.level = ores::workflow::messaging::step_log_level::warn,
                                .message = not_published,
                                .context = {}});
        if (!result.forwards_not_published.empty()) {
            std::string reason = std::to_string(result.forwards_not_published.size()) +
                                 " forward set(s) were not published. A rerun is safe: it "
                                 "publishes only the missing pairs:";
            for (const auto& line : result.forwards_not_published)
                reason += " " + line + ";";
            wf.fail(reason);
            return;
        }
        wf.complete(ores::utility::codec::json::write(result), step_log);
    }

    /**
     * @brief Calibrates and publishes each party's volatility surfaces.
     *
     * A surface is derived data, so it belongs to the party whose blueprint, quotes and forward
     * sets it reads. Each blueprint the party holds is calibrated at the present instant and
     * published under the tenant's auto-approval.
     *
     * A surface with arbitrage, a fit that did not converge or a pillar beyond the tolerance is
     * held back and named: a decider would weigh it, and provisioning has none. A surface that
     * cannot be calibrated, or is raised and not published, fails the step, named with its party.
     * A second run publishes only the surfaces that are still missing.
     */
    void
    publish_party_surfaces_step(const ores::service::messaging::workflow_step_context& wf,
                                const ores::iam::workflow::provision_tenant_step_command& command,
                                const step_actor& actor) {
        constexpr std::string_view publish_reason =
            "Provisioning calibrates the party's surface from its volatility quotes and its "
            "published forward sets.";

        const auto parties = parse_party_surfaces_arguments(command.arguments_json);
        auto discover =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);

        provision_step_result result;
        result.kind = command.kind;
        std::vector<ores::workflow::messaging::step_log_entry> step_log;
        for (const auto& party_name : parties) {
            const auto party = find_party(discover, party_name);
            if (!party)
                throw std::runtime_error("The tenant holds no party named '" + party_name + "'.");
            auto party_client =
                make_step_client(command.tenant_id, actor.account_id, party->id, actor.username);

            ores::refdata::messaging::list_fx_vol_surface_blueprints_request list;
            list.limit = 1000;
            std::vector<ores::refdata::domain::fx_vol_surface_blueprint> blueprints;
            for (auto& b : party_client.request(list).blueprints)
                if (b.party_id == party->id)
                    blueprints.push_back(std::move(b));
            if (blueprints.empty()) {
                result.surfaces_not_published.push_back(party_name +
                                                        ": the party holds no surface blueprint");
                continue;
            }

            std::size_t published = 0;
            for (const auto& blueprint : blueprints) {
                const auto id = boost::uuids::to_string(blueprint.id);
                const auto as_of = std::chrono::system_clock::now();
                const auto line = party_name + ": " + blueprint.name;

                // The read answers the newest published surface at or before the as-of, so a
                // surface an earlier run published is found by a later one, even when a newer
                // publish has since superseded it: the blueprint has a surface either way.
                ores::marketdata::messaging::get_published_fx_vol_surface_request read;
                read.blueprint_id = id;
                read.as_of = as_of;
                if (party_client.request(read).success) {
                    result.surfaces_already_published.push_back(line);
                    continue;
                }

                ores::marketdata::messaging::calibrate_fx_vol_surface_request calibrate;
                calibrate.blueprint_id = id;
                calibrate.as_of = as_of;
                const auto calibrated = party_client.request(calibrate);
                if (!calibrated.success) {
                    result.surfaces_not_published.push_back(line + ": " + calibrated.message);
                    continue;
                }
                if (const auto reason = party_surface_hold_back(calibrated)) {
                    result.surfaces_held_back.push_back(line + ": " + *reason);
                    continue;
                }

                ores::marketdata::messaging::publish_fx_vol_surface_request publish;
                publish.blueprint_id = id;
                publish.as_of = calibrated.as_of;
                publish.reason = std::string(publish_reason);
                const auto answer = party_client.request(publish);
                if (answer.success && answer.published_at) {
                    ++published;
                    result.surfaces.push_back(
                        line + ": as of " +
                        ores::platform::time::datetime::to_iso8601_utc(calibrated.as_of) +
                        ", from forward set " + calibrated.forward_publish_id);
                } else if (answer.success) {
                    result.surfaces_not_published.push_back(
                        line + ": the request " + answer.approval_request_id + " is " +
                        answer.state_code + " and no person has answered it");
                } else {
                    result.surfaces_not_published.push_back(line + ": " + answer.message);
                }
            }
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "publish_party_surfaces: " << party_name << ": published " << published << " of "
                << blueprints.size() << " surface(s)";
            result.parties.push_back(boost::uuids::to_string(party->id));
        }

        for (const auto& held : result.surfaces_held_back)
            step_log.push_back({.level = ores::workflow::messaging::step_log_level::warn,
                                .message = "held back " + held,
                                .context = {}});
        for (const auto& not_published : result.surfaces_not_published)
            step_log.push_back({.level = ores::workflow::messaging::step_log_level::warn,
                                .message = not_published,
                                .context = {}});
        if (!result.surfaces_not_published.empty()) {
            std::string reason = std::to_string(result.surfaces_not_published.size()) +
                                 " surface(s) were not published. A rerun is safe: it publishes "
                                 "only the missing surfaces:";
            for (const auto& line : result.surfaces_not_published)
                reason += " " + line + ";";
            wf.fail(reason);
            return;
        }
        wf.complete(ores::utility::codec::json::write(result), step_log);
    }

    /// The system tenant's surface blueprints, with every series they name.
    system_surface_blueprints
    read_system_surface_blueprints(internal_request_client& system_client) {
        system_surface_blueprints system;
        ores::refdata::messaging::list_fx_vol_surface_blueprints_request blueprints;
        blueprints.limit = 1000;
        system.blueprints = system_client.request(blueprints).blueprints;
        if (system.blueprints.empty())
            return system;

        std::vector<boost::uuids::uuid> ids;
        std::vector<boost::uuids::uuid> series_ids;
        for (const auto& b : system.blueprints) {
            ids.push_back(b.id);
            series_ids.push_back(b.output_series_id);
        }
        ores::refdata::messaging::list_fx_vol_surface_pillars_request pillars;
        pillars.limit = 5000;
        pillars.filter = ores::refdata::messaging::fx_vol_surface_pillars_filter{
            .blueprint_id = std::nullopt, .id_one_of = std::nullopt, .blueprint_id_one_of = ids};
        system.pillars = system_client.request(pillars).pillars;

        std::vector<boost::uuids::uuid> pillar_ids;
        for (const auto& p : system.pillars)
            pillar_ids.push_back(p.id);
        for (std::size_t from = 0; from < pillar_ids.size(); from += 1000) {
            ores::refdata::messaging::list_fx_vol_surface_pillar_sources_request sources;
            sources.limit = 5000;
            sources.filter = ores::refdata::messaging::fx_vol_surface_pillar_sources_filter{
                .pillar_id = std::nullopt,
                .id_one_of = std::nullopt,
                .pillar_id_one_of = std::vector<boost::uuids::uuid>(
                    pillar_ids.begin() + static_cast<std::ptrdiff_t>(from),
                    pillar_ids.begin() +
                        static_cast<std::ptrdiff_t>(std::min(from + 1000, pillar_ids.size())))};
            for (auto& s : system_client.request(sources).pillar_sources)
                system.sources.push_back(std::move(s));
        }
        for (const auto& s : system.sources)
            series_ids.push_back(s.market_series_id);
        std::ranges::sort(series_ids);
        series_ids.erase(std::unique(series_ids.begin(), series_ids.end()), series_ids.end());

        ores::marketdata::messaging::list_market_series_request series;
        series.limit = 5000;
        series.filter =
            ores::marketdata::messaging::market_series_filter{.provider_code = std::nullopt,
                                                              .id_one_of = series_ids,
                                                              .provider_code_one_of = std::nullopt};
        system.series = system_client.request(series).market_series;
        return system;
    }

    /// The series and surface blueprints one party holds.
    party_surface_holdings read_party_surface_holdings(internal_request_client& party_client,
                                                       const boost::uuids::uuid& party_id) {
        party_surface_holdings holdings;
        holdings.party_id = party_id;
        ores::marketdata::messaging::list_market_series_request series;
        series.limit = 10000;
        holdings.series = party_client.request(series).market_series;
        ores::refdata::messaging::list_fx_vol_surface_blueprints_request blueprints;
        blueprints.limit = 1000;
        for (const auto& b : party_client.request(blueprints).blueprints)
            if (b.party_id == party_id)
                holdings.blueprint_ids.insert(b.id);
        return holdings;
    }

    /// Writes the rows a surface plan names: the output series, then the blueprints, their
    /// pillars and the pillars' sources, each of which names the row before it.
    static void write_party_surface_plan(internal_request_client& party_client,
                                         const party_surface_plan& plan,
                                         const std::string& party_name) {
        constexpr std::string_view commentary =
            "Created by provisioning: the party's own copy of the system's surface blueprint";
        const auto check = [&](const auto& saved, std::string_view what) {
            if (saved.result.outcome != ores::utility::domain::outcome::ok)
                throw std::runtime_error("The surface " + std::string(what) + " of '" + party_name +
                                         "' were not saved: " + saved.result.message);
        };
        const ores::utility::domain::precondition any{
            .kind = ores::utility::domain::precondition_kind::any};
        if (!plan.series_to_write.empty()) {
            ores::marketdata::messaging::put_many_market_series_request put;
            for (const auto& s : plan.series_to_write)
                put.changes.push_back(
                    {.write = {.id = s.id,
                               .party_id = s.party_id,
                               .oresmd_uri = s.oresmd_uri,
                               .provider_code = s.provider_code,
                               .series_subclass = s.series_subclass,
                               .derivation_kind = s.derivation_kind,
                               .derivation_config_id = s.derivation_config_id,
                               .derivation_config_version = s.derivation_config_version},
                     .precondition = any});
            put.intent.reason_code = "system.new_record";
            put.intent.commentary = std::string(commentary);
            check(party_client.request(put), "output series");
        }
        if (!plan.blueprints_to_write.empty()) {
            ores::refdata::messaging::put_many_fx_vol_surface_blueprints_request put;
            for (const auto& b : plan.blueprints_to_write)
                put.changes.push_back(
                    {.write = {.id = b.id,
                               .output_series_id = b.output_series_id,
                               .name = b.name,
                               .base_currency_code = b.base_currency_code,
                               .quote_currency_code = b.quote_currency_code,
                               .fx_option_convention_id = b.fx_option_convention_id,
                               .smile_model = b.smile_model,
                               .expiry_interpolation = b.expiry_interpolation,
                               .output_expiries = b.output_expiries,
                               .output_strikes = b.output_strikes},
                     .precondition = any});
            put.intent.reason_code = "system.new_record";
            put.intent.commentary = std::string(commentary);
            check(party_client.request(put), "blueprints");
        }
        if (!plan.pillars_to_write.empty()) {
            ores::refdata::messaging::put_many_fx_vol_surface_pillars_request put;
            for (const auto& p : plan.pillars_to_write)
                put.changes.push_back({.write = {.id = p.id,
                                                 .party_id = p.party_id,
                                                 .blueprint_id = p.blueprint_id,
                                                 .expiry_code = p.expiry_code,
                                                 .strike_label = p.strike_label},
                                       .precondition = any});
            put.intent.reason_code = "system.new_record";
            put.intent.commentary = std::string(commentary);
            check(party_client.request(put), "pillars");
        }
        if (!plan.sources_to_write.empty()) {
            ores::refdata::messaging::put_many_fx_vol_surface_pillar_sources_request put;
            for (const auto& s : plan.sources_to_write)
                put.changes.push_back({.write = {.id = s.id,
                                                 .party_id = s.party_id,
                                                 .pillar_id = s.pillar_id,
                                                 .market_series_id = s.market_series_id,
                                                 .contributor_code = s.contributor_code,
                                                 .weight = s.weight},
                                       .precondition = any});
            put.intent.reason_code = "system.new_record";
            put.intent.commentary = std::string(commentary);
            check(party_client.request(put), "pillar sources");
        }
    }

    /**
     * @brief Gives each party the step names its own FX volatility surface blueprints.
     *
     * A surface is derived data, so it belongs to the party that calibrates it. The party copies
     * every blueprint the system tenant holds, and each pillar source reads the party's own series
     * of the same item from the same provider, which its subscription to the derived source fills.
     * The step waits up to three minutes for those series, writes the rows, and fails naming each
     * blueprint still waiting. A second run writes no row twice.
     */
    void give_party_surface_blueprints_step(
        const ores::service::messaging::workflow_step_context& wf,
        const ores::iam::workflow::provision_tenant_step_command& command,
        const step_actor& actor) {
        constexpr auto quote_patience = std::chrono::minutes{3};
        constexpr auto retry_interval = std::chrono::seconds{10};

        const auto parties = parse_party_curves_arguments(command.arguments_json);
        const auto admin = system_administrator();
        if (!admin) {
            wf.fail("The system tenant holds no administrator to read the surface blueprints as.");
            return;
        }
        auto system_client = make_step_client(
            tenant_context::system_tenant_id, admin->account_id, admin->party_id, admin->username);
        const auto system = read_system_surface_blueprints(system_client);
        if (system.blueprints.empty()) {
            wf.fail("The system tenant holds no surface blueprint to copy.");
            return;
        }

        auto discover =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);
        provision_step_result result;
        result.kind = command.kind;
        std::vector<ores::workflow::messaging::step_log_entry> step_log;
        std::vector<std::string> waiting;
        for (const auto& party_name : parties) {
            const auto party = find_party(discover, party_name);
            if (!party)
                throw std::runtime_error("The tenant holds no party named '" + party_name + "'.");
            auto party_client =
                make_step_client(command.tenant_id, actor.account_id, party->id, actor.username);

            const auto started = std::chrono::steady_clock::now();
            auto plan =
                plan_party_surfaces(system, read_party_surface_holdings(party_client, party->id));
            while (!plan.waiting.empty() &&
                   std::chrono::steady_clock::now() < started + quote_patience) {
                const auto line = party_name + ": " + std::to_string(plan.waiting.size()) +
                                  " surface blueprint(s) wait for their quotes, asking again in " +
                                  std::to_string(retry_interval.count()) + "s";
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                    << "give_party_surface_blueprints: " << line;
                step_log.push_back({.level = ores::workflow::messaging::step_log_level::info,
                                    .message = line,
                                    .context = {}});
                std::this_thread::sleep_for(retry_interval);
                plan = plan_party_surfaces(system,
                                           read_party_surface_holdings(party_client, party->id));
            }
            write_party_surface_plan(party_client, plan, party_name);
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "give_party_surface_blueprints: " << party_name << ": "
                << plan.blueprints_to_write.size() << " written, " << plan.ready.size() << " held, "
                << plan.waiting.size() << " waiting";
            for (const auto& w : plan.waiting)
                waiting.push_back(party_name + ": " + w);
            result.parties.push_back(boost::uuids::to_string(party->id));
        }
        if (!waiting.empty()) {
            std::string reason = std::to_string(waiting.size()) +
                                 " surface blueprint(s) were not given. A rerun is safe:";
            for (const auto& line : waiting)
                reason += " " + line + ";";
            wf.fail(reason);
            return;
        }
        wf.complete(ores::utility::codec::json::write(result), step_log);
    }

    void set_auto_approval_step(const ores::service::messaging::workflow_step_context& wf,
                                const ores::iam::workflow::provision_tenant_step_command& command,
                                const step_actor& actor,
                                bool auto_approved,
                                const std::string& reason) {
        auto client =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);
        ores::inbox::messaging::set_approvals_mode_request request;
        request.auto_approved = auto_approved;
        request.reason = reason;
        const auto response = client.request(request);
        if (response.result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("The approvals mode was not changed: " +
                                     response.result.message);

        provision_step_result result;
        result.kind = command.kind;
        wf.complete(ores::utility::codec::json::write(result));
    }

    /// The last year generate_calendar_exceptions writes. Later days would be
    /// built as weekdays with no holidays.
    static constexpr int last_generated_holiday_year = 2060;

    /// Each calendar builds about 13,000 days, one row at a time, so the
    /// operation outlives the client's default patience.
    void build_calendars_step(const ores::service::messaging::workflow_step_context& wf,
                              const ores::iam::workflow::provision_tenant_step_command& command,
                              const step_actor& actor) {
        auto client =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);
        ores::refdata::messaging::regenerate_calendar_dates_request request;
        request.in_use_only = true;
        request.end_year = last_generated_holiday_year;
        const auto response = client.request(request, std::chrono::seconds{600});
        if (!response.success)
            throw std::runtime_error("The calendars were not built: " + response.message);

        BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
            << "build_calendars: wrote " << response.rows_written << " day(s)";
        provision_step_result result;
        result.kind = command.kind;
        wf.complete(ores::utility::codec::json::write(result));
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
        // a synthetic feeds dataset carries, for one — and those publications read the
        // party from here. The step acts for the tenant's system party, which
        // is where a shared stream belongs.
        if (!actor.party_id.is_nil())
            params.party_id = boost::uuids::to_string(actor.party_id);
        const auto params_json = dq::messaging::build_params_json(params);
        for (const auto& bundle_code : arguments.bundles)
            publish_bundle_or_throw(client, bundle_code, actor.username, params_json, command.kind);

        wf.complete(ores::utility::codec::json::write(
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
                ores::utility::codec::json::write(provision_step_result{
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

        wf.complete(ores::utility::codec::json::write(provision_step_result{
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

        wf.complete(ores::utility::codec::json::write(provision_step_result{
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

        wf.complete(ores::utility::codec::json::write(
            provision_step_result{.kind = command.kind, .parties = parties}));
    }

    /**
     * @brief Gives every staff account the password the starting point states.
     *
     * The accounts are published without credentials, so the person the tenant
     * is demonstrated to cannot sign in until this runs. Each password is set
     * through the account service's own reset-password operation, which
     * validates and hashes it exactly as it hashes any other password and gives
     * each account its own salt; no hash is computed here or stored in the
     * seed data.
     *
     * The run's own administrator is left alone: its password is the one the
     * caller stated on the provisioning form, and this step must not overwrite
     * the credential the operator chose. The account service refuses a password
     * equal to the one an account already holds, and that answer means the
     * account is already as the step wants it, so a repeated run leaves it
     * unchanged rather than failing.
     */
    void set_staff_passwords_step(const ores::service::messaging::workflow_step_context& wf,
                                  const ores::iam::workflow::provision_tenant_step_command& command,
                                  const step_actor& actor) {
        const auto arguments = parse_set_staff_passwords_arguments(command.arguments_json);
        auto client =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);

        iam::messaging::list_accounts_request accounts_req;
        accounts_req.limit = 10'000;
        const auto accounts = client.request(accounts_req).accounts;

        std::vector<std::string> account_ids;
        std::vector<std::string> usernames;
        for (const auto& account : accounts) {
            if (account.account_type != "user")
                continue;
            const auto account_id = boost::uuids::to_string(account.id);
            if (account_id == command.admin_account_id)
                continue;
            account_ids.push_back(account_id);
            usernames.push_back(account.username);
        }

        if (account_ids.empty()) {
            wf.complete(
                ores::utility::codec::json::write(provision_step_result{.kind = command.kind}));
            return;
        }

        iam::messaging::reset_password_request req;
        req.account_ids = account_ids;
        req.new_password = arguments.password;
        const auto resp = client.request(req);

        if (resp.results.size() != account_ids.size()) {
            throw std::runtime_error("The staff passwords were not set: the response named " +
                                     std::to_string(resp.results.size()) + " of " +
                                     std::to_string(account_ids.size()) + " accounts.");
        }

        std::vector<std::string> set;
        for (std::size_t i = 0; i < account_ids.size(); ++i) {
            if (resp.results[i].success ||
                resp.results[i].message == service::password_unchanged_message) {
                set.push_back(usernames[i]);
                continue;
            }
            throw std::runtime_error("The password of '" + usernames[i] +
                                     "' was not set: " + resp.results[i].message);
        }

        wf.complete(ores::utility::codec::json::write(
            provision_step_result{.kind = command.kind, .accounts = set}));
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

        wf.complete(ores::utility::codec::json::write(
            provision_step_result{.kind = command.kind, .parties = parties, .images = images}));
    }

    /// Gives each party the tenant holds a subscription to each of its providers and a route to
    /// one source of each: the step's own, or the ones the step names for that party.
    ///
    /// A demonstration's legal entities are meant to take their market data from different vendors,
    /// because each collection quotes the markets its own feeds serve, so the profile names a
    /// provider and a source for each party it wants on another. A party the profile names twice
    /// takes both providers, as a bank that reads rates from one vendor and crosses from another
    /// does. A party the profile does not name takes the step's own, and a party it names that the
    /// tenant does not hold is refused: a subscription written for nobody would leave the tenant
    /// quietly on the wrong feed.
    ///
    /// The generators belong to the system, so the tenant publishes none and starts none: the
    /// parties take the shared simulated stream through their subscriptions and routes.
    void
    subscribe_market_data_step(const ores::service::messaging::workflow_step_context& wf,
                               const ores::iam::workflow::provision_tenant_step_command& command,
                               const step_actor& actor) {
        const auto arguments = parse_market_data_arguments(command.arguments_json);
        auto discover =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);

        // One lookup for each provider and source named, so a party on another feed costs no more
        // reads than the parties on the step's own.
        std::map<std::string, boost::uuids::uuid> sources;
        const auto source_of = [&](const std::string& provider, const std::string& source) {
            const auto key = provider + "/" + source;
            const auto found = sources.find(key);
            if (found != sources.end())
                return found->second;
            const auto id = find_source_id(discover, provider, source);
            sources.emplace(key, id);
            return id;
        };
        // The step's own source is looked up first, so a profile that names a wrong one fails
        // before any party is subscribed.
        source_of(arguments.provider, arguments.source);

        std::vector<std::string> subscribed;
        std::set<std::string> matched;
        std::uint32_t offset = 0;
        constexpr std::uint32_t page_size = 1000;
        while (true) {
            ores::refdata::messaging::list_parties_request request;
            request.offset = offset;
            request.limit = page_size;
            const auto page = discover.request(request).parties;
            for (const auto& party : page) {
                const auto id_text = boost::uuids::to_string(party.id);
                for (const auto& assignment : arguments.parties)
                    if (assignment.party == id_text || assignment.party == party.full_name)
                        matched.insert(assignment.party);
                auto party_client =
                    make_step_client(command.tenant_id, actor.account_id, party.id, actor.username);
                for (const auto& feed : feeds_of_party(arguments, id_text, party.full_name))
                    subscribe_party(party_client,
                                    party.id,
                                    feed.provider,
                                    source_of(feed.provider, feed.source));
                subscribed.push_back(id_text);
            }
            if (page.size() < page_size)
                break;
            offset += page_size;
        }

        for (const auto& assignment : arguments.parties)
            if (!matched.contains(assignment.party))
                throw std::runtime_error("The tenant holds no party the market data step names: '" +
                                         assignment.party + "'.");

        wf.complete(ores::utility::codec::json::write(
            provision_step_result{.kind = command.kind, .parties = subscribed}));
    }

    /// A client that speaks as one party over the connection this handler
    /// already holds.
    ///
    /// The generation writes through the trading operation, and that operation
    /// takes the instrument's party from the token rather than from the row, so
    /// the population reaches the books of the party the config books for and of
    /// no other. The token is minted once and re-minted when the server rejects
    /// it as expired, which is the contract a service-path client states.
    ores::nats::service::nats_client make_generation_client(const std::string& tenant_id,
                                                            const boost::uuids::uuid& account_id,
                                                            const boost::uuids::uuid& party_id,
                                                            const std::string& username) {
        auto token = std::make_shared<std::string>(
            impersonation_.mint_token(ctx_, tenant_id, account_id, party_id, username));
        return ores::nats::service::nats_client(
            nats_, [this, token, tenant_id, account_id, party_id, username](bool force) {
                if (force)
                    *token =
                        impersonation_.mint_token(ctx_, tenant_id, account_id, party_id, username);
                return *token;
            });
    }

    /**
     * @brief A client that books in the session of the person a slot names.
     *
     * The population is entered by the party's own traders and not by the run: a booking writes the
     * person who entered it, and the authorisation request it raises belongs to that person. The
     * token is minted for whoever @p acting holds at the moment of the request, and re-minted when
     * the slot hands the run to another trader, so one client books a whole population in as many
     * sessions as it has traders.
     */
    ores::nats::service::nats_client
    make_trader_client(const std::string& tenant_id,
                       const boost::uuids::uuid& party_id,
                       std::shared_ptr<ores::synthetic::domain::trade_population_trader> acting) {
        struct minted final {
            boost::uuids::uuid account_id = {};
            std::string username = {};
            std::string token = {};
        };
        auto held = std::make_shared<minted>();
        return ores::nats::service::nats_client(
            nats_, [this, tenant_id, party_id, acting, held](bool force) {
                const auto stale = force || held->token.empty() ||
                                   held->account_id != acting->account_id ||
                                   held->username != acting->username;
                if (stale) {
                    held->account_id = acting->account_id;
                    held->username = acting->username;
                    held->token = impersonation_.mint_token(
                        ctx_, tenant_id, acting->account_id, party_id, acting->username);
                }
                return held->token;
            });
    }

    /// The book the step names, among the party's own.
    ///
    /// The book is resolved by the name the party's own reference data gives
    /// it, so a recipe names no identifier that only one environment holds and
    /// a book the party does not have fails here rather than at the booking
    /// trigger, which would report it as the trade's fault.
    static std::optional<ores::refdata::domain::book>
    find_party_book(internal_request_client& client,
                    const boost::uuids::uuid& party_id,
                    const std::string& name) {
        ores::refdata::messaging::list_books_request request;
        request.limit = 10000;
        const auto response = client.request(request);
        require_read(response.result, "the books of the tenant");
        for (const auto& book : response.books)
            if (book.party_id == party_id && book.name == name)
                return book;
        return std::nullopt;
    }

    /// The counterparties a party may face, each once and in a stable order.
    ///
    /// The links are the set a population should face: a counterparty outside
    /// it inserts, because nothing in the schema checks the pair, and a trade
    /// against one is wrong rather than merely unusual. The read is the
    /// party's own and carries no filter, because this component refuses a
    /// filtered read of the table rather than answering it.
    static std::vector<boost::uuids::uuid>
    party_counterparties(internal_request_client& client, const boost::uuids::uuid& party_id) {
        ores::refdata::messaging::list_by_party_id_party_counterparties_request request;
        request.party_id = party_id;
        request.limit = 10000;

        const auto response = client.request(request);
        require_read(response.result,
                     "the counterparty links of party " + boost::uuids::to_string(party_id));

        std::vector<boost::uuids::uuid> result;
        for (const auto& link : response.party_counterparties)
            result.push_back(link.counterparty_id);
        std::ranges::sort(result);
        const auto duplicates = std::ranges::unique(result);
        result.erase(duplicates.begin(), result.end());
        return result;
    }

    /// The counterparties a config already scopes, in a stable order.
    static std::vector<boost::uuids::uuid>
    scoped_counterparties(internal_request_client& client, const boost::uuids::uuid& config_id) {
        ores::synthetic::messaging::list_trade_generation_counterparties_request request;
        request.limit = 10000;
        request.filter = ores::synthetic::messaging::trade_generation_counterparties_filter{
            .config_id = config_id};

        const auto response = client.request(request);
        require_read(response.result, "the scope of config " + boost::uuids::to_string(config_id));

        std::vector<boost::uuids::uuid> result;
        for (const auto& row : response.trade_generation_counterparties)
            result.push_back(row.counterparty_id);
        std::ranges::sort(result);
        return result;
    }

    /// Whether a config already lands its population in the book named.
    static bool book_is_scoped(internal_request_client& client,
                               const boost::uuids::uuid& config_id,
                               const boost::uuids::uuid& book_id) {
        ores::synthetic::messaging::list_trade_generation_books_request request;
        request.limit = 1000;
        request.filter =
            ores::synthetic::messaging::trade_generation_books_filter{.config_id = config_id};
        const auto response = client.request(request);
        require_read(response.result,
                     "the book scope of config " + boost::uuids::to_string(config_id));
        for (const auto& row : response.trade_generation_books)
            if (row.book_id == book_id)
                return true;
        return false;
    }

    /// The config the step's party already holds under the name the entry
    /// states, so a second run reuses the recipe rather than recording a
    /// second one.
    static std::optional<ores::synthetic::domain::trade_generation_config>
    find_trade_generation_config(internal_request_client& client, const std::string& name) {
        ores::synthetic::messaging::list_trade_generation_configs_request request;
        request.limit = 1000;
        const auto response = client.request(request);
        require_read(response.result, "the trade generation configs of the party");
        for (const auto& config : response.trade_generation_configs)
            if (config.name == name)
                return config;
        return std::nullopt;
    }

    /// A read a step cannot act without, refused loudly.
    ///
    /// The store reports a refusal in the response and not as a transport
    /// error, so a step that read the refusal as an empty answer would record
    /// a config that scopes nobody and write nothing against it.
    static void require_read(const ores::utility::domain::result& result, const std::string& what) {
        if (result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("Reading " + what + " failed: " + result.message);
    }

    static void require_recorded(const ores::utility::domain::result& result,
                                 const std::string& what) {
        if (result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("Recording " + what + " failed: " + result.message);
    }

    /// The day the population is anchored on. An entry that states a date pins
    /// the population to it; an entry that states none anchors on the day the
    /// step runs, because the population is meant to sit relative to the
    /// present rather than to the day the recipe was written.
    static std::chrono::year_month_day
    trade_anchor_date(const trade_generation_assignment& assignment) {
        if (assignment.as_of_date)
            return *assignment.as_of_date;
        return std::chrono::year_month_day(
            std::chrono::floor<std::chrono::days>(std::chrono::system_clock::now()));
    }

    /// Records one entry's config against the party it books for, and returns
    /// the id it carries. A config the party already holds under that name is
    /// left as it stands, so re-provisioning writes no version and a
    /// population that has already run keeps the anchor it ran on.
    boost::uuids::uuid record_trade_generation_config(internal_request_client& client,
                                                      const boost::uuids::uuid& party_id,
                                                      const trade_generation_assignment& entry,
                                                      const std::string& reference_curve_id) {
        if (const auto existing = find_trade_generation_config(client, entry.name)) {
            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "generate_trades: the config '" << entry.name << "' is already recorded.";
            return existing->id;
        }

        const auto config_id = ores::synthetic::service::config_id_for(party_id, entry.name);
        ores::synthetic::messaging::put_trade_generation_config_request request;
        request.change.write.id = config_id;
        request.change.write.name = entry.name;
        request.change.write.description =
            entry.description.empty() ? entry.name : entry.description;
        request.change.write.asset_class_code = entry.asset_class_code;
        request.change.write.min_tenor_code = entry.min_tenor_code;
        request.change.write.max_tenor_code = entry.max_tenor_code;
        request.change.write.min_notional = *entry.min_notional;
        request.change.write.max_notional = *entry.max_notional;
        request.change.write.as_of_date = trade_anchor_date(entry);
        request.change.write.seed = *entry.seed;
        request.change.write.trades_per_book = entry.trades_per_book.value_or(8);
        request.change.write.structures_per_book = entry.structures_per_book.value_or(2);
        request.change.write.enabled = true;
        request.change.write.reference_curve_id = reference_curve_id;
        request.change.precondition = {ores::utility::domain::precondition_kind::must_not_exist,
                                       std::nullopt};
        request.intent.reason_code = "system.initial_load";
        request.intent.commentary = "Recorded by the tenant's seed profile.";

        const auto response = client.request(request);
        require_recorded(response.result, "the trade generation config '" + entry.name + "'");
        return config_id;
    }

    /// The curve a party's book is discounted on, as the oresmd URI of the
    /// market series the party's own curve configuration published for the
    /// book's currency.
    ///
    /// The curve belongs to the market, so the config names the series the
    /// party holds rather than spelling ORE's name for it: ORE's own name for
    /// one index differs between the reference data and the market's
    /// forwarding, and only the latter is what the engine resolves. A party
    /// with no curve for the book's currency gets an empty name, and the
    /// population's bonds then state none and are refused by the engine's
    /// builder rather than priced on another party's curve.
    std::string find_reference_curve_id(internal_request_client& client,
                                        const boost::uuids::uuid& party_id,
                                        const std::string& currency) {
        if (currency.empty())
            return {};

        ores::marketdata::messaging::list_market_series_request request;
        request.limit = 10000;
        const auto response = client.request(request);
        require_read(response.result, "the market series of the tenant");

        const auto prefix = "oresmd://ir/" + currency + "?type=series&instrument=";
        for (const auto& s : response.market_series) {
            if (s.party_id != party_id)
                continue;
            if (!s.oresmd_uri.starts_with(prefix))
                continue;
            if (s.oresmd_uri.find("&curve_id=") == std::string::npos)
                continue;
            return s.oresmd_uri;
        }
        return {};
    }

    /// Records the trade types the entry states, each once. A type already
    /// recorded is left alone, so the step stays re-runnable.
    void record_trade_types(internal_request_client& client,
                            const boost::uuids::uuid& config_id,
                            const trade_generation_assignment& entry) {
        if (entry.trade_type_codes.empty())
            return;

        ores::synthetic::messaging::list_trade_generation_trade_types_request list;
        list.filter =
            ores::synthetic::messaging::trade_generation_trade_types_filter{.config_id = config_id};
        list.limit = 1000;
        const auto listed = client.request(list);
        require_read(listed.result, "the trade types of '" + entry.name + "'");

        ores::synthetic::messaging::put_many_trade_generation_trade_types_request request;
        request.intent.reason_code = "system.initial_load";
        request.intent.commentary = "Trade types of the population.";
        for (const auto& code : entry.trade_type_codes) {
            const auto recorded =
                std::ranges::any_of(listed.trade_generation_trade_types,
                                    [&](const auto& row) { return row.trade_type_code == code; });
            if (recorded)
                continue;
            ores::synthetic::messaging::trade_generation_trade_type_change change;
            change.write.id = ores::synthetic::service::trade_type_scope_id_for(config_id, code);
            change.write.config_id = config_id;
            change.write.trade_type_code = code;
            change.precondition = {ores::utility::domain::precondition_kind::must_not_exist,
                                   std::nullopt};
            request.changes.push_back(std::move(change));
        }
        if (!request.changes.empty())
            require_recorded(client.request(request).result,
                             "the trade types of '" + entry.name + "'");
    }

    /// Records the scope the entry describes: the party's book of that name,
    /// and every counterparty the party may face. A row that is already there
    /// is left alone, so the step is re-runnable and a scope that grew between
    /// two runs gains only what it gained.
    void record_trade_generation_scope(internal_request_client& client,
                                       const boost::uuids::uuid& config_id,
                                       const boost::uuids::uuid& party_id,
                                       const trade_generation_assignment& entry) {
        const auto book = find_party_book(client, party_id, entry.book);
        if (!book)
            throw std::runtime_error("The party holds no book named '" + entry.book +
                                     "', so the population has nowhere to land.");

        ores::synthetic::messaging::put_many_trade_generation_books_request book_request;
        ores::synthetic::messaging::trade_generation_book_change book_change;
        book_change.write.id = ores::synthetic::service::book_scope_id_for(config_id, book->id);
        book_change.write.config_id = config_id;
        book_change.write.book_id = book->id;
        book_change.precondition = {ores::utility::domain::precondition_kind::must_not_exist,
                                    std::nullopt};
        book_request.changes.push_back(book_change);
        book_request.intent.reason_code = "system.initial_load";
        book_request.intent.commentary = "Book scope of the population.";

        if (!book_is_scoped(client, config_id, book->id))
            require_recorded(client.request(book_request).result,
                             "the book scope of '" + entry.name + "'");

        record_trade_types(client, config_id, entry);

        const auto scoped = scoped_counterparties(client, config_id);
        std::vector<boost::uuids::uuid> missing;
        for (const auto& counterparty_id : party_counterparties(client, party_id))
            if (!std::ranges::binary_search(scoped, counterparty_id))
                missing.push_back(counterparty_id);
        if (missing.empty())
            return;

        ores::synthetic::messaging::put_many_trade_generation_counterparties_request request;
        request.intent.reason_code = "system.initial_load";
        request.intent.commentary = "Counterparty scope of the population.";
        for (const auto& counterparty_id : missing) {
            ores::synthetic::messaging::trade_generation_counterparty_change change;
            change.write.id =
                ores::synthetic::service::counterparty_scope_id_for(config_id, counterparty_id);
            change.write.config_id = config_id;
            change.write.counterparty_id = counterparty_id;
            change.precondition = {ores::utility::domain::precondition_kind::must_not_exist,
                                   std::nullopt};
            request.changes.push_back(std::move(change));
        }
        require_recorded(client.request(request).result,
                         "the counterparty scope of '" + entry.name + "'");
    }

    /// Records a trade generation config for each party the step names, and
    /// writes the population each one describes into that party's own books.
    ///
    /// The step owns the config because the config cannot exist before the step
    /// runs: its book scope and its counterparty scope are rows of the tenant
    /// that the earlier steps publish, so a recipe stated anywhere else would
    /// name identifiers no environment holds yet. An entry therefore states a
    /// party by the legal name the LEI import created, a book by the name that
    /// party's own reference data gives it, and leaves the counterparties to the
    /// links the party actually has.
    ///
    /// The writes go through the trading operation as the party that books
    /// them, because that operation takes the instrument's party from the token
    /// rather than from the row.
    void generate_trades_step(const ores::service::messaging::workflow_step_context& wf,
                              const ores::iam::workflow::provision_tenant_step_command& command,
                              const step_actor& actor) {
        auto assignments = parse_trade_generation_assignments(command.arguments_json);

        /*
         * A run may state the population it wants rather than take the counts
         * its recipe states: a total and a percentage per class, which become
         * the count each config books. The floor is the codes the class's
         * planner cycles, because a config shorter than its code list states a
         * prefix and drops the rest.
         */
        apply_population_knobs(assignments,
                               read_population_knobs(command.parameters),
                               [](const std::string& asset_class) {
                                   return ores::synthetic::service::routed_code_count(asset_class);
                               });
        auto discover =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);

        provision_step_result result;
        result.kind = command.kind;

        const auto seconds_since = [](std::chrono::steady_clock::time_point from) {
            return std::chrono::duration<double>(std::chrono::steady_clock::now() - from).count();
        };
        BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
            << "generate_trades: " << assignments.size() << " entr(y/ies) to write";
        std::size_t position = 0;
        for (const auto& entry : assignments) {
            ++position;
            const auto entry_started = std::chrono::steady_clock::now();
            const auto phase = [&](std::string_view what) {
                BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                    << "generate_trades: " << entry.name << " (" << position << " of "
                    << assignments.size() << "): " << what << " after "
                    << seconds_since(entry_started) << "s";
            };
            phase("finding the party " + entry.party);
            const auto party = find_party(discover, entry.party);
            if (!party)
                throw std::runtime_error("The tenant holds no party named '" + entry.party + "'.");

            auto party_client =
                make_step_client(command.tenant_id, actor.account_id, party->id, actor.username);

            /*
             * The population is kept in the book's own currency, so the curve
             * its bonds are discounted on is resolved from that same book. The
             * curve configuration the party published is what the resolution
             * reads, so the config names a series the party actually holds.
             */
            phase("finding the book " + entry.book);
            const auto book = find_party_book(party_client, party->id, entry.book);
            if (!book)
                throw std::runtime_error("The party holds no book named '" + entry.book +
                                         "', so the population has nowhere to land.");
            phase("finding the reference curve in " + book->functional_currency);
            const auto reference_curve_id =
                find_reference_curve_id(party_client, party->id, book->functional_currency);

            phase("recording the trade generation config");
            const auto config_id =
                record_trade_generation_config(party_client, party->id, entry, reference_curve_id);
            phase("recording the config's scope");
            record_trade_generation_scope(party_client, config_id, party->id, entry);

            auto generation_client = make_generation_client(
                command.tenant_id, actor.account_id, party->id, actor.username);

            /*
             * Each trade is booked by the person the slot names, which is one of the party's own
             * traders: the trade states them as its author and the authorisation request it raises
             * is theirs, so a person of the party finds the requests of the trades they entered.
             */
            auto acting = std::make_shared<ores::synthetic::domain::trade_population_trader>();
            auto booker = std::make_shared<ores::nats::service::nats_client>(
                make_trader_client(command.tenant_id, party->id, acting));
            ores::synthetic::service::trading_trade_population_writer writer(
                generation_client,
                [booker, acting](const ores::synthetic::domain::trade_population_slot& slot)
                    -> ores::nats::service::nats_client& {
                    *acting = slot.trader;
                    return *booker;
                },
                ores::synthetic::domain::trade_population_trader{actor.account_id, actor.username});
            ores::synthetic::service::trade_population_service service(writer);
            phase("writing the population of config " + boost::uuids::to_string(config_id));
            const auto run = service.run(config_id);

            result.configs.push_back(boost::uuids::to_string(config_id));
            result.parties.push_back(boost::uuids::to_string(party->id));
            result.trades_planned += run.planned;
            result.trades_written += run.written;
            result.trades_already_present += run.already_present;

            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "generate_trades: " << entry.name << " for party " << entry.party << ": planned "
                << run.planned << ", wrote " << run.written << ", already present "
                << run.already_present << ", in " << seconds_since(entry_started) << "s";
        }

        BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
            << "generate_trades: every entry is written (" << result.trades_written << " written, "
            << result.trades_already_present << " already present); completing the step";
        wf.complete(ores::utility::codec::json::write(result));
    }

    /// Every definition a party owns, one page at a time, as the party reads
    /// them. A single page with a fixed limit would schedule the first hundred
    /// and leave the rest of the catalogue unscheduled without saying so.
    static std::vector<ores::reporting::domain::report_definition>
    list_party_report_definitions(internal_request_client& client,
                                  const boost::uuids::uuid& party_id) {
        std::vector<ores::reporting::domain::report_definition> definitions;
        std::uint32_t offset = 0;
        constexpr std::uint32_t page_size = 100;
        while (true) {
            ores::reporting::messaging::list_report_definitions_request request;
            request.offset = offset;
            request.limit = page_size;
            request.filter =
                ores::reporting::messaging::report_definitions_filter{.party_id = party_id};
            const auto page = client.request(request).definitions;
            for (const auto& definition : page)
                definitions.push_back(definition);
            if (page.size() < page_size)
                return definitions;
            offset += page_size;
        }
    }

    /**
     * @brief Schedules the report definitions each party the step names
     * publishes.
     *
     * A provisioned tenant ticks nothing on its own: the catalogue states what
     * each report's schedule is, but a run needs a scheduler job and a run
     * grant, and only the reporting schedule operation creates the pair. So the
     * run that builds the tenant also puts its catalogue on the scheduler,
     * acting as the run's administrator inside each party, which is the
     * identity the operation makes the grant for.
     *
     * Three rules keep that honest. A report the catalogue states as on demand
     * is left to be asked for by hand, and the step never fails because of it:
     * asking for a report a person asks for is not a provisioning fault. A
     * definition that already holds a scheduler job is passed over, so a second
     * provision creates no second job and no second grant. And a definition
     * whose schedule is not a cron is passed over with a warning, because the
     * tenant should still come up.
     */
    void schedule_reports_step(const ores::service::messaging::workflow_step_context& wf,
                               const ores::iam::workflow::provision_tenant_step_command& command,
                               const step_actor& actor) {
        const auto arguments = parse_report_schedule_arguments(command.arguments_json);
        auto discover =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);

        provision_step_result result;
        result.kind = command.kind;

        for (const auto& party_name : arguments.parties) {
            const auto party = find_party(discover, party_name);
            if (!party)
                throw std::runtime_error("The tenant holds no party named '" + party_name + "'.");

            // The operation makes the run grant for the session's party, and
            // refuses a definition of another one, so the step acts as the
            // party whose catalogue it schedules.
            auto party_client =
                make_step_client(command.tenant_id, actor.account_id, party->id, actor.username);

            std::vector<ores::iam::messaging::report_schedule_candidate> candidates;
            for (const auto& definition : list_party_report_definitions(party_client, party->id))
                candidates.push_back(
                    {.id = boost::uuids::to_string(definition.id),
                     .schedule_expression = definition.schedule_expression,
                     .holds_scheduler_job = definition.scheduler_job_id.has_value()});

            const auto selection =
                ores::iam::messaging::select_report_definitions_to_schedule(candidates);

            provision_step_result::party_report_schedule schedule;
            schedule.party = party_name;
            schedule.already_scheduled = selection.already_scheduled;
            schedule.on_demand = selection.on_demand;
            schedule.not_a_cron = selection.not_a_cron;

            if (!selection.ids.empty()) {
                ores::reporting::messaging::schedule_report_definitions_request request;
                request.ids = selection.ids;
                // One call carries a party's whole catalogue; each definition
                // costs a run grant, a scheduler write and a definition save,
                // so the operation outlives the client's default patience.
                const auto response = party_client.request(request, std::chrono::seconds{600});
                if (!response.success)
                    throw std::runtime_error("The reporting service refused to schedule " +
                                             std::to_string(response.failed_ids.size()) +
                                             " report definitions of '" + party_name +
                                             "': " + response.message);
                schedule.scheduled = static_cast<std::uint32_t>(response.scheduled_count);
            }

            BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
                << "schedule_reports: " << party_name << ": scheduled " << schedule.scheduled
                << ", already scheduled " << schedule.already_scheduled << ", on demand "
                << schedule.on_demand << ", not a cron " << schedule.not_a_cron;
            result.parties.push_back(boost::uuids::to_string(party->id));
            result.report_schedules.push_back(std::move(schedule));
        }

        wf.complete(ores::utility::codec::json::write(result));
    }

    /**
     * @brief Hands the tenant to its own administrator by starting the run that
     * finishes it.
     *
     * This is how one run hands work to another. The system-side run is owned
     * by the caller's tenant and its earlier steps act in the system tenant, so
     * it cannot act inside the tenant it creates; the tenant's own run is owned
     * by the new tenant and acts as the administrator the synchronous phase
     * created, which is the identity the tenant's own steps need and the scope
     * the tenant's own administrator can follow. The step therefore does no
     * work itself: it starts that run with the same request this run received,
     * and reports the child's instance id so the handover is visible in the
     * parent's step result.
     */
    void start_tenant_setup_step(const ores::service::messaging::workflow_step_context& wf,
                                 const ores::iam::workflow::provision_tenant_step_command& command,
                                 const std::string& correlation_id) {
        // The step's argument is the whole run request, so the child works from
        // exactly what this run received rather than from a request composed
        // again here.
        const auto run = ores::iam::workflow::detail::read_workflow_request(command.arguments_json);
        const auto actor = resolve_step_actor(command);

        boost::uuids::random_generator generate;
        const auto instance_id = boost::uuids::to_string(generate());

        ores::workflow::messaging::start_workflow_message start;
        start.type = std::string(ores::iam::workflow::tenant_setup_workflow_type);
        // The child belongs to the new tenant, not to the run that starts it, so
        // the tenant's own administrator can follow it and every step it
        // dispatches is scoped to that tenant.
        start.tenant_id = run.tenant_id;
        start.target_kind = std::string(ores::iam::workflow::provision_tenant_target_kind);
        start.target_id = run.tenant_id;
        start.request_json = command.arguments_json;
        start.correlation_id = correlation_id;
        start.instance_id = instance_id;

        // The start message requires a session, so it is published with a token
        // minted for the new tenant's administrator, which is the identity the
        // child's own steps then act as.
        auto client =
            make_step_client(command.tenant_id, actor.account_id, actor.party_id, actor.username);
        client.publish(ores::workflow::messaging::start_workflow_message::nats_subject,
                       start,
                       start.instance_id);

        BOOST_LOG_SEV(tenant_provisioning_handler_lg(), info)
            << "Started " << start.type << " for tenant " << run.tenant_code
            << " (instance: " << instance_id << ")";

        wf.complete(ores::utility::codec::json::write(provision_step_result{
            .kind = command.kind, .tenant_id = run.tenant_id, .child_instance_id = instance_id}));
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
        ores::database::query_builder::execute_parameterized_command(
            sys_ctx,
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

        const auto result = ores::utility::codec::json::write(
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

    // The id of the source the step names, which must belong to the provider.
    static boost::uuids::uuid find_source_id(internal_request_client& client,
                                             const std::string& provider,
                                             const std::string& source) {
        marketdata::messaging::list_feeds_request request;
        request.limit = 1000;
        const auto response = client.request(request);
        if (response.result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("The market data feeds were not read: " +
                                     response.result.message);
        for (const auto& candidate : response.feeds)
            if (candidate.name == source && candidate.publisher_code == provider)
                return candidate.id;
        throw std::runtime_error("The catalogue holds no feed '" + source + "' of the provider '" +
                                 provider + "'.");
    }

    // Gives one party a subscription to a provider and a route to a source. The
    // client speaks as the party, because the service stamps the party from the
    // session. A subscription or a route the party already holds is left as it is,
    // so the step can run again.
    static void subscribe_party(internal_request_client& party_client,
                                const boost::uuids::uuid& party_id,
                                const std::string& provider,
                                const boost::uuids::uuid& source_id) {
        const auto party_text = boost::uuids::to_string(party_id);
        boost::uuids::random_generator uuid_gen;

        marketdata::messaging::list_subscriptions_request held_subscriptions;
        held_subscriptions.limit = 1000;
        const auto subscriptions = party_client.request(held_subscriptions);
        if (subscriptions.result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("The subscriptions of party " + party_text +
                                     " were not read: " + subscriptions.result.message);
        // The session of a parent party also sees the rows of its child parties, so the
        // check names the party and a child's subscription does not count as the parent's.
        const bool has_subscription =
            std::ranges::any_of(subscriptions.subscriptions, [&](const auto& s) {
                return s.party_id == party_id && s.provider_code == provider;
            });
        if (!has_subscription) {
            marketdata::messaging::put_subscription_request put;
            put.change.write.id = uuid_gen();
            put.change.write.party_id = party_id;
            put.change.write.provider_code = provider;
            put.change.write.display = true;
            put.change.write.non_display = true;
            put.change.write.derived = true;
            put.change.precondition.kind = ores::utility::domain::precondition_kind::any;
            put.intent.reason_code = "system.new_record";
            put.intent.commentary =
                "Created by provisioning: the party takes the shared market data";
            const auto saved = party_client.request(put);
            if (saved.result.outcome != ores::utility::domain::outcome::ok)
                throw std::runtime_error("The subscription of party " + party_text + " to '" +
                                         provider + "' was not saved: " + saved.result.message);
        }

        marketdata::messaging::list_routes_request held_routes;
        held_routes.limit = 1000;
        const auto routes = party_client.request(held_routes);
        if (routes.result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("The routes of party " + party_text +
                                     " were not read: " + routes.result.message);
        const bool has_route = std::ranges::any_of(routes.routes, [&](const auto& r) {
            return r.party_id == party_id && r.publisher_code == provider;
        });
        if (!has_route) {
            marketdata::messaging::put_route_request put;
            put.change.write.id = uuid_gen();
            put.change.write.party_id = party_id;
            put.change.write.publisher_code = provider;
            put.change.write.asset_class = "fx";
            put.change.write.feed_id = source_id;
            put.change.precondition.kind = ores::utility::domain::precondition_kind::any;
            put.intent.reason_code = "system.new_record";
            put.intent.commentary =
                "Created by provisioning: the party takes the shared market data";
            const auto saved = party_client.request(put);
            if (saved.result.outcome != ores::utility::domain::outcome::ok)
                throw std::runtime_error("The route of party " + party_text + " to '" + provider +
                                         "' was not saved: " + saved.result.message);
        }
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
        return ores::database::query_builder::execute_parameterized_string_query(
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

    /// Records that the system setup finished, which is what stops a screen
    /// from showing the system administrator the setup wizard again. The run
    /// records it, so every client that starts a run gets the same result.
    static void complete_system_onboarding(internal_request_client& client) {
        ores::variability::messaging::complete_system_onboarding_request req;
        const auto resp = client.request(req);
        if (resp.result.outcome != ores::utility::domain::outcome::ok)
            throw std::runtime_error("The system onboarding was not completed: " +
                                     resp.result.message);
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
            auto dataset = ores::database::query_builder::execute_parameterized_string_query(
                ctx,
                "SELECT id::text FROM ores_dq_datasets_tbl WHERE code = $1 "
                "AND valid_to = ores_utility_infinity_timestamp_fn()",
                {"acme." + office_code + ".accounts"},
                tenant_provisioning_handler_lg(),
                "grant_cross_entity_access");
            if (dataset.empty())
                return std::vector<std::string>{};
            auto usernames =
                role ? ores::database::query_builder::execute_parameterized_string_query(
                           ctx,
                           "SELECT username FROM ores_dq_accounts_artefact_tbl WHERE dataset_id = "
                           "$1::uuid AND business_unit_code = $2 AND role = $3",
                           {dataset.front(), business_unit_code, *role},
                           tenant_provisioning_handler_lg(),
                           "grant_cross_entity_access") :
                       ores::database::query_builder::execute_parameterized_string_query(
                           ctx,
                           "SELECT username FROM ores_dq_accounts_artefact_tbl WHERE dataset_id = "
                           "$1::uuid AND business_unit_code = $2",
                           {dataset.front(), business_unit_code},
                           tenant_provisioning_handler_lg(),
                           "grant_cross_entity_access");
            std::vector<std::string> account_ids;
            for (const auto& u : usernames) {
                auto acct = ores::database::query_builder::execute_parameterized_string_query(
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
            ores::database::query_builder::execute_parameterized_command(
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

/**
 * @brief The members of the step result and of its report schedule, in the
 * order they declare them.
 */
[[nodiscard]] constexpr auto wire_fields(
    std::type_identity<tenant_provisioning_handler::provision_step_result::party_report_schedule>) {
    using ::ores::utility::codec::field;
    using schedule = tenant_provisioning_handler::provision_step_result::party_report_schedule;
    return ::ores::utility::codec::field_list{
        field{"party", &schedule::party},
        field{"scheduled", &schedule::scheduled},
        field{"already_scheduled", &schedule::already_scheduled},
        field{"on_demand", &schedule::on_demand},
        field{"not_a_cron", &schedule::not_a_cron}};
}

static_assert(::ores::utility::codec::names_every_member<
                  tenant_provisioning_handler::provision_step_result::party_report_schedule>(),
              "tenant_provisioning_handler::provision_step_result::party_report_schedule gained a "
              "member its wire_fields does not name.");

[[nodiscard]] constexpr auto
wire_fields(std::type_identity<tenant_provisioning_handler::provision_step_result>) {
    using ::ores::utility::codec::field;
    using result = tenant_provisioning_handler::provision_step_result;
    return ::ores::utility::codec::field_list{
        field{"kind", &result::kind},
        field{"bundles", &result::bundles},
        field{"root_lei", &result::root_lei},
        field{"parties", &result::parties},
        field{"accounts", &result::accounts},
        field{"images", &result::images},
        field{"datasets", &result::datasets},
        field{"tenant_id", &result::tenant_id},
        field{"child_instance_id", &result::child_instance_id},
        field{"configs", &result::configs},
        field{"trades_planned", &result::trades_planned},
        field{"trades_written", &result::trades_written},
        field{"trades_already_present", &result::trades_already_present},
        field{"report_schedules", &result::report_schedules},
        field{"curves", &result::curves},
        field{"curves_not_published", &result::curves_not_published},
        field{"forwards", &result::forwards},
        field{"forwards_already_published", &result::forwards_already_published},
        field{"forwards_skipped", &result::forwards_skipped},
        field{"forwards_not_published", &result::forwards_not_published},
        field{"surfaces", &result::surfaces},
        field{"surfaces_already_published", &result::surfaces_already_published},
        field{"surfaces_held_back", &result::surfaces_held_back},
        field{"surfaces_not_published", &result::surfaces_not_published}};
}

static_assert(::ores::utility::codec::names_every_member<
                  tenant_provisioning_handler::provision_step_result>(),
              "tenant_provisioning_handler::provision_step_result gained a member its wire_fields "
              "does not name.");

} // namespace ores::iam::messaging
#endif
