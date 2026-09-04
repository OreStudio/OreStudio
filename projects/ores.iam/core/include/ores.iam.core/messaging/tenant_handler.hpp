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
#ifndef ORES_IAM_MESSAGING_TENANT_HANDLER_HPP
#define ORES_IAM_MESSAGING_TENANT_HANDLER_HPP

#include "ores.assets.api/messaging/assets_protocol.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.dq.api/messaging/dataset_protocol.hpp"
#include "ores.dq.api/messaging/party_provisioning_plan.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/tenant_protocol.hpp"
#include "ores.iam.core/repository/tenant_repository.hpp"
#include "ores.iam.core/service/internal_impersonation_service.hpp"
#include "ores.iam.core/service/internal_request_client.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/feed_binding_protocol.hpp"
#include "ores.marketdata.api/messaging/market_feed_config_protocol.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.api/messaging/folder_protocol.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include "ores.variability.api/messaging/system_settings_protocol.hpp"
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

inline auto& tenant_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.iam.messaging.tenant_handler");
    return instance;
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

class tenant_handler {
public:
    tenant_handler(ores::nats::service::client& nats,
                   ores::database::context ctx,
                   ores::security::jwt::jwt_authenticator signer,
                   ores::iam::service::internal_impersonation_service impersonation)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , signer_(std::move(signer))
        , impersonation_(std::move(impersonation)) {}

    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(tenant_handler_lg(), msg);
        try {
            repository::tenant_repository repo(ctx_);
            get_tenants_response resp;
            resp.tenants = repo.read_latest();
            BOOST_LOG_SEV(tenant_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_tenants_response{});
        }
    }

    void save(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(tenant_handler_lg(), msg);
        auto req = decode<save_tenant_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            if (!has_permission(ctx, "iam::tenants:write")) {
                error_reply(nats_, msg, ores::service::error_code::forbidden);
                return;
            }
            if (req->data.id.is_nil())
                req->data.id = boost::uuids::random_generator()();
            repository::tenant_repository repo(ctx);
            stamp(req->data, ctx);
            repo.write(req->data);
            BOOST_LOG_SEV(tenant_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_tenant_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_tenant_response{.success = false, .message = e.what()});
        }
    }

    void remove(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(tenant_handler_lg(), msg);
        auto req = decode<delete_tenant_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            repository::tenant_repository repo(ctx_);
            boost::uuids::string_generator sg;
            for (const auto& id_str : req->ids)
                repo.remove(sg(id_str));
            BOOST_LOG_SEV(tenant_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_tenant_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_tenant_response{.success = false, .message = e.what()});
        }
    }

    void history(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(tenant_handler_lg(), msg);
        auto req = decode<get_tenant_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            repository::tenant_repository repo(ctx_);
            boost::uuids::string_generator sg;
            auto hist = repo.read_history(sg(req->id));
            BOOST_LOG_SEV(tenant_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_,
                  msg,
                  get_tenant_history_response{.success = true, .versions = std::move(hist)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_tenant_history_response{.success = false, .message = e.what()});
        }
    }

    void complete_provisioning(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(tenant_handler_lg(), msg);
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
                                                   tenant_handler_lg(),
                                                   "complete_provisioning");
            if (ids.empty()) {
                BOOST_LOG_SEV(tenant_handler_lg(), error)
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
                                          tenant_handler_lg(),
                                          "complete_provisioning");

            BOOST_LOG_SEV(tenant_handler_lg(), info) << "Tenant marked active: " << ids.front();

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

                std::unordered_map<std::string, std::string> hdrs;
                if (const auto bearer = extract_bearer(msg); !bearer.empty())
                    hdrs[std::string(ores::nats::headers::delegated_authorization)] =
                        std::string(ores::nats::headers::bearer_prefix) + bearer;

                const auto resp_msg = nats_.request_sync(clear_bootstrap_mode_request::nats_subject,
                                                         bytes,
                                                         std::move(hdrs),
                                                         std::chrono::seconds(5));
                const auto resp = codec.decode<clear_bootstrap_mode_response>(resp_msg.data);
                if (resp && resp->success) {
                    BOOST_LOG_SEV(tenant_handler_lg(), info)
                        << "Bootstrap mode cleared for tenant: " << ids.front();
                } else {
                    BOOST_LOG_SEV(tenant_handler_lg(), warn)
                        << "Failed to clear bootstrap mode for tenant " << ids.front() << ": "
                        << (resp ? resp->message : "malformed response");
                }
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(tenant_handler_lg(), warn)
                    << "Failed to clear bootstrap mode for tenant " << ids.front() << ": "
                    << e.what();
            }

            reply(nats_, msg, complete_tenant_provisioning_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_,
                  msg,
                  complete_tenant_provisioning_response{.success = false, .message = e.what()});
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
        [[maybe_unused]] const auto correlation_id = log_handler_entry(tenant_handler_lg(), msg);
        provision_acme_tenant_response resp;
        resp.success = true;
        auto add_step = [&](std::string step, std::string action, std::uint64_t count = 1) {
            BOOST_LOG_SEV(tenant_handler_lg(), info)
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

            const auto bearer = extract_bearer(msg);
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
                    attach_staff_photos(
                        client, *ctx_expected, tenant_id_str, "acme_group", username);
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
                attach_staff_photos(client, *ctx_expected, tenant_id_str, office.code, username);
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
                        {"synthetic_realistic_2026",
                         "synthetic market data configuration (2026)"},
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
                            auto wait_result = system_client.wait_for_workflow_instance(
                                instance_id, std::chrono::seconds{120}, expected, progress(mkt_label));
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
                        // folders, via the same folder-scoped mechanism PR
                        // #1741 introduced for the Qt Market Simulator, so
                        // provisioning and manual start share one path. The
                        // folder cascade starts no feeds for offices -- they
                        // have no configs of their own.
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

            BOOST_LOG_SEV(tenant_handler_lg(), info) << "Acme tenant provisioned: " << tenant_id_str
                                                     << " (" << resp.steps.size() << " step(s))";
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(
                nats_, msg, provision_acme_tenant_response{.success = false, .message = e.what()});
        }
    }

private:
    // Extract the raw JWT from an incoming message, preferring the delegated
    // header so the original end-user context propagates to downstream calls.
    static std::string extract_bearer(const ores::nats::message& msg) {
        using namespace ores::nats::headers;
        for (auto hdr : {delegated_authorization, authorization}) {
            const auto it = msg.headers.find(std::string(hdr));
            if (it != msg.headers.end() && it->second.starts_with(bearer_prefix))
                return std::string(it->second.substr(bearer_prefix.size()));
        }
        return {};
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
        ores::refdata::messaging::get_parties_request req;
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
    // the server cascades across asset classes -- the same mechanism the
    // Qt Market Simulator's "Start at Root -> pick a theme" flow uses (PR
    // #1741), resolved server-side from synthetic_publish_from_dq's
    // container-per-(tenant, party, dataset) convention rather than by
    // matching on display name. Best-effort: logs and returns false on any
    // resolution miss (dataset/config/folder not found, or a list request
    // itself failing server-side) rather than failing provisioning over a
    // cosmetic follow-on step -- the party itself is already fully
    // provisioned by this point. Returns whether resolution succeeded far
    // enough to attempt starting feeds, so the caller can surface a miss in
    // its own step list rather than reporting "completed" for a no-op.
    static bool start_synthetic_theme_feeds(internal_request_client& client,
                                            const std::string& dataset_code) {
        std::optional<boost::uuids::uuid> dataset_id;
        {
            dq::messaging::get_datasets_request req;
            req.limit = 1000;
            auto resp = client.request(req);
            for (auto& d : resp.datasets)
                if (d.code == dataset_code) {
                    dataset_id = d.id;
                    break;
                }
        }
        if (!dataset_id) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn)
                << "start_synthetic_theme_feeds: dataset not found: " << dataset_code;
            return false;
        }

        std::optional<boost::uuids::uuid> config_id;
        {
            synthetic::messaging::get_market_data_generation_configs_request req;
            req.limit = 1000;
            auto resp = client.request(req);
            if (!resp.success) {
                BOOST_LOG_SEV(tenant_handler_lg(), warn)
                    << "start_synthetic_theme_feeds: list market_data_generation_configs "
                       "failed: "
                    << resp.message;
                return false;
            }
            for (auto& c : resp.market_data_generation_configs)
                if (c.dataset_id == dataset_id) {
                    config_id = c.id;
                    break;
                }
        }
        if (!config_id) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn)
                << "start_synthetic_theme_feeds: no market_data_generation_config for dataset "
                << dataset_code;
            return false;
        }

        std::optional<boost::uuids::uuid> folder_id;
        {
            synthetic::messaging::get_folders_request req;
            req.limit = 1000;
            auto resp = client.request(req);
            if (!resp.success) {
                BOOST_LOG_SEV(tenant_handler_lg(), warn)
                    << "start_synthetic_theme_feeds: list folders failed: " << resp.message;
                return false;
            }
            for (auto& f : resp.folders)
                if (f.kind == "collection" && f.collection_id == config_id) {
                    folder_id = f.id;
                    break;
                }
        }
        if (!folder_id) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn)
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
            BOOST_LOG_SEV(tenant_handler_lg(), warn)
                << "start_synthetic_theme_feeds: start folder failed: " << resp.message;
            return false;
        }
        return true;
    }

    // Finds the tenant's system party (party_category == "System", created
    // once per tenant by the IAM provisioner) -- the owner of the simulated
    // market config in the consistent world.
    static std::optional<ores::refdata::domain::party> find_system_party(internal_request_client& client) {
        ores::refdata::messaging::get_parties_request req;
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
    // are skipped, so the step is re-runnable. Workspace defaults to the
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
            dq::messaging::get_datasets_request req;
            req.limit = 1000;
            auto resp = resolve_client.request(req);
            for (auto& d : resp.datasets)
                if (d.code == dataset_code) {
                    dataset_id = d.id;
                    break;
                }
        }
        if (!dataset_id) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn)
                << "create_theme_feed_bindings: dataset not found: " << dataset_code;
            return false;
        }

        std::optional<boost::uuids::uuid> config_id;
        {
            synthetic::messaging::get_market_data_generation_configs_request req;
            req.limit = 1000;
            auto resp = resolve_client.request(req);
            if (!resp.success) {
                BOOST_LOG_SEV(tenant_handler_lg(), warn)
                    << "create_theme_feed_bindings: list market_data_generation_configs "
                       "failed: "
                    << resp.message;
                return false;
            }
            for (auto& c : resp.market_data_generation_configs)
                if (c.dataset_id == dataset_id) {
                    config_id = c.id;
                    break;
                }
        }
        if (!config_id) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn)
                << "create_theme_feed_bindings: no market_data_generation_config for dataset "
                << dataset_code;
            return false;
        }

        std::vector<std::pair<std::string, std::string>> sources; // (source_name, ore_key)
        {
            synthetic::messaging::get_fx_spot_generation_configs_request req;
            req.limit = 1000;
            auto resp = resolve_client.request(req);
            if (!resp.success) {
                BOOST_LOG_SEV(tenant_handler_lg(), warn)
                    << "create_theme_feed_bindings: list fx_spot_generation_configs failed: "
                    << resp.message;
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
            marketdata::messaging::get_feed_bindings_request req;
            req.limit = 1000;
            auto resp = save_client.request(req);
            if (!resp.success) {
                BOOST_LOG_SEV(tenant_handler_lg(), warn)
                    << "create_theme_feed_bindings: list feed_bindings failed: "
                    << resp.message;
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
                BOOST_LOG_SEV(tenant_handler_lg(), info)
                    << "create_theme_feed_bindings: binding " << source_name << " for party "
                    << party_id_str << " already exists; skipping";
                continue;
            }
            marketdata::domain::feed_binding binding;
            binding.id = uuid_gen();
            binding.ore_key = ore_key;
            binding.source_name = source_name;
            binding.asset_class = marketdata::domain::asset_class::fx;
            binding.enabled = true;
            binding.party_id = sg(party_id_str);
            binding.change_reason_code = "system.new_record";
            binding.change_commentary =
                "Created by ACME provisioning: consumes the system-party simulated market "
                "stream";
            auto resp = save_client.request(
                marketdata::messaging::save_feed_binding_request::from(std::move(binding)));
            if (!resp.success) {
                BOOST_LOG_SEV(tenant_handler_lg(), warn)
                    << "create_theme_feed_bindings: save binding " << source_name << " for party "
                    << party_id_str << " failed: " << resp.message;
                all_saved = false;
            }
        }
        if (sources.empty())
            BOOST_LOG_SEV(tenant_handler_lg(), info)
                << "create_theme_feed_bindings: no enabled FX sources for dataset "
                << dataset_code;
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
                                                          const std::string& key,
                                                          const std::string& username) {
        auto existing = execute_parameterized_string_query(
            ctx,
            "SELECT image_id::text FROM ores_assets_images_tbl WHERE tenant_id = $1::uuid AND "
            "key = $2 AND valid_to = ores_utility_infinity_timestamp_fn()",
            {tenant_id, key},
            tenant_handler_lg(),
            "copy_template_image");
        if (!existing.empty())
            return boost::uuids::string_generator{}(existing.front());

        auto rows = execute_parameterized_multi_column_query(
            ctx,
            "SELECT description, mime_type, data FROM ores_assets_get_template_image_fn($1)",
            {key},
            tenant_handler_lg(),
            "copy_template_image");
        if (rows.empty() || rows.front().size() < 3 || !rows.front()[0] || !rows.front()[1] ||
            !rows.front()[2]) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn) << "No system-tenant template image: " << key;
            return std::nullopt;
        }

        ores::assets::domain::image img;
        img.tenant_id =
            ores::utility::uuid::tenant_id::from_string(tenant_id).value_or(img.tenant_id);
        img.image_id = boost::uuids::random_generator{}();
        img.key = key;
        img.description = *rows.front()[0];
        img.mime_type = *rows.front()[1];
        const auto& base64_data = *rows.front()[2];
        img.data = ores::utility::converter::base64_converter::convert(base64_data);
        img.modified_by = username;
        img.performed_by = username;
        img.change_reason_code = "system.external_data_import";
        img.change_commentary = "Copied from system-tenant template: " + key;

        ores::assets::messaging::save_image_request req{.data = std::move(img)};
        auto resp = client.request(req);
        if (!resp.success) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn)
                << "Failed to copy template image " << key << ": " << resp.message;
            return std::nullopt;
        }
        return img.image_id;
    }

    // Attaches a profile picture to every staff account in one office that
    // has a photo_key recorded in its accounts dataset and doesn't already
    // have an image_id. Reads (username, photo_key) directly from the DQ
    // artefact table (not modeled in the account NATS API, same reason
    // grant_cross_entity_access's account_ids_for does the same for
    // business_unit_code/role), then copies each account's own template
    // image and re-saves the account with every other field echoed back
    // unchanged (update_account_request has no partial-update semantics --
    // omitting a field would clear it).
    void attach_staff_photos(internal_request_client& client,
                             ores::database::context& ctx,
                             const std::string& tenant_id,
                             const std::string& office_code,
                             const std::string& username) {
        auto dataset = execute_parameterized_string_query(
            ctx,
            "SELECT id::text FROM ores_dq_datasets_tbl WHERE code = $1 "
            "AND valid_to = ores_utility_infinity_timestamp_fn()",
            {"acme." + office_code + ".accounts"},
            tenant_handler_lg(),
            "attach_staff_photos");
        if (dataset.empty())
            return;

        auto rows = execute_parameterized_multi_column_query(
            ctx,
            "SELECT username, photo_key FROM ores_dq_accounts_artefact_tbl "
            "WHERE dataset_id = $1::uuid AND photo_key IS NOT NULL",
            {dataset.front()},
            tenant_handler_lg(),
            "attach_staff_photos");
        if (rows.empty())
            return;

        std::unordered_map<std::string, std::string> photo_key_by_username;
        for (const auto& row : rows) {
            if (row.size() < 2 || !row[0] || !row[1])
                continue;
            photo_key_by_username[*row[0]] = *row[1];
        }

        iam::messaging::get_accounts_request_typed accounts_req;
        accounts_req.limit = 10'000;
        auto accounts_resp = client.request(accounts_req);
        for (const auto& a : accounts_resp.accounts) {
            const auto it = photo_key_by_username.find(a.username);
            if (it == photo_key_by_username.end() || !a.image_id.is_nil())
                continue;

            auto image_id = copy_template_image(client, ctx, tenant_id, it->second, username);
            if (!image_id)
                continue;

            iam::messaging::update_account_request update_req;
            update_req.account_id = boost::uuids::to_string(a.id);
            update_req.email = a.email;
            update_req.full_name = a.full_name;
            update_req.default_party_id =
                a.default_party_id ? boost::uuids::to_string(*a.default_party_id) : "";
            update_req.job_title = a.job_title;
            update_req.reports_to_account_id = a.reports_to_account_id.is_nil() ?
                                                   "" :
                                                   boost::uuids::to_string(a.reports_to_account_id);
            update_req.image_id = boost::uuids::to_string(*image_id);
            update_req.change_reason_code = "system.external_data_import";
            update_req.change_commentary = "Attached staff photo during Acme provisioning";
            client.request(update_req);
        }
    }

    // Activates the party (if Inactive), attaches its logo (if unset),
    // marks its onboarding wizard complete, and associates the tenant
    // admin with it -- the same effects the generic shell/wizard
    // "provision party" flow's final phase produces, driven here via the
    // real save_party/complete_party_onboarding/save_account_party
    // requests instead of hand-written SQL.
    void finish_party(internal_request_client& client,
                      ores::database::context& ctx,
                      const std::string& tenant_id,
                      const boost::uuids::uuid& account_id,
                      const std::string& username,
                      ores::refdata::domain::party party,
                      bool set_default) {
        bool changed = false;
        if (party.status != "Active") {
            party.status = "Active";
            changed = true;
        }
        if (!party.image_id) {
            auto image_id =
                copy_template_image(client, ctx, tenant_id, "acme_party_logo", username);
            if (image_id) {
                party.image_id = image_id;
                changed = true;
            }
        }
        if (changed) {
            party.change_reason_code = "system.external_data_import";
            party.change_commentary = "Activated (and logo attached) during Acme provisioning";
            party.modified_by = username;
            party.performed_by = username;
            client.request(ores::refdata::messaging::save_party_request::from(party));
        }

        ores::variability::messaging::complete_party_onboarding_request onboarding_req;
        onboarding_req.party_id = boost::uuids::to_string(party.id);
        client.request(onboarding_req);

        ores::iam::domain::account_party assoc;
        assoc.tenant_id = tenant_id;
        assoc.account_id = account_id;
        assoc.party_id = party.id;
        assoc.modified_by = username;
        assoc.performed_by = username;
        assoc.change_reason_code = "system.external_data_import";
        assoc.change_commentary = "Associated during Acme provisioning";
        ores::iam::messaging::save_account_party_request assoc_req;
        assoc_req.account_parties = {std::move(assoc)};
        client.request(assoc_req);

        if (set_default) {
            ores::iam::messaging::set_my_default_party_request default_req;
            default_req.party_id = boost::uuids::to_string(party.id);
            client.request(default_req);
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
                tenant_handler_lg(),
                "grant_cross_entity_access");
            if (dataset.empty())
                return std::vector<std::string>{};
            auto usernames =
                role ? execute_parameterized_string_query(
                           ctx,
                           "SELECT username FROM ores_dq_accounts_artefact_tbl WHERE dataset_id = "
                           "$1::uuid AND business_unit_code = $2 AND role = $3",
                           {dataset.front(), business_unit_code, *role},
                           tenant_handler_lg(),
                           "grant_cross_entity_access") :
                       execute_parameterized_string_query(
                           ctx,
                           "SELECT username FROM ores_dq_accounts_artefact_tbl WHERE dataset_id = "
                           "$1::uuid AND business_unit_code = $2",
                           {dataset.front(), business_unit_code},
                           tenant_handler_lg(),
                           "grant_cross_entity_access");
            std::vector<std::string> account_ids;
            for (const auto& u : usernames) {
                auto acct = execute_parameterized_string_query(
                    ctx,
                    "SELECT id::text FROM ores_iam_accounts_tbl WHERE tenant_id = $1::uuid "
                    "AND username = $2 AND valid_to = ores_utility_infinity_timestamp_fn()",
                    {tenant_id, u},
                    tenant_handler_lg(),
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
                tenant_handler_lg(),
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
