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
#include "ores.reporting.core/messaging/report_execution_handler.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam.client/client/run_token_minter.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/repository/report_input_bundle_repository.hpp"
#include "ores.reporting.core/repository/risk_report_config_repository.hpp"
#include "ores.reporting.core/service/execution_storage_plan.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.storage.api/net/object_keys.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <rfl/json.hpp>
#include <string_view>

namespace ores::reporting::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

namespace {

template <typename Req>
std::optional<typename Req::response_type>
nats_call(ores::nats::service::nats_client& nats, const Req& request, std::string& out_error) {
    using Resp = typename Req::response_type;
    try {
        const auto& codec = ores::nats::default_wire_codec();
        const auto msg = nats.authenticated_request(Req::nats_subject, codec.encode(request));

        const auto err_it = msg.headers.find("X-Error");
        if (err_it != msg.headers.end()) {
            out_error = std::format("Service error on {}: {}", Req::nats_subject, err_it->second);
            return std::nullopt;
        }
        auto result = codec.decode<Resp>(msg.data);
        if (!result) {
            out_error = std::format(
                "Failed to parse response from {}: {}", Req::nats_subject, result.error().what());
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        out_error = std::format("Exception calling {}: {}", Req::nats_subject, e.what());
        return std::nullopt;
    }
}

/**
 * @brief Drops a run's cached run tokens when its step ends.
 *
 * Nothing the cache holds outlives the step, so no token sits in memory between
 * steps. The destructor runs on every return, including the failure paths.
 */
using run_token_step_scope = ores::service::service::cache::run_token_step_scope;

} // namespace


void report_execution_handler::mark_instance_failed(const std::string& tenant_id,
                                                    const std::string& instance_id,
                                                    const std::string& error_message) {
    try {
        // A failed run holds no token, whether or not this instance already
        // recorded its failure.
        run_tokens_.evict_run(instance_id);
        auto tenant_ctx = ores::database::service::tenant_context::with_tenant(ctx_, tenant_id);
        service::report_instance_service inst_svc(tenant_ctx);
        boost::uuids::string_generator sg;
        auto inst = inst_svc.get_instance(sg(instance_id));
        if (inst) {
            // The engine compensates every completed step and every step's
            // compensation is this same fail_report subject, so this runs once
            // per completed step, each carrying that step's own generic
            // message. The failure that actually happened is the first one:
            // the first writer wins and the rest are no-ops, rather than the
            // last one overwriting the true reason with an unrelated step's.
            if (inst->fsm_state_id && *inst->fsm_state_id == instance_states_.require("failed"))
                return;
            inst->fsm_state_id = instance_states_.require("failed");
            inst->completed_at = std::chrono::system_clock::now();
            inst->output_message = error_message;
            inst_svc.save_instance(*inst);
            BOOST_LOG_SEV(lg(), info)
                << "Marked report instance " << instance_id << " as failed: " << error_message;
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to mark instance " << instance_id << " as failed: " << e.what();
    }
}

report_execution_handler::report_execution_handler(
    ores::nats::service::client& nats,
    ores::database::context ctx,
    ores::nats::service::nats_client svc_nats,
    ores::workflow::service::fsm_state_map instance_states,
    std::string http_base_url)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , svc_nats_(std::move(svc_nats))
    , run_tokens_(ores::iam::client::make_run_token_minter(svc_nats_))
    , instance_states_(std::move(instance_states))
    , http_base_url_(std::move(http_base_url)) {}

std::optional<ores::nats::service::nats_client>
report_execution_handler::run_token_client(const std::string& tenant_id,
                                           const std::string& run_id,
                                           bool renew,
                                           std::string& error) {
    try {
        const auto tenant_ctx =
            ores::database::service::tenant_context::with_tenant(ctx_, tenant_id);
        service::report_instance_service instances(tenant_ctx);
        boost::uuids::string_generator sg;
        const auto instance = instances.get_instance(sg(run_id));
        if (!instance || !instance->run_grant_id) {
            error = "The report instance records no run grant, so its run cannot act.";
            return std::nullopt;
        }
        const ores::service::service::cache::run_token_key key{
            .grant_id = boost::uuids::to_string(*instance->run_grant_id), .run_id = run_id};
        if (renew)
            run_tokens_.invalidate(key);
        const auto token = run_tokens_.token_for(key, tenant_id);
        if (token.empty()) {
            error = "No run token is available for this run.";
            return std::nullopt;
        }
        return svc_nats_.with_delegation(token);
    } catch (const std::exception& e) {
        error = std::string("Could not obtain a run token: ") + e.what();
        return std::nullopt;
    }
}

void report_execution_handler::gather_trades(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf)
        return;

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<gather_trades_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode gather_trades_request");
        return;
    }
    const auto& req = *parsed;
    run_token_step_scope step_tokens(run_tokens_, req.report_instance_id);

    BOOST_LOG_SEV(lg(), info) << "gather_trades starting | instance=" << req.report_instance_id
                              << " definition=" << req.definition_id;

    try {
        auto tenant_ctx = ores::database::service::tenant_context::with_tenant(ctx_, req.tenant_id);

        // ── Load risk_report_config ──────────────────────────────────
        repository::risk_report_config_repository config_repo;
        const auto config = config_repo.find_by_definition_id(tenant_ctx, req.definition_id);
        if (!config) {
            const auto err = "risk_report_config not found for definition " + req.definition_id;
            mark_instance_failed(req.tenant_id, req.report_instance_id, err);
            wf->fail(err);
            return;
        }

        const auto config_id = boost::uuids::to_string(config->id);

        // ── Resolve book scope via SQL function ──────────────────────
        const auto book_ids = config_repo.resolve_book_ids(tenant_ctx, config_id);

        if (book_ids.empty()) {
            const std::string err = "Report has no trades: the risk report configuration "
                                    "has no books or portfolios in scope. Please add at least "
                                    "one portfolio or book to the report definition before "
                                    "running.";
            mark_instance_failed(req.tenant_id, req.report_instance_id, err);
            wf->fail(err);
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "gather_trades: resolved " << book_ids.size()
                                  << " book(s) in scope";

        // ── Update instance state to running ─────────────────────────
        service::report_instance_service inst_svc(tenant_ctx);
        boost::uuids::string_generator sg;
        auto inst = inst_svc.get_instance(sg(req.report_instance_id));
        if (!inst) {
            wf->fail("Report instance not found: " + req.report_instance_id);
            return;
        }
        inst->fsm_state_id = instance_states_.require("running");
        inst_svc.save_instance(*inst);

        // ── Ask trading service to export trades to storage ──────────
        const auto key = service::trades_storage_key(req.report_instance_id);
        ores::trading::messaging::export_trades_to_storage_request exp_req;
        exp_req.book_ids = book_ids;
        exp_req.storage_bucket = std::string(ores::storage::api::object_keys::ores_bucket);
        exp_req.storage_key = key;

        std::string err;
        auto owner = run_token_client(req.tenant_id, req.report_instance_id, false, err);
        std::optional<decltype(exp_req)::response_type> exp_resp;
        if (owner)
            exp_resp = nats_call(*owner, exp_req, err);
        // An expired token costs one exchange and one repeat of the request.
        if (!exp_resp && err.find("token_expired") != std::string::npos) {
            err.clear();
            owner = run_token_client(req.tenant_id, req.report_instance_id, true, err);
            if (owner)
                exp_resp = nats_call(*owner, exp_req, err);
        }
        if (!exp_resp || !exp_resp->success) {
            const auto failure =
                "export_trades_to_storage failed: " +
                (err.empty() ? (exp_resp ? exp_resp->message : "(no response)") : err);
            mark_instance_failed(req.tenant_id, req.report_instance_id, failure);
            wf->fail(failure);
            return;
        }

        // ── Build result ─────────────────────────────────────────────
        gather_trades_result result;
        result.success = true;
        result.trade_count = exp_resp->trade_count;
        result.storage_key = exp_resp->storage_key;
        result.message =
            std::format("Gathered {} trades from {} books", exp_resp->trade_count, book_ids.size());

        BOOST_LOG_SEV(lg(), info) << "gather_trades complete | instance=" << req.report_instance_id
                                  << " trades=" << exp_resp->trade_count
                                  << " books=" << book_ids.size();

        wf->complete(rfl::json::write(result));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "gather_trades failed: " << e.what();
        mark_instance_failed(req.tenant_id, req.report_instance_id, e.what());
        wf->fail(e.what());
    }
}

void report_execution_handler::gather_market_data(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf)
        return;

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<gather_market_data_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode gather_market_data_request");
        return;
    }
    const auto& req = *parsed;
    run_token_step_scope step_tokens(run_tokens_, req.report_instance_id);

    BOOST_LOG_SEV(lg(), info) << "gather_market_data starting | instance="
                              << req.report_instance_id;

    try {
        // Ask marketdata service to export all series to storage.
        const auto key = service::market_data_storage_key(req.report_instance_id);
        ores::marketdata::messaging::export_market_data_to_storage_request md_req;
        md_req.storage_bucket = std::string(ores::storage::api::object_keys::ores_bucket);
        md_req.storage_key = key;

        std::string err;
        auto owner = run_token_client(req.tenant_id, req.report_instance_id, false, err);
        std::optional<decltype(md_req)::response_type> md_resp;
        if (owner)
            md_resp = nats_call(*owner, md_req, err);
        // An expired token costs one exchange and one repeat of the request.
        if (!md_resp && err.find("token_expired") != std::string::npos) {
            err.clear();
            owner = run_token_client(req.tenant_id, req.report_instance_id, true, err);
            if (owner)
                md_resp = nats_call(*owner, md_req, err);
        }
        if (!md_resp || !md_resp->success) {
            const auto failure =
                "export_market_data_to_storage failed: " +
                (err.empty() ? (md_resp ? md_resp->message : "(no response)") : err);
            mark_instance_failed(req.tenant_id, req.report_instance_id, failure);
            wf->fail(failure);
            return;
        }

        gather_market_data_result result;
        result.success = true;
        result.series_count = md_resp->series_count;
        result.storage_key = md_resp->storage_key;
        result.message = std::format("Gathered {} market data series", md_resp->series_count);

        BOOST_LOG_SEV(lg(), info) << "gather_market_data complete | instance="
                                  << req.report_instance_id << " series=" << md_resp->series_count;

        wf->complete(rfl::json::write(result));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "gather_market_data failed: " << e.what();
        mark_instance_failed(req.tenant_id, req.report_instance_id, e.what());
        wf->fail(e.what());
    }
}

void report_execution_handler::assemble_bundle(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf)
        return;

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<assemble_bundle_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode assemble_bundle_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "assemble_bundle starting | instance=" << req.report_instance_id
                              << " trades_key=" << req.trades_storage_key
                              << " market_data_key=" << req.market_data_storage_key;

    try {
        if (req.trades_storage_key.empty()) {
            wf->fail("assemble_bundle: trades_storage_key is missing");
            return;
        }
        if (req.market_data_storage_key.empty()) {
            wf->fail("assemble_bundle: market_data_storage_key is missing");
            return;
        }

        auto tenant_ctx = ores::database::service::tenant_context::with_tenant(ctx_, req.tenant_id);

        const auto bundle_id = boost::uuids::to_string(boost::uuids::random_generator()());

        repository::report_input_bundle_entity bundle;
        bundle.id = bundle_id;
        bundle.tenant_id = req.tenant_id;
        bundle.report_instance_id = req.report_instance_id;
        bundle.definition_id = req.definition_id;
        bundle.trades_storage_key = req.trades_storage_key;
        bundle.market_data_storage_key = req.market_data_storage_key;
        bundle.trade_count = req.trade_count;
        bundle.series_count = req.series_count;
        bundle.created_at =
            ores::platform::time::datetime::to_db_string(std::chrono::system_clock::now());

        repository::report_input_bundle_repository bundle_repo;
        bundle_repo.create(tenant_ctx, bundle);

        assemble_bundle_result result;
        result.success = true;
        result.bundle_id = bundle_id;
        result.message = std::format("Bundle persisted: {} trades, {} market data series",
                                     req.trade_count,
                                     req.series_count);

        BOOST_LOG_SEV(lg(), info) << "assemble_bundle complete | instance="
                                  << req.report_instance_id << " bundle=" << bundle_id;

        wf->complete(rfl::json::write(result));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "assemble_bundle failed: " << e.what();
        mark_instance_failed(req.tenant_id, req.report_instance_id, e.what());
        wf->fail(e.what());
    }
}

void report_execution_handler::collect_results(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf)
        return;

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<collect_compute_results_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode collect_compute_results_request");
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "collect_compute_results: stub pass-through | instance="
                              << parsed->report_instance_id << " batch=" << parsed->batch_id;

    // Phase 3.11 stub: pass through immediately.
    // A future phase will download output tarballs, parse ORE result data,
    // and persist structured results to the reporting database.
    collect_compute_results_result result;
    result.success = true;
    result.message = "Pass-through (Phase 3.11 stub)";
    wf->complete(rfl::json::write(result));
}

void report_execution_handler::resolve_prepared_input(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf)
        return;

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<resolve_prepared_input_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode resolve_prepared_input_request");
        return;
    }
    const auto& req = *parsed;

    if (req.prepared_input_key.empty()) {
        wf->fail("resolve_prepared_input: the run configuration names no prepared input");
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "resolve_prepared_input complete | instance="
                              << req.report_instance_id << " input=" << req.prepared_input_key;

    // The archive is named, not read. Answering in the packaging phase's own
    // result type is what lets the substitution be invisible to what follows.
    prepare_ore_package_result result;
    result.success = true;
    result.message = std::format("Resolved prepared input {}", req.prepared_input_key);
    result.tarball_uris = {req.prepared_input_key};
    wf->complete(rfl::json::write(result));
}

void report_execution_handler::ignore_compute_results(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf)
        return;

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<ignore_compute_results_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode ignore_compute_results_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "ignore_compute_results complete | instance="
                              << req.report_instance_id << " batch=" << req.batch_id
                              << " (results not ingested)";

    collect_compute_results_result result;
    result.success = true;
    result.message = std::format("Results of batch {} left unread by configuration.", req.batch_id);
    wf->complete(rfl::json::write(result));
}

void report_execution_handler::finalise(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf)
        return;

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<finalise_report_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode finalise_report_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "finalise starting | instance=" << req.report_instance_id;

    try {
        auto tenant_ctx = ores::database::service::tenant_context::with_tenant(ctx_, req.tenant_id);

        service::report_instance_service inst_svc(tenant_ctx);
        boost::uuids::string_generator sg;
        auto inst = inst_svc.get_instance(sg(req.report_instance_id));
        if (!inst) {
            wf->fail("Report instance not found: " + req.report_instance_id);
            return;
        }

        inst->fsm_state_id = instance_states_.require("completed");
        inst->completed_at = std::chrono::system_clock::now();
        inst->output_message = "Report execution completed.";
        inst_svc.save_instance(*inst);
        // The run is terminal, so no token of it may stay in memory.
        run_tokens_.evict_run(req.report_instance_id);

        BOOST_LOG_SEV(lg(), info) << "finalise complete | instance=" << req.report_instance_id;

        finalise_report_result result;
        result.success = true;
        result.message = "Report instance marked as completed.";
        wf->complete(rfl::json::write(result));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "finalise failed: " << e.what();
        wf->fail(e.what());
    }
}

void report_execution_handler::fail(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf)
        return;

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<fail_report_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode fail_report_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "fail (compensation) | instance=" << req.report_instance_id
                              << " error=" << req.error_message;

    try {
        mark_instance_failed(req.tenant_id, req.report_instance_id, req.error_message);

        fail_report_result result;
        result.success = true;
        result.message = "Report instance marked as failed.";
        wf->complete(rfl::json::write(result));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "fail handler error: " << e.what();
        wf->fail(e.what());
    }
}

}
