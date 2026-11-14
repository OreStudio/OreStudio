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
#include "ores.compute.api/messaging/result_protocol.hpp"
#include "ores.compute.api/messaging/workunit_protocol.hpp"
#include "ores.compute.api/net/compute_storage.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam.client/client/run_token_minter.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.nats/domain/delivery.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.ore.api/net/ore_storage.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.ore.core/results/output_manifest.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/repository/report_cashflow_repository.hpp"
#include "ores.reporting.core/repository/report_input_bundle_repository.hpp"
#include "ores.reporting.core/repository/report_metric_repository.hpp"
#include "ores.reporting.core/repository/report_text_value_repository.hpp"
#include "ores.reporting.core/repository/risk_report_config_repository.hpp"
#include "ores.reporting.core/results/canonical_log_reader.hpp"
#include "ores.reporting.core/results/run_output_reader.hpp"
#include "ores.reporting.core/service/engine_log_row.hpp"
#include "ores.reporting.core/service/execution_storage_plan.hpp"
#include "ores.reporting.core/service/report_error_recorder.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"
#include "ores.reporting.core/service/run_document_generator.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.storage.api/net/object_keys.hpp"
#include "ores.storage.api/net/storage_paths.hpp"
#include "ores.storage.core/net/storage_transfer.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <filesystem>
#include <format>
#include <fstream>
#include <map>
#include <stdexcept>
#include <string_view>

namespace ores::reporting::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

namespace {

// The rows of a run are written this many at a time: a block is one insert
// statement, and the most rows a reader holds at once.
constexpr std::size_t load_block_size = 10000;

inline auto& nats_call_lg() {
    static auto instance =
        ores::logging::make_logger("ores.reporting.messaging.report_execution_handler.nats_call");
    return instance;
}

/**
 * @brief Calls a service and states how long the call took.
 *
 * A call that fails says how long it waited, so a timeout reads as a duration and
 * not as a bare refusal. A call that takes more than five seconds is logged as a
 * warning, before it is slow enough to fail.
 */
template <typename Req>
std::optional<typename Req::response_type>
nats_call(ores::nats::service::nats_client& nats, const Req& request, std::string& out_error) {
    using Resp = typename Req::response_type;
    using namespace ores::logging;
    const auto began = std::chrono::steady_clock::now();
    const auto elapsed_ms = [&] {
        return std::chrono::duration_cast<std::chrono::milliseconds>(
                   std::chrono::steady_clock::now() - began)
            .count();
    };
    try {
        const auto& codec = ores::nats::default_wire_codec();
        const auto msg = nats.authenticated_request(Req::nats_subject, codec.encode(request));
        const auto took = elapsed_ms();
        BOOST_LOG_SEV(nats_call_lg(), took > 5000 ? warn : debug)
            << Req::nats_subject << " answered in " << took << " ms";

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
        out_error = std::format(
            "Exception calling {} after {} ms: {}", Req::nats_subject, elapsed_ms(), e.what());
        BOOST_LOG_SEV(nats_call_lg(), error) << out_error;
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

/// The compute result outcome that says the job ran: 1=Success.
constexpr int compute_outcome_success = 1;

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
            // The failure is recorded against the run in the same place the
            // engine's own errors go, so a reader sees one answer to what went
            // wrong rather than two. This branch runs once, for the first
            // writer, so the run's error is written once.
            service::report_error_recorder recorder(tenant_ctx);
            const auto failure = recorder.record_run_failure(instance_id, error_message);
            if (!failure.empty())
                BOOST_LOG_SEV(lg(), error) << failure;
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
    const ores::database::context& ctx,
    ores::nats::service::nats_client svc_nats,
    ores::workflow::service::fsm_state_map instance_states,
    std::string http_base_url)
    : nats_(nats)
    , ctx_(ctx)
    , svc_nats_(svc_nats)
    , run_tokens_(ores::iam::client::make_run_token_minter(svc_nats_))
    , instance_states_(instance_states)
    , http_base_url_(http_base_url) {}

std::optional<ores::nats::service::nats_client> report_execution_handler::run_token_client(
    const std::string& tenant_id, const std::string& run_id, bool renew, std::string& error) {
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

std::string report_execution_handler::run_token(const std::string& tenant_id,
                                                const std::string& run_id,
                                                std::string& error) {
    try {
        const auto tenant_ctx =
            ores::database::service::tenant_context::with_tenant(ctx_, tenant_id);
        service::report_instance_service instances(tenant_ctx);
        boost::uuids::string_generator sg;
        const auto instance = instances.get_instance(sg(run_id));
        if (!instance || !instance->run_grant_id) {
            error = "The report instance records no run grant, so its run cannot act.";
            return {};
        }
        const ores::service::service::cache::run_token_key key{
            .grant_id = boost::uuids::to_string(*instance->run_grant_id), .run_id = run_id};
        const auto token = run_tokens_.token_for(key, tenant_id);
        if (token.empty())
            error = "No run token is available for this run.";
        return token;
    } catch (const std::exception& e) {
        error = std::string("Could not obtain a run token: ") + e.what();
        return {};
    }
}

void report_execution_handler::gather_trades(ores::nats::delivery delivery) {
    const auto& msg = delivery.msg();

    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) {
        std::move(delivery).reject();
        return;
    }

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = ores::utility::codec::json::try_read<gather_trades_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode gather_trades_request");
        std::move(delivery).commit();
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
            std::move(delivery).commit();
            return;
        }

        const auto config_id = boost::uuids::to_string(config->id);

        // ── Resolve book scope via SQL function ──────────────────────
        const auto book_ids =
            config_repo.resolve_book_ids(tenant_ctx, config_id, req.definition_id);

        if (book_ids.empty()) {
            // A typed report reads the books of its type only, so a scope can hold books
            // and still leave none to read. The message says which of the two it is.
            const auto book_type =
                config_repo.regulatory_book_type_of(tenant_ctx, req.definition_id);
            const std::string err =
                book_type ? "Report has no books to read: it reads " + *book_type +
                                " books only, and the risk report configuration has no such book "
                                "or portfolio in scope. Add a portfolio or book of that type to "
                                "the scope before running." :
                            "Report has no trades: the risk report configuration "
                            "has no books or portfolios in scope. Please add at least "
                            "one portfolio or book to the report definition before "
                            "running.";
            mark_instance_failed(req.tenant_id, req.report_instance_id, err);
            wf->fail(err);
            std::move(delivery).commit();
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
            std::move(delivery).commit();
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
            std::move(delivery).commit();
            return;
        }

        // ── Build result ─────────────────────────────────────────────
        gather_trades_result result;
        result.success = true;
        result.trade_count = exp_resp->trade_count;
        result.storage_key = exp_resp->storage_key;
        // The manifest is a lookup from trade id to the version the export read.
        // It travels to the bundle as JSON, which is the form the bundle column
        // holds, so no reader has to rebuild it from the trades blob.
        result.trade_versions = exp_resp->trade_versions.empty() ?
                                    std::string{} :
                                    ores::utility::codec::json::write(exp_resp->trade_versions);
        result.message =
            std::format("Gathered {} trades from {} books", exp_resp->trade_count, book_ids.size());

        BOOST_LOG_SEV(lg(), info) << "gather_trades complete | instance=" << req.report_instance_id
                                  << " trades=" << exp_resp->trade_count
                                  << " books=" << book_ids.size();

        wf->complete(ores::utility::codec::json::write(result));
        std::move(delivery).commit();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "gather_trades failed: " << e.what();
        mark_instance_failed(req.tenant_id, req.report_instance_id, e.what());
        wf->fail(e.what());
        std::move(delivery).commit();
    }
}

void report_execution_handler::gather_market_data(ores::nats::delivery delivery) {
    const auto& msg = delivery.msg();

    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) {
        std::move(delivery).reject();
        return;
    }

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = ores::utility::codec::json::try_read<gather_market_data_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode gather_market_data_request");
        std::move(delivery).commit();
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
        // The run values the portfolio at its own date. The export gives each point the value
        // it had at the close of that day.
        auto tenant_ctx = ores::database::service::tenant_context::with_tenant(ctx_, req.tenant_id);
        const auto config = repository::risk_report_config_repository{}.find_by_definition_id(
            tenant_ctx, req.definition_id);
        if (!config) {
            const auto err = "risk_report_config not found for definition " + req.definition_id;
            mark_instance_failed(req.tenant_id, req.report_instance_id, err);
            wf->fail(err);
            std::move(delivery).commit();
            return;
        }
        md_req.valuation_date = service::valuation_date_of(
            *config,
            std::format("{:%F}",
                        std::chrono::floor<std::chrono::days>(std::chrono::system_clock::now())));
        md_req.storage_bucket = std::string(ores::storage::api::object_keys::ores_bucket);
        md_req.storage_key = key;
        md_req.fixings_storage_key = service::fixings_storage_key(req.report_instance_id);
        BOOST_LOG_SEV(lg(), info) << "gather_market_data asking marketdata to export | instance="
                                  << req.report_instance_id
                                  << " valuation_date=" << md_req.valuation_date.value_or("")
                                  << " storage_key=" << md_req.storage_key;

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
            std::move(delivery).commit();
            return;
        }

        gather_market_data_result result;
        result.success = true;
        result.series_count = md_resp->series_count;
        result.storage_key = md_resp->storage_key;
        result.fixings_storage_key = md_resp->fixings_storage_key;
        result.message = std::format("Gathered {} market data series", md_resp->series_count);
        if (md_resp->stale_count > 0) {
            result.message += std::format(
                ". {} datum(s) have no value for the valuation date within the age limit, and "
                "{} carry a value from an earlier day",
                md_resp->stale_count,
                md_resp->carried_forward_count);
            BOOST_LOG_SEV(lg(), warn) << "gather_market_data left out " << md_resp->stale_count
                                      << " stale datum(s) | instance=" << req.report_instance_id;
        }

        BOOST_LOG_SEV(lg(), info) << "gather_market_data complete | instance="
                                  << req.report_instance_id << " series=" << md_resp->series_count;

        wf->complete(ores::utility::codec::json::write(result));
        std::move(delivery).commit();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "gather_market_data failed: " << e.what();
        mark_instance_failed(req.tenant_id, req.report_instance_id, e.what());
        wf->fail(e.what());
        std::move(delivery).commit();
    }
}

void report_execution_handler::assemble_bundle(ores::nats::delivery delivery) {
    const auto& msg = delivery.msg();

    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) {
        std::move(delivery).reject();
        return;
    }

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = ores::utility::codec::json::try_read<assemble_bundle_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode assemble_bundle_request");
        std::move(delivery).commit();
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "assemble_bundle starting | instance=" << req.report_instance_id
                              << " trades_key=" << req.trades_storage_key
                              << " market_data_key=" << req.market_data_storage_key
                              << " fixings_key=" << req.fixings_storage_key;

    try {
        if (req.trades_storage_key.empty()) {
            wf->fail("assemble_bundle: trades_storage_key is missing");
            std::move(delivery).commit();
            return;
        }
        if (req.market_data_storage_key.empty()) {
            wf->fail("assemble_bundle: market_data_storage_key is missing");
            std::move(delivery).commit();
            return;
        }
        if (req.fixings_storage_key.empty()) {
            wf->fail("assemble_bundle: fixings_storage_key is missing");
            std::move(delivery).commit();
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
        bundle.fixings_storage_key = req.fixings_storage_key;
        bundle.trade_count = req.trade_count;
        bundle.trade_versions =
            req.trade_versions.empty() ? std::nullopt : std::optional(req.trade_versions);
        bundle.series_count = req.series_count;
        bundle.created_at = std::chrono::system_clock::now();

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

        wf->complete(ores::utility::codec::json::write(result));
        std::move(delivery).commit();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "assemble_bundle failed: " << e.what();
        mark_instance_failed(req.tenant_id, req.report_instance_id, e.what());
        wf->fail(e.what());
        std::move(delivery).commit();
    }
}

std::vector<service::engine_log_row>
report_execution_handler::read_run_log_rows(const std::string& tenant_id,
                                            const std::string& report_instance_id,
                                            const std::string& result_id) {
    try {
        std::string err;
        const auto token = run_token(tenant_id, report_instance_id, err);
        if (token.empty()) {
            BOOST_LOG_SEV(lg(), warn)
                << "The log rows of result " << result_id << " were not read: " << err;
            return {};
        }
        const auto uri = ores::compute::net::compute_storage::logs_path(result_id);
        std::string bucket;
        std::string key;
        if (!ores::storage::net::storage_paths::split_object_path(uri, bucket, key)) {
            BOOST_LOG_SEV(lg(), warn)
                << "The log rows of result " << result_id << " name no readable location: " << uri;
            return {};
        }
        ores::storage::net::storage_transfer transfer(http_base_url_, token);
        const auto body = transfer.download_blob(bucket, key);
        const auto read =
            results::read_canonical_log_csv(std::string_view(body.data(), body.size()));
        if (!read.refusals.empty()) {
            std::string reasons;
            for (const auto& refusal : read.refusals) {
                if (!reasons.empty())
                    reasons += " ";
                reasons += refusal;
            }
            BOOST_LOG_SEV(lg(), warn)
                << "The log rows of result " << result_id << " were refused: " << reasons;
            return {};
        }
        return read.rows;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "The log rows of result " << result_id
                                  << " were not read: " << e.what();
        return {};
    }
}

std::vector<service::engine_log_row>
report_execution_handler::read_package_log_rows(const std::string& tenant_id,
                                                const std::string& report_instance_id) {
    try {
        std::string err;
        const auto token = run_token(tenant_id, report_instance_id, err);
        if (token.empty()) {
            BOOST_LOG_SEV(lg(), warn) << "The package log of report instance " << report_instance_id
                                      << " was not read: " << err;
            return {};
        }
        ores::storage::net::storage_transfer transfer(http_base_url_, token);
        const auto body = transfer.download_blob(
            std::string(ores::storage::api::object_keys::ores_bucket),
            ores::ore::net::ore_storage::package_log_key(report_instance_id));
        const auto read =
            results::read_canonical_log_csv(std::string_view(body.data(), body.size()));
        if (!read.refusals.empty()) {
            BOOST_LOG_SEV(lg(), warn) << "The package log of report instance " << report_instance_id
                                      << " was refused: " << read.refusals.front();
            return {};
        }
        return read.rows;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), debug)
            << "Report instance " << report_instance_id << " stores no package log: " << e.what();
        return {};
    }
}

service::run_problems
report_execution_handler::record_run_logs(const std::string& tenant_id,
                                          const std::string& report_instance_id,
                                          const std::vector<std::string>& result_ids) {
    auto rows = read_package_log_rows(tenant_id, report_instance_id);
    for (const auto& result_id : result_ids) {
        auto read = read_run_log_rows(tenant_id, report_instance_id, result_id);
        rows.insert(
            rows.end(), std::make_move_iterator(read.begin()), std::make_move_iterator(read.end()));
    }
    if (rows.empty())
        return {};

    try {
        // One write for the run, whatever it took to read it: the recorder keeps
        // a run's account once, and a call per result would keep only the first.
        service::report_error_recorder recorder(
            ores::database::service::tenant_context::with_tenant(ctx_, tenant_id));
        const auto recorded = recorder.record_engine_errors(report_instance_id, rows);
        if (!recorded.failure.empty()) {
            BOOST_LOG_SEV(lg(), error) << recorded.failure;
            return {};
        }
        BOOST_LOG_SEV(lg(), info) << "Recorded the log of " << result_ids.size()
                                  << " result(s) for report instance " << report_instance_id << ": "
                                  << recorded.stored.errors << " error(s), "
                                  << recorded.stored.warnings << " warning(s), "
                                  << recorded.stored.infos << " info row(s)";
        return recorded.stored;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "The log rows of report instance " << report_instance_id
                                  << " were not recorded: " << e.what();
        return {};
    }
}

void report_execution_handler::collect_results(ores::nats::delivery delivery) {
    const auto& msg = delivery.msg();

    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) {
        std::move(delivery).reject();
        return;
    }

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = ores::utility::codec::json::try_read<collect_compute_results_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode collect_compute_results_request");
        std::move(delivery).commit();
        return;
    }
    const auto& req = *parsed;
    run_token_step_scope step_tokens(run_tokens_, req.report_instance_id);

    BOOST_LOG_SEV(lg(), info) << "collect_compute_results starting | instance="
                              << req.report_instance_id << " batch=" << req.batch_id;

    // One way out of this step, so a run that cannot collect its results is
    // recorded as failed rather than left looking like one still working.
    const auto refuse = [&](const std::string& failure) {
        mark_instance_failed(req.tenant_id, req.report_instance_id, failure);
        wf->fail(failure);
        std::move(delivery).commit();
    };

    try {
        boost::uuids::string_generator sg;
        std::string err;

        // The reads below go to compute as the run rather than as this
        // service: compute's rows belong to the run's tenant, and the run's
        // token is what names it.
        const auto ask = [&](const auto& request, std::string& error) {
            using request_type = std::decay_t<decltype(request)>;
            auto owner = run_token_client(req.tenant_id, req.report_instance_id, false, error);
            std::optional<typename request_type::response_type> response;
            if (owner)
                response = nats_call(*owner, request, error);
            // An expired token costs one exchange and one repeat of the request.
            if (!response && error.find("token_expired") != std::string::npos) {
                error.clear();
                owner = run_token_client(req.tenant_id, req.report_instance_id, true, error);
                if (owner)
                    response = nats_call(*owner, request, error);
            }
            return response;
        };

        ores::compute::messaging::list_by_batch_id_workunits_request units_req;
        units_req.batch_id = sg(req.batch_id);
        const auto units = ask(units_req, err);
        if (!units) {
            refuse("Reading the run's compute workunits failed: " + err);
            return;
        }

        // Every result of every workunit. A failed one is a failure of the run
        // and carries the wrapper's own reason, which is the only account of
        // what went wrong that anyone has.
        std::string output_uri;
        for (const auto& workunit : units->workunits) {
            ores::compute::messaging::list_by_workunit_id_results_request results_req;
            results_req.workunit_id = workunit.id;
            const auto results = ask(results_req, err);
            if (!results) {
                refuse("Reading a compute result failed: " + err);
                return;
            }
            for (const auto& one : results->results) {
                if (one.outcome != compute_outcome_success) {
                    // The node uploads the log it wrote even when the engine
                    // failed, so the run's account of the failure is its own rows
                    // rather than one sentence.
                    const auto recorded = record_run_logs(
                        req.tenant_id, req.report_instance_id, {boost::uuids::to_string(one.id)});
                    refuse(service::run_failure_message(one.outcome, one.error_message, recorded));
                    return;
                }
                if (output_uri.empty())
                    output_uri = one.output_uri;
            }
        }

        if (output_uri.empty()) {
            refuse("The run's compute result names no output, so there is nothing to retrieve.");
            return;
        }

        // The object belongs to compute, so it is read from there and kept
        // under this component's own key: the instance then names an object
        // reporting owns, and a later reader needs no second name for it.
        std::string source_bucket;
        std::string source_key;
        if (!ores::storage::net::storage_paths::split_object_path(
                output_uri, source_bucket, source_key)) {
            refuse(std::format("The compute result names an output {} that is not an object path.",
                               output_uri));
            return;
        }

        const auto run_token_value = run_token(req.tenant_id, req.report_instance_id, err);
        if (run_token_value.empty()) {
            refuse("Retrieving the run's output needs a run token: " + err);
            return;
        }

        ores::storage::net::storage_transfer transfer(http_base_url_, run_token_value);
        const auto body = transfer.download_blob(source_bucket, source_key);

        const auto key = service::output_storage_key(req.report_instance_id);
        transfer.upload_blob(std::string(ores::storage::api::object_keys::ores_bucket), key, body);

        service::report_instance_service inst_svc(
            ores::database::service::tenant_context::with_tenant(ctx_, req.tenant_id));
        auto instance = inst_svc.get_instance(sg(req.report_instance_id));
        if (!instance) {
            refuse("Report instance not found: " + req.report_instance_id);
            return;
        }
        instance->output_storage_key = key;
        inst_svc.save_instance(*instance);

        collect_compute_results_result result;
        result.success = true;
        result.message = std::format("Retrieved {} bytes of run output to {}", body.size(), key);

        BOOST_LOG_SEV(lg(), info) << "collect_compute_results complete | instance="
                                  << req.report_instance_id << " output=" << key
                                  << " bytes=" << body.size();

        wf->complete(ores::utility::codec::json::write(result));
        std::move(delivery).commit();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "collect_compute_results failed: " << e.what();
        mark_instance_failed(req.tenant_id, req.report_instance_id, e.what());
        wf->fail(e.what());
        std::move(delivery).commit();
    }
}

void report_execution_handler::resolve_prepared_input(ores::nats::delivery delivery) {
    const auto& msg = delivery.msg();

    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) {
        std::move(delivery).reject();
        return;
    }

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = ores::utility::codec::json::try_read<resolve_prepared_input_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode resolve_prepared_input_request");
        std::move(delivery).commit();
        return;
    }
    const auto& req = *parsed;

    if (req.prepared_input_key.empty()) {
        wf->fail("resolve_prepared_input: the run configuration names no prepared input");
        std::move(delivery).commit();
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
    wf->complete(ores::utility::codec::json::write(result));
    std::move(delivery).commit();
}


namespace {

/** The reasons a read refused, as one sentence that names each of them. */
std::string reasons_of(const std::vector<std::string>& refusals) {
    std::string reasons;
    for (const auto& refusal : refusals) {
        if (!reasons.empty())
            reasons += " ";
        reasons += refusal;
    }
    return reasons;
}

}

void report_execution_handler::post_process_results(ores::nats::delivery delivery) {
    const auto& msg = delivery.msg();
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) {
        std::move(delivery).reject();
        return;
    }

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = ores::utility::codec::json::try_read<post_process_results_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode post_process_results_request");
        std::move(delivery).commit();
        return;
    }
    const auto& req = *parsed;
    run_token_step_scope step_tokens(run_tokens_, req.report_instance_id);

    BOOST_LOG_SEV(lg(), info) << "post_process_results starting | instance="
                              << req.report_instance_id << " batch=" << req.batch_id;

    const auto refuse = [&](const std::string& failure) {
        mark_instance_failed(req.tenant_id, req.report_instance_id, failure);
        wf->fail(failure);
        std::move(delivery).commit();
    };

    try {
        boost::uuids::string_generator sg;
        std::string err;

        const auto tenant_ctx =
            ores::database::service::tenant_context::with_tenant(ctx_, req.tenant_id);
        service::report_instance_service inst_svc(tenant_ctx);
        const auto instance = inst_svc.get_instance(sg(req.report_instance_id));
        if (!instance) {
            refuse("Report instance not found: " + req.report_instance_id);
            return;
        }

        const auto ask = [&](const auto& request, std::string& error) {
            using request_type = std::decay_t<decltype(request)>;
            auto owner = run_token_client(req.tenant_id, req.report_instance_id, false, error);
            std::optional<typename request_type::response_type> response;
            if (owner)
                response = nats_call(*owner, request, error);
            if (!response && error.find("token_expired") != std::string::npos) {
                error.clear();
                owner = run_token_client(req.tenant_id, req.report_instance_id, true, error);
                if (owner)
                    response = nats_call(*owner, request, error);
            }
            return response;
        };

        ores::compute::messaging::list_by_batch_id_workunits_request units_req;
        units_req.batch_id = sg(req.batch_id);
        const auto units = ask(units_req, err);
        if (!units) {
            refuse("Reading the run's compute workunits failed: " + err);
            return;
        }

        std::vector<std::string> result_ids;
        for (const auto& workunit : units->workunits) {
            ores::compute::messaging::list_by_workunit_id_results_request results_req;
            results_req.workunit_id = workunit.id;
            const auto results = ask(results_req, err);
            if (!results) {
                refuse("Reading a compute result failed: " + err);
                return;
            }
            for (const auto& one : results->results) {
                if (one.outcome != compute_outcome_success) {
                    // The engine's log is written whatever became of the run, so
                    // a failure is stated from its rows here too. This step can
                    // be the first to see the failed result, and it would
                    // otherwise record the node's transcript in place of them.
                    const auto recorded = record_run_logs(
                        req.tenant_id, req.report_instance_id, {boost::uuids::to_string(one.id)});
                    refuse(service::run_failure_message(one.outcome, one.error_message, recorded));
                    return;
                }
                result_ids.push_back(boost::uuids::to_string(one.id));
            }
        }

        // The run's account is written before its rows are loaded, so a run whose
        // rows are refused still states what the engine said.
        record_run_logs(req.tenant_id, req.report_instance_id, result_ids);

        if (result_ids.empty()) {
            refuse("The run's compute batch names no result, so there are no rows to load.");
            return;
        }

        const auto run_token_value = run_token(req.tenant_id, req.report_instance_id, err);
        if (run_token_value.empty()) {
            refuse("Loading the run's rows needs a run token: " + err);
            return;
        }

        // One result is one run of the engine, and the node uploaded each file it
        // wrote as its own object, with a manifest that names them. The readers
        // know each file's family, so this fetches the result files by name and
        // leaves the rest where they are.
        const auto as_of = instance->priced_at.value_or(std::chrono::system_clock::now());

        // The definition chose the families the run stores. A skipped family's
        // files stay in the object store and are not fetched.
        ores::ore::results::family_selection families;
        try {
            families = ores::ore::results::family_selection_of(req.skipped_families);
        } catch (const std::invalid_argument& e) {
            refuse(std::string("The run skips a family that does not exist: ") + e.what());
            return;
        }

        // One transaction, written a block at a time: a refusal anywhere rolls
        // back every block, so a partial load is never seen.
        ores::database::repository::unit_of_work uow(tenant_ctx);
        const auto& load_ctx = uow.ctx();
        std::size_t metrics_loaded = 0;
        std::size_t cashflows_loaded = 0;
        std::size_t text_values_loaded = 0;
        const results::run_output_sinks sinks{
            .metrics =
                [&](std::vector<domain::report_metric>& block) {
                    repository::report_metric_repository{}.insert(load_ctx, block);
                    metrics_loaded += block.size();
                },
            .cashflows =
                [&](std::vector<domain::report_cashflow>& block) {
                    repository::report_cashflow_repository{}.insert(load_ctx, block);
                    cashflows_loaded += block.size();
                },
            .text_values =
                [&](std::vector<domain::report_text_value>& block) {
                    repository::report_text_value_repository{}.insert(load_ctx, block);
                    text_values_loaded += block.size();
                }};

        std::vector<std::string> manifest_uris;
        ores::storage::net::storage_transfer transfer(http_base_url_, run_token_value);
        const std::string bucket(ores::compute::net::compute_storage::bucket);
        for (const auto& result_id : result_ids) {
            const auto manifest_key =
                ores::compute::net::compute_storage::output_manifest_key(result_id);
            manifest_uris.push_back(
                ores::storage::net::storage_paths::make_object_path(bucket, manifest_key));

            const results::run_stamp run{.party_id = instance->party_id,
                                         .report_instance_id = sg(req.report_instance_id),
                                         .report_definition_id = instance->definition_id,
                                         .result_id = sg(result_id),
                                         .priced_at = as_of,
                                         .families = families};
            results::run_output_outcome read;
            try {
                const auto text = transfer.download_blob(bucket, manifest_key);
                const auto manifest = ores::ore::results::manifest_from_json(
                    std::string_view(text.data(), text.size()));
                if (manifest.result_id != result_id)
                    throw std::runtime_error("its manifest states result " + manifest.result_id);
                for (const auto& file : ores::ore::results::located_files(manifest, manifest_key)) {
                    if (!ores::ore::results::is_result_file(file))
                        continue;
                    results::output_file one{.name = file.name};
                    if (families.stores(file.family)) {
                        {
                            const auto body = transfer.download_blob(bucket, file.key);
                            one.body.assign(body.begin(), body.end());
                        }
                        ores::ore::results::verify_body(file, one.body);
                    }
                    results::read_run_output(one, run, load_block_size, sinks, read);
                    if (!read.refusals.empty())
                        break;
                }
            } catch (const std::exception& failure) {
                refuse(std::format(
                    "The output of result {} was not read: {}", result_id, failure.what()));
                return;
            }
            if (!read.refusals.empty()) {
                refuse("The run's output was refused: " + reasons_of(read.refusals));
                return;
            }
            BOOST_LOG_SEV(lg(), info)
                << "Read " << read.files_read.size() << " output file(s) of result " << result_id;
            for (const auto& skipped : read.files_skipped)
                BOOST_LOG_SEV(lg(), info) << "Skipped " << skipped << " of result " << result_id
                                          << ": its definition does not store the family";
        }

        const auto loaded = metrics_loaded + cashflows_loaded + text_values_loaded;
        if (loaded == 0) {
            refuse("The run's output states no figure, so there is nothing to load.");
            return;
        }
        uow.commit();
        BOOST_LOG_SEV(lg(), info) << "Loaded " << metrics_loaded << " metric(s), "
                                  << cashflows_loaded << " cashflow figure(s) and "
                                  << text_values_loaded << " text value(s) in blocks of "
                                  << load_block_size;

        // The market state the run priced against belongs to the market data
        // store, and it is far too large to travel in a message: the manifests
        // are named and the run's own token is lent, so marketdata reads them.
        // A state that is not stored leaves the run's figures intact, so it is
        // stated and carried on with rather than failing the run.
        std::string market_state_note;
        try {
            ores::marketdata::messaging::store_market_state_request state;
            state.party_id = boost::uuids::to_string(instance->party_id);
            state.report_instance_id = req.report_instance_id;
            state.report_definition_id = boost::uuids::to_string(instance->definition_id);
            state.as_of = std::format("{:%F}", std::chrono::floor<std::chrono::days>(as_of));
            state.storage_token = run_token_value;
            state.manifest_uris = manifest_uris;
            state.skipped_families = families.skipped;

            std::string state_error;
            auto owner = svc_nats_.with_delegation(run_token_value);
            const auto answer = nats_call(owner, state, state_error);
            if (!answer) {
                market_state_note = "Its market state was not stored: " + state_error;
            } else if (answer->result.outcome != ores::utility::domain::outcome::ok) {
                market_state_note = "Its market state was not stored: " + answer->result.message;
            } else {
                BOOST_LOG_SEV(lg(), info)
                    << "Stored " << answer->rows << " market-state value(s) from " << answer->files
                    << " file(s) for " << req.report_instance_id;
            }
        } catch (const std::exception& e) {
            market_state_note = std::string("Its market state was not stored: ") + e.what();
        }
        if (!market_state_note.empty()) {
            BOOST_LOG_SEV(lg(), error) << market_state_note;
            market_state_note = " " + market_state_note;
        }

        post_process_results_result result;
        result.success = true;
        result.rows_loaded = static_cast<int>(loaded);
        result.message =
            std::format("Loaded {} metrics, {} cashflow figures and {} text values for run {}.{}",
                        metrics_loaded,
                        cashflows_loaded,
                        text_values_loaded,
                        req.report_instance_id,
                        market_state_note);

        BOOST_LOG_SEV(lg(), info) << "post_process_results complete | instance="
                                  << req.report_instance_id << " rows=" << loaded;

        wf->complete(ores::utility::codec::json::write(result));
        std::move(delivery).commit();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "post_process_results failed: " << e.what();
        mark_instance_failed(req.tenant_id, req.report_instance_id, e.what());
        wf->fail(e.what());
        std::move(delivery).commit();
    }
}

void report_execution_handler::ignore_compute_results(ores::nats::delivery delivery) {
    const auto& msg = delivery.msg();

    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) {
        std::move(delivery).reject();
        return;
    }

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = ores::utility::codec::json::try_read<ignore_compute_results_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode ignore_compute_results_request");
        std::move(delivery).commit();
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "ignore_compute_results complete | instance="
                              << req.report_instance_id << " batch=" << req.batch_id
                              << " (results not ingested)";

    collect_compute_results_result result;
    result.success = true;
    result.message = std::format("Results of batch {} left unread by configuration.", req.batch_id);
    wf->complete(ores::utility::codec::json::write(result));
    std::move(delivery).commit();
}

void report_execution_handler::finalise(ores::nats::delivery delivery) {
    const auto& msg = delivery.msg();

    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) {
        std::move(delivery).reject();
        return;
    }

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = ores::utility::codec::json::try_read<finalise_report_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode finalise_report_request");
        std::move(delivery).commit();
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
            std::move(delivery).commit();
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
        wf->complete(ores::utility::codec::json::write(result));
        std::move(delivery).commit();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "finalise failed: " << e.what();
        wf->fail(e.what());
        std::move(delivery).commit();
    }
}

void report_execution_handler::fail(ores::nats::delivery delivery) {
    const auto& msg = delivery.msg();

    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) {
        std::move(delivery).reject();
        return;
    }

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = ores::utility::codec::json::try_read<fail_report_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode fail_report_request");
        std::move(delivery).commit();
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
        wf->complete(ores::utility::codec::json::write(result));
        std::move(delivery).commit();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "fail handler error: " << e.what();
        wf->fail(e.what());
        std::move(delivery).commit();
    }
}

}
