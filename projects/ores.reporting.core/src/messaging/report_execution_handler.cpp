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

#include <chrono>
#include <format>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.storage/net/storage_transfer.hpp"
#include "ores.reporting.api/messaging/report_execution_protocol.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"
#include "ores.reporting.core/repository/risk_report_config_repository.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"

namespace ores::reporting::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

namespace {

template<typename Req>
std::optional<typename Req::response_type>
nats_call(ores::nats::service::nats_client& nats, const Req& request,
    std::string& out_error) {
    using Resp = typename Req::response_type;
    try {
        const auto json = rfl::json::write(request);
        const auto msg = nats.authenticated_request(Req::nats_subject, json);

        const auto err_it = msg.headers.find("X-Error");
        if (err_it != msg.headers.end()) {
            out_error = std::format("Service error on {}: {}",
                Req::nats_subject, err_it->second);
            return std::nullopt;
        }
        const std::string_view sv(
            reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
        auto result = rfl::json::read<Resp>(sv);
        if (!result) {
            out_error = std::format("Failed to parse response from {}: {}",
                Req::nats_subject, result.error().what());
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        out_error = std::format("Exception calling {}: {}",
            Req::nats_subject, e.what());
        return std::nullopt;
    }
}

} // namespace

namespace {

constexpr std::string_view report_data_bucket = "report-data";

std::string trades_storage_key(const std::string& instance_id) {
    return instance_id + "/trades.msgpack";
}

} // namespace

report_execution_handler::report_execution_handler(
    ores::nats::service::client& nats,
    ores::database::context ctx,
    ores::nats::service::nats_client svc_nats,
    ores::workflow::service::fsm_state_map instance_states,
    std::string http_base_url)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , svc_nats_(std::move(svc_nats))
    , instance_states_(std::move(instance_states))
    , http_base_url_(std::move(http_base_url)) {}

void report_execution_handler::gather_trades(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) return;

    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<gather_trades_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode gather_trades_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "gather_trades starting | instance="
                              << req.report_instance_id
                              << " definition=" << req.definition_id;

    try {
        auto tenant_ctx = ores::database::service::tenant_context::with_tenant(
            ctx_, req.tenant_id);

        // ── Load risk_report_config for book/portfolio scope ─────────
        repository::risk_report_config_repository config_repo;
        const auto config = config_repo.find_by_definition_id(
            tenant_ctx, req.definition_id);
        if (!config) {
            wf->fail("risk_report_config not found for definition "
                + req.definition_id);
            return;
        }

        const auto config_id = boost::uuids::to_string(config->id);

        // ── Resolve scope and fetch trades ───────────────────────────
        std::vector<ores::trading::messaging::trade_export_item> all_items;
        const auto book_ids = config_repo.get_book_scope(tenant_ctx, config_id);

        if (!book_ids.empty()) {
            // Explicit book scope — fetch per book.
            for (const auto& bid : book_ids) {
                try {
                    ores::trading::messaging::export_portfolio_request exp_req;
                    exp_req.book_id = bid;
                    std::string err;
                    auto resp = nats_call(svc_nats_, exp_req, err);
                    if (!resp || !resp->success) {
                        const auto failure = err.empty() ? resp->message : err;
                        BOOST_LOG_SEV(lg(), warn)
                            << "gather_trades: export failed for book " << bid
                            << ": " << failure;
                        continue;
                    }
                    all_items.insert(all_items.end(),
                        std::make_move_iterator(resp->items.begin()),
                        std::make_move_iterator(resp->items.end()));
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(lg(), warn)
                        << "gather_trades: exception exporting book " << bid
                        << ": " << e.what();
                }
            }
        } else {
            // No book scope — try portfolio scope, fetching trades directly.
            const auto portfolio_ids = config_repo.get_portfolio_scope(
                tenant_ctx, config_id);
            for (const auto& pid : portfolio_ids) {
                try {
                    ores::trading::messaging::export_portfolio_request exp_req;
                    exp_req.portfolio_id = pid;
                    std::string err;
                    auto resp = nats_call(svc_nats_, exp_req, err);
                    if (!resp || !resp->success) {
                        const auto failure = err.empty() ? resp->message : err;
                        BOOST_LOG_SEV(lg(), warn)
                            << "gather_trades: export failed for portfolio "
                            << pid << ": " << failure;
                        continue;
                    }
                    all_items.insert(all_items.end(),
                        std::make_move_iterator(resp->items.begin()),
                        std::make_move_iterator(resp->items.end()));
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(lg(), warn)
                        << "gather_trades: exception exporting portfolio "
                        << pid << ": " << e.what();
                }
            }

            if (portfolio_ids.empty()) {
                // No scope at all — means "all trades for tenant".
                // Fetch via a single unscoped export call.
                BOOST_LOG_SEV(lg(), info)
                    << "gather_trades: no book/portfolio scope; "
                    << "fetching all tenant trades";
                ores::trading::messaging::export_portfolio_request exp_req;
                std::string err;
                auto resp = nats_call(svc_nats_, exp_req, err);
                if (resp && resp->success) {
                    all_items = std::move(resp->items);
                }
            }
        }

        // ── Update instance state to running ─────────────────────────
        service::report_instance_service inst_svc(tenant_ctx);
        auto inst = inst_svc.find_instance(req.report_instance_id);
        if (!inst) {
            wf->fail("Report instance not found: " + req.report_instance_id);
            return;
        }
        inst->fsm_state_id = instance_states_.require("running");
        inst_svc.save_instance(*inst);

        // ── Serialise to MsgPack and upload to storage ────────────────
        const auto key = trades_storage_key(req.report_instance_id);
        {
            const auto blob = rfl::msgpack::write(all_items);
            ores::storage::net::storage_transfer transfer(http_base_url_);
            transfer.upload_blob(
                std::string(report_data_bucket), key, blob);
        }

        // ── Build result ─────────────────────────────────────────────
        gather_trades_result result;
        result.success = true;
        result.trade_count = static_cast<int>(all_items.size());
        result.storage_key = key;
        result.message = std::format("Gathered {} trades from {} books",
            all_items.size(), book_ids.size());

        BOOST_LOG_SEV(lg(), info) << "gather_trades complete | instance="
                                  << req.report_instance_id
                                  << " trades=" << all_items.size()
                                  << " books=" << book_ids.size();

        wf->complete(rfl::json::write(result));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "gather_trades failed: " << e.what();
        wf->fail(e.what());
    }
}

void report_execution_handler::assemble_bundle(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) return;

    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<assemble_bundle_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode assemble_bundle_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "assemble_bundle starting | instance="
                              << req.report_instance_id;

    // Stub: in Phase 3.6 this will persist a report_input_bundle entity.
    // For now, pass through successfully so finalise can run.
    assemble_bundle_result result;
    result.success = true;
    result.message = "Bundle assembly not yet implemented; passing through.";
    result.bundle_ref = req.report_instance_id;

    BOOST_LOG_SEV(lg(), info) << "assemble_bundle complete (stub) | instance="
                              << req.report_instance_id;
    wf->complete(rfl::json::write(result));
}

void report_execution_handler::finalise(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) return;

    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<finalise_report_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode finalise_report_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "finalise starting | instance="
                              << req.report_instance_id;

    try {
        auto tenant_ctx = ores::database::service::tenant_context::with_tenant(
            ctx_, req.tenant_id);

        service::report_instance_service inst_svc(tenant_ctx);
        auto inst = inst_svc.find_instance(req.report_instance_id);
        if (!inst) {
            wf->fail("Report instance not found: " + req.report_instance_id);
            return;
        }

        inst->fsm_state_id = instance_states_.require("completed");
        inst->completed_at = std::chrono::system_clock::now();
        inst->output_message = "Report execution completed.";
        inst_svc.save_instance(*inst);

        BOOST_LOG_SEV(lg(), info) << "finalise complete | instance="
                                  << req.report_instance_id;

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
    if (!wf) return;

    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<fail_report_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode fail_report_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "fail (compensation) | instance="
                              << req.report_instance_id
                              << " error=" << req.error_message;

    try {
        auto tenant_ctx = ores::database::service::tenant_context::with_tenant(
            ctx_, req.tenant_id);

        service::report_instance_service inst_svc(tenant_ctx);
        auto inst = inst_svc.find_instance(req.report_instance_id);
        if (!inst) {
            wf->fail("Report instance not found: " + req.report_instance_id);
            return;
        }

        inst->fsm_state_id = instance_states_.require("failed");
        inst->completed_at = std::chrono::system_clock::now();
        inst->output_message = req.error_message;
        inst_svc.save_instance(*inst);

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
