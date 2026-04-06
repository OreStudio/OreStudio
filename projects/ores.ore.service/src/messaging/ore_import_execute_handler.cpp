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
#include "ores.ore.service/messaging/ore_import_execute_handler.hpp"

#include <format>
#include <set>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.storage/net/storage_transfer.hpp"
#include "ores.ore.api/net/ore_storage.hpp"
#include "ores.ore.api/messaging/ore_import_engine_protocol.hpp"
#include "ores.ore/scanner/ore_directory_scanner.hpp"
#include "ores.ore/planner/ore_import_planner.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.api/messaging/portfolio_protocol.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"

namespace ores::ore::service::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

namespace {

/**
 * @brief Makes an authenticated NATS request and deserialises the response.
 *
 * Returns nullopt and populates out_error on any error.
 */
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

ore_import_execute_handler::ore_import_execute_handler(
    ores::nats::service::client& nats,
    ores::nats::service::nats_client outbound_nats,
    std::string http_base_url,
    std::string work_dir)
    : nats_(nats)
    , outbound_nats_(std::move(outbound_nats))
    , http_base_url_(std::move(http_base_url))
    , work_dir_(std::move(work_dir)) {}

void ore_import_execute_handler::execute(ores::nats::message msg) {
    using ores::ore::messaging::ore_import_execute_request;
    using ores::ore::messaging::ore_import_execute_result;

    const auto step_id = extract_workflow_header(msg, workflow_step_id_header);
    const auto inst_id = extract_workflow_header(msg, workflow_instance_id_header);

    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<ore_import_execute_request>(sv);
    if (!parsed) {
        BOOST_LOG_SEV(lg(), error) << "ore.import.execute: failed to decode request | step="
                                   << step_id;
        publish_step_completion(nats_, step_id, inst_id, false, "",
            "Failed to decode ore_import_execute_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute starting | corr=" << req.correlation_id
                              << " request_id=" << req.request_id
                              << " step=" << step_id;

    auto delegated_nats = outbound_nats_
        .with_delegation(req.bearer_token)
        .with_correlation_id(req.correlation_id);

    ore_import_execute_result result;
    result.correlation_id = req.correlation_id;

    // -------------------------------------------------------------------------
    // Step 0: fetch tarball from storage and extract to work directory
    // -------------------------------------------------------------------------
    const auto import_dir = work_dir_ / req.request_id;
    BOOST_LOG_SEV(lg(), debug) << "ore.import.execute step 0: fetch_and_unpack | corr="
                               << req.correlation_id
                               << " bucket=" << ores::ore::net::ore_storage::bucket
                               << " key=" << ores::ore::net::ore_storage::import_key(req.request_id)
                               << " dest=" << import_dir.string();

    try {
        std::filesystem::create_directories(import_dir);
        ores::storage::net::storage_transfer transfer(http_base_url_);
        transfer.fetch_and_unpack(
            std::string(ores::ore::net::ore_storage::bucket),
            ores::ore::net::ore_storage::import_key(req.request_id),
            import_dir);
    } catch (const std::exception& e) {
        const auto failure = std::format("fetch_and_unpack failed: {}", e.what());
        BOOST_LOG_SEV(lg(), error) << "ore.import.execute step 0 failed | corr="
                                   << req.correlation_id << " error=" << failure;
        publish_step_completion(nats_, step_id, inst_id, false, "", failure);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 0 complete | corr="
                              << req.correlation_id
                              << " import_dir=" << import_dir.string();

    // -------------------------------------------------------------------------
    // Step 1: scan the extracted directory
    // -------------------------------------------------------------------------
    ores::ore::scanner::scan_result scan;
    try {
        ores::ore::scanner::ore_directory_scanner scanner(import_dir);
        scan = scanner.scan();
    } catch (const std::exception& e) {
        const auto failure = std::format("Directory scan failed: {}", e.what());
        BOOST_LOG_SEV(lg(), error) << "ore.import.execute step 1 failed | corr="
                                   << req.correlation_id << " error=" << failure;
        publish_step_completion(nats_, step_id, inst_id, false, "", failure);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 1 complete | corr="
                              << req.correlation_id
                              << " currency_files=" << scan.currency_files.size()
                              << " portfolio_files=" << scan.portfolio_files.size()
                              << " ignored_files=" << scan.ignored_files.size();

    // -------------------------------------------------------------------------
    // Step 2: list existing currency ISO codes
    // -------------------------------------------------------------------------
    std::set<std::string> existing_iso_codes;
    {
        ores::refdata::messaging::get_currencies_request list_req;
        constexpr int max_currency_fetch = 10'000;
        list_req.offset = 0;
        list_req.limit = max_currency_fetch;
        std::string err;
        auto list_resp = nats_call(delegated_nats, list_req, err);
        if (!list_resp) {
            BOOST_LOG_SEV(lg(), error) << "ore.import.execute step 2 failed | corr="
                                       << req.correlation_id << " error=" << err;
            publish_step_completion(nats_, step_id, inst_id, false, "", err);
            return;
        }
        for (const auto& c : list_resp->currencies)
            existing_iso_codes.insert(c.iso_code);
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 2 complete | corr="
                              << req.correlation_id
                              << " existing_iso_codes=" << existing_iso_codes.size();

    // -------------------------------------------------------------------------
    // Step 3: build the import plan
    // -------------------------------------------------------------------------
    ores::ore::planner::import_choices choices;
    if (!req.import_choices_json.empty()) {
        auto parsed_choices = rfl::json::read<ores::ore::planner::import_choices>(
            req.import_choices_json);
        if (!parsed_choices) {
            const auto failure = std::format("Failed to parse import_choices_json: {}",
                parsed_choices.error().what());
            BOOST_LOG_SEV(lg(), error) << "ore.import.execute step 3 failed | corr="
                                       << req.correlation_id << " error=" << failure;
            publish_step_completion(nats_, step_id, inst_id, false, "", failure);
            return;
        }
        choices = std::move(*parsed_choices);
    }

    ores::ore::planner::ore_import_plan plan;
    try {
        ores::ore::planner::ore_import_planner planner(
            std::move(scan), std::move(existing_iso_codes), std::move(choices));
        plan = planner.plan();
    } catch (const std::exception& e) {
        const auto failure = std::format("Import plan failed: {}", e.what());
        BOOST_LOG_SEV(lg(), error) << "ore.import.execute step 3 failed | corr="
                                   << req.correlation_id << " error=" << failure;
        publish_step_completion(nats_, step_id, inst_id, false, "", failure);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 3 complete | corr="
                              << req.correlation_id
                              << " currencies=" << plan.currencies.size()
                              << " portfolios=" << plan.portfolios.size()
                              << " books=" << plan.books.size()
                              << " trades=" << plan.trades.size();

    // -------------------------------------------------------------------------
    // Step 4: save currencies
    // -------------------------------------------------------------------------
    for (auto& currency : plan.currencies) {
        const auto iso = currency.iso_code;
        ores::refdata::messaging::save_currency_request save_req;
        save_req.data = std::move(currency);
        std::string err;
        auto resp = nats_call(delegated_nats, save_req, err);
        if (!resp || !resp->success) {
            const auto failure = err.empty()
                ? std::format("save_currency failed for {}: {}",
                    iso, resp ? resp->message : "(no response)")
                : err;
            BOOST_LOG_SEV(lg(), error) << "ore.import.execute step 4 failed | corr="
                                       << req.correlation_id
                                       << " iso_code=" << iso << " error=" << failure;
            publish_step_completion(nats_, step_id, inst_id, false, "", failure);
            return;
        }
        result.saved_currency_iso_codes.push_back(iso);
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 4 complete | corr="
                              << req.correlation_id
                              << " saved=" << result.saved_currency_iso_codes.size();

    // -------------------------------------------------------------------------
    // Step 5: save portfolios (parents-first from planner)
    // -------------------------------------------------------------------------
    for (auto& portfolio : plan.portfolios) {
        const auto pid = boost::uuids::to_string(portfolio.id);
        const auto name = portfolio.name;
        ores::refdata::messaging::save_portfolio_request save_req;
        save_req.data = std::move(portfolio);
        std::string err;
        auto resp = nats_call(delegated_nats, save_req, err);
        if (!resp || !resp->success) {
            const auto failure = err.empty()
                ? std::format("save_portfolio failed for '{}' ({}): {}",
                    name, pid, resp ? resp->message : "(no response)")
                : err;
            BOOST_LOG_SEV(lg(), error) << "ore.import.execute step 5 failed | corr="
                                       << req.correlation_id
                                       << " portfolio=" << pid << " error=" << failure;
            publish_step_completion(nats_, step_id, inst_id, false, "", failure);
            return;
        }
        result.saved_portfolio_ids.push_back(pid);
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 5 complete | corr="
                              << req.correlation_id
                              << " saved=" << result.saved_portfolio_ids.size();

    // -------------------------------------------------------------------------
    // Step 6: save books
    // -------------------------------------------------------------------------
    for (auto& book : plan.books) {
        const auto bid = boost::uuids::to_string(book.id);
        const auto name = book.name;
        ores::refdata::messaging::save_book_request save_req;
        save_req.data = std::move(book);
        std::string err;
        auto resp = nats_call(delegated_nats, save_req, err);
        if (!resp || !resp->success) {
            const auto failure = err.empty()
                ? std::format("save_book failed for '{}' ({}): {}",
                    name, bid, resp ? resp->message : "(no response)")
                : err;
            BOOST_LOG_SEV(lg(), error) << "ore.import.execute step 6 failed | corr="
                                       << req.correlation_id
                                       << " book=" << bid << " error=" << failure;
            publish_step_completion(nats_, step_id, inst_id, false, "", failure);
            return;
        }
        result.saved_book_ids.push_back(bid);
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 6 complete | corr="
                              << req.correlation_id
                              << " saved=" << result.saved_book_ids.size();

    // -------------------------------------------------------------------------
    // Step 7: save trades (failures collected; saga continues)
    // -------------------------------------------------------------------------
    for (auto& item : plan.trades) {
        const auto tid = boost::uuids::to_string(item.trade.id);
        const auto src = item.source_file.string();
        const auto ext_id = item.trade.external_id;

        ores::trading::messaging::save_trade_request save_req;
        save_req.trades = {std::move(item.trade)};

        std::string trade_error;
        auto resp = nats_call(delegated_nats, save_req, trade_error);
        if (!resp || !resp->success) {
            const auto trade_msg = resp ? resp->message : trade_error;
            BOOST_LOG_SEV(lg(), warn) << "ore.import.execute trade save failed | corr="
                                      << req.correlation_id
                                      << " trade_id=" << tid
                                      << " source=" << src
                                      << " error=" << trade_msg;
            result.item_errors.push_back({
                .source_file = src,
                .item_id = ext_id,
                .message = trade_msg});
        } else {
            result.saved_trade_ids.push_back(tid);
        }
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute step 7 complete | corr="
                              << req.correlation_id
                              << " saved=" << result.saved_trade_ids.size()
                              << " failed=" << result.item_errors.size();

    // -------------------------------------------------------------------------
    // Done — publish result
    // -------------------------------------------------------------------------
    result.success = true;
    result.message = "ORE import completed.";

    BOOST_LOG_SEV(lg(), info) << "ore.import.execute complete | corr=" << req.correlation_id
                              << " currencies=" << result.saved_currency_iso_codes.size()
                              << " portfolios=" << result.saved_portfolio_ids.size()
                              << " books=" << result.saved_book_ids.size()
                              << " trades=" << result.saved_trade_ids.size()
                              << " item_errors=" << result.item_errors.size();

    publish_step_completion(nats_, step_id, inst_id, true,
        rfl::json::write(result), "");
}

void ore_import_execute_handler::rollback(ores::nats::message msg) {
    using ores::ore::messaging::ore_import_rollback_request;

    const auto step_id = extract_workflow_header(msg, workflow_step_id_header);
    const auto inst_id = extract_workflow_header(msg, workflow_instance_id_header);

    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<ore_import_rollback_request>(sv);
    if (!parsed) {
        BOOST_LOG_SEV(lg(), error) << "ore.import.rollback: failed to decode request | step="
                                   << step_id;
        publish_step_completion(nats_, step_id, inst_id, false, "",
            "Failed to decode ore_import_rollback_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "ore.import.rollback starting | corr=" << req.correlation_id
                              << " step=" << step_id;

    auto delegated_nats = outbound_nats_
        .with_delegation(req.bearer_token)
        .with_correlation_id(req.correlation_id);

    // ── Delete trades ────────────────────────────────────────────────────────
    if (!req.saved_trade_ids.empty()) {
        BOOST_LOG_SEV(lg(), info) << "ore.import.rollback: delete trades | corr="
                                  << req.correlation_id
                                  << " count=" << req.saved_trade_ids.size();
        ores::trading::messaging::delete_trade_request del_req;
        del_req.ids = req.saved_trade_ids;
        std::string err;
        auto r = nats_call(delegated_nats, del_req, err);
        if (!r || !r->success) {
            const auto reason = (r && !r->message.empty()) ? r->message : err;
            BOOST_LOG_SEV(lg(), error) << "ore.import.rollback delete_trades failed | corr="
                                       << req.correlation_id << " error=" << reason;
        }
    }

    // ── Delete books ─────────────────────────────────────────────────────────
    if (!req.saved_book_ids.empty()) {
        BOOST_LOG_SEV(lg(), info) << "ore.import.rollback: delete books | corr="
                                  << req.correlation_id
                                  << " count=" << req.saved_book_ids.size();
        ores::refdata::messaging::delete_book_request del_req;
        del_req.ids = req.saved_book_ids;
        std::string err;
        auto r = nats_call(delegated_nats, del_req, err);
        if (!r || !r->success) {
            const auto reason = (r && !r->message.empty()) ? r->message : err;
            BOOST_LOG_SEV(lg(), error) << "ore.import.rollback delete_books failed | corr="
                                       << req.correlation_id << " error=" << reason;
        }
    }

    // ── Delete portfolios (reverse order — children before parents) ──────────
    if (!req.saved_portfolio_ids.empty()) {
        BOOST_LOG_SEV(lg(), info) << "ore.import.rollback: delete portfolios | corr="
                                  << req.correlation_id
                                  << " count=" << req.saved_portfolio_ids.size();
        ores::refdata::messaging::delete_portfolio_request del_req;
        del_req.ids = std::vector<std::string>(
            req.saved_portfolio_ids.rbegin(), req.saved_portfolio_ids.rend());
        std::string err;
        auto r = nats_call(delegated_nats, del_req, err);
        if (!r || !r->success) {
            const auto reason = (r && !r->message.empty()) ? r->message : err;
            BOOST_LOG_SEV(lg(), error) << "ore.import.rollback delete_portfolios failed | corr="
                                       << req.correlation_id << " error=" << reason;
        }
    }

    // ── Delete currencies ────────────────────────────────────────────────────
    if (!req.saved_currency_iso_codes.empty()) {
        BOOST_LOG_SEV(lg(), info) << "ore.import.rollback: delete currencies | corr="
                                  << req.correlation_id
                                  << " count=" << req.saved_currency_iso_codes.size();
        ores::refdata::messaging::delete_currency_request del_req;
        del_req.iso_codes = req.saved_currency_iso_codes;
        std::string err;
        auto r = nats_call(delegated_nats, del_req, err);
        if (!r || !r->success) {
            const auto reason = (r && !r->message.empty()) ? r->message : err;
            BOOST_LOG_SEV(lg(), error) << "ore.import.rollback delete_currencies failed | corr="
                                       << req.correlation_id << " error=" << reason;
        }
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import.rollback complete | corr=" << req.correlation_id;
    publish_step_completion(nats_, step_id, inst_id, true, "{\"success\":true}", "");
}

}
