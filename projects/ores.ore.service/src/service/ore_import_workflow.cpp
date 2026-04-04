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
#include "ores.ore.service/service/ore_import_workflow.hpp"

#include <format>
#include <set>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.storage/net/storage_transfer.hpp"
#include "ores.ore.api/net/ore_storage.hpp"
#include "ores.ore/scanner/ore_directory_scanner.hpp"
#include "ores.ore/planner/ore_import_planner.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.api/messaging/portfolio_protocol.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"

namespace ores::ore::service::service {

using namespace ores::logging;

namespace {

inline static std::string_view logger_name =
    "ores.ore.service.service.ore_import_workflow";

static auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

/**
 * @brief Makes an authenticated NATS request and deserializes the response.
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

ore_import_workflow::ore_import_workflow(boost::uuids::uuid workflow_id,
    ores::ore::messaging::ore_import_request request,
    std::string correlation_id,
    std::string http_base_url,
    std::string work_dir)
    : workflow_id_(workflow_id)
    , request_(std::move(request))
    , correlation_id_(std::move(correlation_id))
    , http_base_url_(std::move(http_base_url))
    , work_dir_(std::move(work_dir)) {}

bool ore_import_workflow::execute(ores::database::context /*ctx*/,
    ores::nats::service::nats_client& nats) {

    BOOST_LOG_SEV(lg(), info) << "ore_import_workflow starting | corr="
                              << correlation_id_
                              << " request_id=" << request_.request_id
                              << " workflow_id=" << boost::uuids::to_string(workflow_id_);

    // -------------------------------------------------------------------------
    // Step 0: fetch tarball from storage and extract to work directory
    // -------------------------------------------------------------------------
    const auto import_dir = work_dir_ / request_.request_id;
    BOOST_LOG_SEV(lg(), debug) << "ore_import_workflow step 0: fetch_and_unpack | corr="
                               << correlation_id_
                               << " bucket=" << ores::ore::net::ore_storage::bucket
                               << " key=" << ores::ore::net::ore_storage::import_key(request_.request_id)
                               << " dest=" << import_dir.string();

    try {
        std::filesystem::create_directories(import_dir);
        ores::storage::net::storage_transfer transfer(http_base_url_);
        transfer.fetch_and_unpack(
            std::string(ores::ore::net::ore_storage::bucket),
            ores::ore::net::ore_storage::import_key(request_.request_id),
            import_dir);
    } catch (const std::exception& e) {
        error_ = std::format("fetch_and_unpack failed: {}", e.what());
        BOOST_LOG_SEV(lg(), error) << "ore_import_workflow step 0 failed | corr="
                                   << correlation_id_ << " error=" << error_;
        return false;
    }

    BOOST_LOG_SEV(lg(), info) << "ore_import_workflow step 0 complete | corr="
                              << correlation_id_
                              << " import_dir=" << import_dir.string();

    // -------------------------------------------------------------------------
    // Step 1: scan the extracted directory
    // -------------------------------------------------------------------------
    BOOST_LOG_SEV(lg(), debug) << "ore_import_workflow step 1: scan | corr="
                               << correlation_id_
                               << " root=" << import_dir.string();

    ores::ore::scanner::scan_result scan;
    try {
        ores::ore::scanner::ore_directory_scanner scanner(import_dir);
        scan = scanner.scan();
    } catch (const std::exception& e) {
        error_ = std::format("Directory scan failed: {}", e.what());
        BOOST_LOG_SEV(lg(), error) << "ore_import_workflow step 1 failed | corr="
                                   << correlation_id_ << " error=" << error_;
        return false;
    }

    BOOST_LOG_SEV(lg(), info) << "ore_import_workflow step 1 complete | corr="
                              << correlation_id_
                              << " currency_files=" << scan.currency_files.size()
                              << " portfolio_files=" << scan.portfolio_files.size()
                              << " ignored_files=" << scan.ignored_files.size();

    // -------------------------------------------------------------------------
    // Step 2: list existing currency ISO codes
    // -------------------------------------------------------------------------
    BOOST_LOG_SEV(lg(), debug) << "ore_import_workflow step 2: list currencies | corr="
                               << correlation_id_;

    std::set<std::string> existing_iso_codes;
    {
        ores::refdata::messaging::get_currencies_request list_req;
        // Currencies are a small reference dataset; this limit covers any realistic deployment.
        constexpr int max_currency_fetch = 10'000;
        list_req.offset = 0;
        list_req.limit = max_currency_fetch;
        auto list_resp = nats_call(nats, list_req, error_);
        if (!list_resp) {
            BOOST_LOG_SEV(lg(), error) << "ore_import_workflow step 2 failed | corr="
                                       << correlation_id_ << " error=" << error_;
            return false;
        }
        for (const auto& c : list_resp->currencies)
            existing_iso_codes.insert(c.iso_code);
    }

    BOOST_LOG_SEV(lg(), info) << "ore_import_workflow step 2 complete | corr="
                              << correlation_id_
                              << " existing_iso_codes=" << existing_iso_codes.size();

    // -------------------------------------------------------------------------
    // Step 3: build the import plan
    // -------------------------------------------------------------------------
    BOOST_LOG_SEV(lg(), debug) << "ore_import_workflow step 3: build plan | corr="
                               << correlation_id_;

    ores::ore::planner::import_choices choices;
    if (!request_.import_choices_json.empty()) {
        auto parsed = rfl::json::read<ores::ore::planner::import_choices>(
            request_.import_choices_json);
        if (!parsed) {
            error_ = std::format("Failed to parse import_choices_json: {}",
                parsed.error().what());
            BOOST_LOG_SEV(lg(), error) << "ore_import_workflow step 3 failed | corr="
                                       << correlation_id_ << " error=" << error_;
            return false;
        }
        choices = std::move(*parsed);
    }

    ores::ore::planner::ore_import_plan plan;
    try {
        ores::ore::planner::ore_import_planner planner(
            std::move(scan), std::move(existing_iso_codes), std::move(choices));
        plan = planner.plan();
    } catch (const std::exception& e) {
        error_ = std::format("Import plan failed: {}", e.what());
        BOOST_LOG_SEV(lg(), error) << "ore_import_workflow step 3 failed | corr="
                                   << correlation_id_ << " error=" << error_;
        return false;
    }

    BOOST_LOG_SEV(lg(), info) << "ore_import_workflow step 3 complete | corr="
                              << correlation_id_
                              << " currencies=" << plan.currencies.size()
                              << " portfolios=" << plan.portfolios.size()
                              << " books=" << plan.books.size()
                              << " trades=" << plan.trades.size();

    // -------------------------------------------------------------------------
    // Step 4: save currencies (one per call)
    // -------------------------------------------------------------------------
    BOOST_LOG_SEV(lg(), debug) << "ore_import_workflow step 4: save currencies | corr="
                               << correlation_id_
                               << " count=" << plan.currencies.size();

    for (auto& currency : plan.currencies) {
        const auto iso = currency.iso_code;
        ores::refdata::messaging::save_currency_request req;
        req.data = std::move(currency);
        auto resp = nats_call(nats, req, error_);
        if (!resp || !resp->success) {
            if (error_.empty())
                error_ = std::format("save_currency failed for {}: {}",
                    iso, resp ? resp->message : "(no response)");
            BOOST_LOG_SEV(lg(), error) << "ore_import_workflow step 4 failed | corr="
                                       << correlation_id_
                                       << " iso_code=" << iso
                                       << " error=" << error_;
            return false;
        }
        saved_currency_iso_codes_.push_back(iso);
        BOOST_LOG_SEV(lg(), debug) << "ore_import_workflow saved currency | corr="
                                   << correlation_id_ << " iso_code=" << iso;
    }

    BOOST_LOG_SEV(lg(), info) << "ore_import_workflow step 4 complete | corr="
                              << correlation_id_
                              << " saved=" << saved_currency_iso_codes_.size();

    // -------------------------------------------------------------------------
    // Step 5: save portfolios (parents-first ordering from planner)
    // -------------------------------------------------------------------------
    BOOST_LOG_SEV(lg(), debug) << "ore_import_workflow step 5: save portfolios | corr="
                               << correlation_id_
                               << " count=" << plan.portfolios.size();

    for (auto& portfolio : plan.portfolios) {
        const auto pid = boost::uuids::to_string(portfolio.id);
        const auto name = portfolio.name;
        ores::refdata::messaging::save_portfolio_request req;
        req.data = std::move(portfolio);
        auto resp = nats_call(nats, req, error_);
        if (!resp || !resp->success) {
            if (error_.empty())
                error_ = std::format("save_portfolio failed for '{}' ({}): {}",
                    name, pid, resp ? resp->message : "(no response)");
            BOOST_LOG_SEV(lg(), error) << "ore_import_workflow step 5 failed | corr="
                                       << correlation_id_
                                       << " portfolio=" << pid
                                       << " error=" << error_;
            return false;
        }
        saved_portfolio_ids_.push_back(pid);
        BOOST_LOG_SEV(lg(), debug) << "ore_import_workflow saved portfolio | corr="
                                   << correlation_id_
                                   << " id=" << pid << " name=" << name;
    }

    BOOST_LOG_SEV(lg(), info) << "ore_import_workflow step 5 complete | corr="
                              << correlation_id_
                              << " saved=" << saved_portfolio_ids_.size();

    // -------------------------------------------------------------------------
    // Step 6: save books (one per call)
    // -------------------------------------------------------------------------
    BOOST_LOG_SEV(lg(), debug) << "ore_import_workflow step 6: save books | corr="
                               << correlation_id_
                               << " count=" << plan.books.size();

    for (auto& book : plan.books) {
        const auto bid = boost::uuids::to_string(book.id);
        const auto name = book.name;
        ores::refdata::messaging::save_book_request req;
        req.data = std::move(book);
        auto resp = nats_call(nats, req, error_);
        if (!resp || !resp->success) {
            if (error_.empty())
                error_ = std::format("save_book failed for '{}' ({}): {}",
                    name, bid, resp ? resp->message : "(no response)");
            BOOST_LOG_SEV(lg(), error) << "ore_import_workflow step 6 failed | corr="
                                       << correlation_id_
                                       << " book=" << bid
                                       << " error=" << error_;
            return false;
        }
        saved_book_ids_.push_back(bid);
        BOOST_LOG_SEV(lg(), debug) << "ore_import_workflow saved book | corr="
                                   << correlation_id_
                                   << " id=" << bid << " name=" << name;
    }

    BOOST_LOG_SEV(lg(), info) << "ore_import_workflow step 6 complete | corr="
                              << correlation_id_
                              << " saved=" << saved_book_ids_.size();

    // -------------------------------------------------------------------------
    // Step 7: save trades (one per call; failures collected, saga continues)
    // -------------------------------------------------------------------------
    BOOST_LOG_SEV(lg(), debug) << "ore_import_workflow step 7: save trades | corr="
                               << correlation_id_
                               << " count=" << plan.trades.size();

    for (auto& item : plan.trades) {
        const auto tid = boost::uuids::to_string(item.trade.id);
        const auto src = item.source_file.string();
        const auto ext_id = item.trade.external_id;

        ores::trading::messaging::save_trade_request req;
        req.trades = {std::move(item.trade)};

        std::string trade_error;
        auto resp = nats_call(nats, req, trade_error);
        if (!resp || !resp->success) {
            const auto msg = resp ? resp->message : trade_error;
            BOOST_LOG_SEV(lg(), warn) << "ore_import_workflow trade save failed | corr="
                                      << correlation_id_
                                      << " trade_id=" << tid
                                      << " source=" << src
                                      << " error=" << msg;
            result_.item_errors.push_back({
                .source_file = src,
                .item_id = ext_id,
                .message = msg});
        } else {
            saved_trade_ids_.push_back(tid);
            BOOST_LOG_SEV(lg(), debug) << "ore_import_workflow saved trade | corr="
                                       << correlation_id_
                                       << " id=" << tid << " ext_id=" << ext_id;
        }
    }

    BOOST_LOG_SEV(lg(), info) << "ore_import_workflow step 7 complete | corr="
                              << correlation_id_
                              << " saved=" << saved_trade_ids_.size()
                              << " failed=" << result_.item_errors.size();

    // -------------------------------------------------------------------------
    // Done — populate result
    // -------------------------------------------------------------------------
    result_.success = true;
    result_.message = "ORE import completed.";
    result_.correlation_id = correlation_id_;

    BOOST_LOG_SEV(lg(), info) << "ore_import_workflow complete | corr="
                              << correlation_id_
                              << " currencies=" << saved_currency_iso_codes_.size()
                              << " portfolios=" << saved_portfolio_ids_.size()
                              << " books=" << saved_book_ids_.size()
                              << " trades=" << saved_trade_ids_.size()
                              << " item_errors=" << result_.item_errors.size();
    return true;
}

void ore_import_workflow::compensate(ores::database::context /*ctx*/,
    ores::nats::service::nats_client& nats) {

    BOOST_LOG_SEV(lg(), info) << "ore_import_workflow compensating | corr="
                              << correlation_id_;

    // ── Delete trades ────────────────────────────────────────────────────────
    if (!saved_trade_ids_.empty()) {
        BOOST_LOG_SEV(lg(), info) << "Compensating: delete trades | corr="
                                  << correlation_id_
                                  << " count=" << saved_trade_ids_.size();
        ores::trading::messaging::delete_trade_request del_req;
        del_req.ids = saved_trade_ids_;
        std::string err;
        auto r = nats_call(nats, del_req, err);
        if (!r || !r->success) {
            const auto reason = (r && !r->message.empty()) ? r->message : err;
            BOOST_LOG_SEV(lg(), error) << "Compensation delete_trades failed | corr="
                                       << correlation_id_ << " error=" << reason;
        }
    }

    // ── Delete books ─────────────────────────────────────────────────────────
    if (!saved_book_ids_.empty()) {
        BOOST_LOG_SEV(lg(), info) << "Compensating: delete books | corr="
                                  << correlation_id_
                                  << " count=" << saved_book_ids_.size();
        ores::refdata::messaging::delete_book_request del_req;
        del_req.ids = saved_book_ids_;
        std::string err;
        auto r = nats_call(nats, del_req, err);
        if (!r || !r->success) {
            const auto reason = (r && !r->message.empty()) ? r->message : err;
            BOOST_LOG_SEV(lg(), error) << "Compensation delete_books failed | corr="
                                       << correlation_id_ << " error=" << reason;
        }
    }

    // ── Delete portfolios (reverse order — children before parents) ──────────
    if (!saved_portfolio_ids_.empty()) {
        BOOST_LOG_SEV(lg(), info) << "Compensating: delete portfolios | corr="
                                  << correlation_id_
                                  << " count=" << saved_portfolio_ids_.size();
        ores::refdata::messaging::delete_portfolio_request del_req;
        // Reverse the list so children are deleted before parents.
        del_req.ids = std::vector<std::string>(
            saved_portfolio_ids_.rbegin(), saved_portfolio_ids_.rend());
        std::string err;
        auto r = nats_call(nats, del_req, err);
        if (!r || !r->success) {
            const auto reason = (r && !r->message.empty()) ? r->message : err;
            BOOST_LOG_SEV(lg(), error) << "Compensation delete_portfolios failed | corr="
                                       << correlation_id_ << " error=" << reason;
        }
    }

    // ── Delete currencies ────────────────────────────────────────────────────
    if (!saved_currency_iso_codes_.empty()) {
        BOOST_LOG_SEV(lg(), info) << "Compensating: delete currencies | corr="
                                  << correlation_id_
                                  << " count=" << saved_currency_iso_codes_.size();
        ores::refdata::messaging::delete_currency_request del_req;
        del_req.iso_codes = saved_currency_iso_codes_;
        std::string err;
        auto r = nats_call(nats, del_req, err);
        if (!r || !r->success) {
            const auto reason = (r && !r->message.empty()) ? r->message : err;
            BOOST_LOG_SEV(lg(), error) << "Compensation delete_currencies failed | corr="
                                       << correlation_id_ << " error=" << reason;
        }
    }

    BOOST_LOG_SEV(lg(), info) << "ore_import_workflow compensation complete | corr="
                              << correlation_id_;
}

}
