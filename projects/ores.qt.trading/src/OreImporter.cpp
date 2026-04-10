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
#include "ores.qt/OreImporter.hpp"

#include <map>
#include <unordered_set>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.ore/planner/ore_import_planner.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.api/messaging/portfolio_protocol.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ores.database/domain/change_reason_constants.hpp"

namespace ores::qt {

using namespace ores::logging;
using ore::planner::ore_import_result;
using ore::planner::ore_instrument_error;

namespace {

// Fallback dates applied when ORE XML omits required trade fields.
constexpr std::string_view fallback_trade_date       = "2026-01-01";
constexpr std::string_view fallback_timestamp_suffix = " 00:00:00+00:00";
constexpr std::string_view fallback_termination_date = "2099-12-31";

auto make_error(const auto& resp, const char* step) -> std::string {
    if (!resp)
        return std::string(step) + ": " + std::string(resp.error());
    return std::string(step) + ": " + resp->message;
}

}

OreImporter::OreImporter(ClientManager* cm) : cm_(cm) {}

ore_import_result OreImporter::execute(
        ore::scanner::scan_result scan_result,
        std::set<std::string> existing_iso_codes,
        ore::planner::import_choices choices) {

    try {
        ore::planner::ore_import_planner planner(
            std::move(scan_result),
            std::move(existing_iso_codes),
            choices);
        auto plan = planner.plan();

        BOOST_LOG_SEV(lg(), info) << "Import plan: "
            << plan.currencies.size() << " currencies, "
            << plan.portfolios.size() << " portfolios, "
            << plan.books.size() << " books, "
            << plan.trades.size() << " trades";

        // -----------------------------------------------------------------
        // Resolve name conflicts for portfolios and books.
        // -----------------------------------------------------------------

        // Step A: fetch existing portfolios
        std::map<std::string, boost::uuids::uuid> existing_portfolio_by_name;
        {
            refdata::messaging::get_portfolios_request req;
            req.limit = 1000;
            const auto resp = cm_->process_authenticated_request(std::move(req));
            if (resp) {
                for (const auto& p : resp->portfolios)
                    existing_portfolio_by_name[p.name] = p.id;
            }
            BOOST_LOG_SEV(lg(), debug) << "Fetched "
                << existing_portfolio_by_name.size() << " existing portfolios";
        }

        // Step B: build portfolio UUID remap; decide which to save.
        const bool new_versions = (choices.portfolio_reimport_mode ==
            ore::planner::reimport_mode::create_new_versions);
        std::map<boost::uuids::uuid, boost::uuids::uuid> portfolio_uuid_remap;
        {
            std::map<std::string, boost::uuids::uuid> committed_names =
                existing_portfolio_by_name;
            std::map<boost::uuids::uuid, std::string> uuid_to_name;
            for (const auto& [name, id] : existing_portfolio_by_name)
                uuid_to_name[id] = name;
            std::vector<refdata::domain::portfolio> portfolios_to_save;

            auto make_unique_name = [&](const std::string& base,
                                        const std::optional<boost::uuids::uuid>& parent_id)
                    -> std::string {
                if (parent_id) {
                    auto pit = portfolio_uuid_remap.find(*parent_id);
                    auto pkey = pit != portfolio_uuid_remap.end() ? pit->second : *parent_id;
                    if (auto nit = uuid_to_name.find(pkey); nit != uuid_to_name.end()) {
                        std::string candidate = base + " " + nit->second;
                        if (!committed_names.count(candidate))
                            return candidate;
                    }
                }
                for (int n = 2; ; ++n) {
                    std::string candidate = base + " " + std::to_string(n);
                    if (!committed_names.count(candidate))
                        return candidate;
                }
            };

            for (auto& p : plan.portfolios) {
                if (p.parent_portfolio_id) {
                    auto pit = portfolio_uuid_remap.find(*p.parent_portfolio_id);
                    if (pit != portfolio_uuid_remap.end())
                        p.parent_portfolio_id = pit->second;
                }

                if (committed_names.count(p.name)) {
                    auto it = existing_portfolio_by_name.find(p.name);
                    if (it != existing_portfolio_by_name.end()) {
                        portfolio_uuid_remap[p.id] = it->second;
                        p.id = it->second;
                        if (new_versions || p.parent_portfolio_id) {
                            portfolios_to_save.push_back(std::move(p));
                            BOOST_LOG_SEV(lg(), debug)
                                << "Portfolio '" << p.name << "' exists, "
                                << (new_versions ? "creating new version" : "re-parenting");
                        } else {
                            BOOST_LOG_SEV(lg(), debug)
                                << "Portfolio '" << p.name << "' exists, skipping";
                        }
                    } else {
                        const auto unique = make_unique_name(p.name, p.parent_portfolio_id);
                        BOOST_LOG_SEV(lg(), debug)
                            << "Portfolio '" << p.name << "' renamed to '" << unique << "'";
                        p.name = unique;
                        committed_names[p.name] = p.id;
                        uuid_to_name[p.id] = p.name;
                        portfolios_to_save.push_back(std::move(p));
                    }
                    continue;
                }

                committed_names[p.name] = p.id;
                uuid_to_name[p.id] = p.name;
                portfolios_to_save.push_back(std::move(p));
            }
            plan.portfolios = std::move(portfolios_to_save);
            BOOST_LOG_SEV(lg(), info) << "After dedup: "
                << plan.portfolios.size() << " portfolio(s) to save";
        }

        // Apply portfolio remap to books and trades
        for (auto& b : plan.books) {
            auto it = portfolio_uuid_remap.find(b.parent_portfolio_id);
            if (it != portfolio_uuid_remap.end())
                b.parent_portfolio_id = it->second;
        }
        for (auto& item : plan.trades) {
            auto it = portfolio_uuid_remap.find(item.trade.portfolio_id);
            if (it != portfolio_uuid_remap.end())
                item.trade.portfolio_id = it->second;
        }

        // Step C: fetch existing books
        std::map<std::string, boost::uuids::uuid> existing_book_by_name;
        {
            refdata::messaging::get_books_request req;
            req.limit = 1000;
            const auto resp = cm_->process_authenticated_request(std::move(req));
            if (resp) {
                for (const auto& b : resp->books)
                    existing_book_by_name[b.name] = b.id;
            }
            BOOST_LOG_SEV(lg(), debug) << "Fetched "
                << existing_book_by_name.size() << " existing books";
        }

        // Step D: build book UUID remap; decide which to save.
        std::map<boost::uuids::uuid, boost::uuids::uuid> book_uuid_remap;
        {
            std::map<std::string, boost::uuids::uuid> committed_book_names =
                existing_book_by_name;
            std::map<boost::uuids::uuid, std::string> portfolio_uuid_to_name;
            for (const auto& p : plan.portfolios)
                portfolio_uuid_to_name[p.id] = p.name;
            for (const auto& [name, id] : existing_portfolio_by_name)
                portfolio_uuid_to_name.emplace(id, name);

            auto make_unique_book_name = [&](const std::string& base,
                                             const boost::uuids::uuid& parent_portfolio_id)
                    -> std::string {
                if (auto pit = portfolio_uuid_to_name.find(parent_portfolio_id);
                        pit != portfolio_uuid_to_name.end()) {
                    std::string candidate = base + " " + pit->second;
                    if (!committed_book_names.count(candidate))
                        return candidate;
                }
                for (int n = 2; ; ++n) {
                    std::string candidate = base + " " + std::to_string(n);
                    if (!committed_book_names.count(candidate))
                        return candidate;
                }
            };

            std::vector<refdata::domain::book> books_to_save;
            for (auto& b : plan.books) {
                if (committed_book_names.count(b.name)) {
                    auto it = existing_book_by_name.find(b.name);
                    if (it != existing_book_by_name.end()) {
                        book_uuid_remap[b.id] = it->second;
                        if (new_versions) {
                            b.id = it->second;
                            books_to_save.push_back(std::move(b));
                            BOOST_LOG_SEV(lg(), debug)
                                << "Book '" << b.name << "' exists, creating new version";
                        } else {
                            BOOST_LOG_SEV(lg(), debug)
                                << "Book '" << b.name << "' exists, skipping";
                        }
                    } else {
                        const auto unique = make_unique_book_name(b.name, b.parent_portfolio_id);
                        BOOST_LOG_SEV(lg(), debug)
                            << "Book '" << b.name << "' renamed to '" << unique << "'";
                        b.name = unique;
                        committed_book_names[b.name] = b.id;
                        books_to_save.push_back(std::move(b));
                    }
                    continue;
                }
                committed_book_names[b.name] = b.id;
                books_to_save.push_back(std::move(b));
            }
            plan.books = std::move(books_to_save);
            BOOST_LOG_SEV(lg(), info) << "After dedup: "
                << plan.books.size() << " book(s) to save";
        }

        // Apply book remap to trades
        for (auto& item : plan.trades) {
            auto it = book_uuid_remap.find(item.trade.book_id);
            if (it != book_uuid_remap.end())
                item.trade.book_id = it->second;
        }

        ore_import_result res;

        // Step 1: currencies (one per request)
        for (const auto& ccy : plan.currencies) {
            auto req = refdata::messaging::save_currency_request::from(ccy);
            const auto resp = cm_->process_authenticated_request(std::move(req));
            if (!resp || !resp->success)
                return {.success=false, .error=make_error(resp, "Currency save")};
            res.currencies++;
        }
        if (res.currencies > 0)
            BOOST_LOG_SEV(lg(), info) << "Saved " << res.currencies << " currencies";

        // Step 2: portfolios (one per request)
        for (const auto& pf : plan.portfolios) {
            refdata::messaging::save_portfolio_request req;
            req.data = pf;
            const auto resp = cm_->process_authenticated_request(std::move(req));
            if (!resp || !resp->success)
                return {.success=false, .error=make_error(resp, "Portfolio save"),
                        .currencies=res.currencies};
            res.portfolios++;
        }
        if (res.portfolios > 0)
            BOOST_LOG_SEV(lg(), info) << "Saved " << res.portfolios << " portfolios";

        // Step 3: books (one per request)
        for (const auto& bk : plan.books) {
            refdata::messaging::save_book_request req;
            req.data = bk;
            const auto resp = cm_->process_authenticated_request(std::move(req));
            if (!resp || !resp->success)
                return {.success=false, .error=make_error(resp, "Book save"),
                        .currencies=res.currencies, .portfolios=res.portfolios};
            res.books++;
        }
        if (res.books > 0)
            BOOST_LOG_SEV(lg(), info) << "Saved " << res.books << " books";

        // Fill required trade fields that ORE XML may omit.
        {
            const std::string& td = choices.defaults.trade_date;
            const std::string ts_fallback = td.empty()
                ? std::string(fallback_trade_date) + std::string(fallback_timestamp_suffix)
                : td + std::string(fallback_timestamp_suffix);
            const std::string date_fallback = td.empty()
                ? std::string(fallback_trade_date) : td;

            int filled = 0;
            for (auto& item : plan.trades) {
                auto& t = item.trade;
                bool any = false;
                if (t.execution_timestamp.empty()) {
                    t.execution_timestamp = ts_fallback;
                    any = true;
                }
                if (t.effective_date.empty()) {
                    t.effective_date = date_fallback;
                    any = true;
                }
                if (t.termination_date.empty()) {
                    t.termination_date = std::string(fallback_termination_date);
                    any = true;
                }
                if (any) ++filled;
            }
            if (filled > 0)
                BOOST_LOG_SEV(lg(), info) << "Filled missing date fields for "
                    << filled << " trade(s)";
        }

        // Step 4: trades in batches of 100
        constexpr int trade_batch_size = 100;
        const int total_trades = static_cast<int>(plan.trades.size());
        const int num_batches = total_trades == 0 ? 0
            : (total_trades + trade_batch_size - 1) / trade_batch_size;
        BOOST_LOG_SEV(lg(), info)
            << "Sending " << total_trades << " trades in "
            << num_batches << " batch(es) of up to " << trade_batch_size << " each";
        for (int offset = 0; offset < total_trades; offset += trade_batch_size) {
            const int end = std::min(offset + trade_batch_size, total_trades);
            namespace reason = ores::database::domain::change_reason_constants;
            std::vector<trading::domain::trade> batch;
            batch.reserve(static_cast<std::size_t>(end - offset));
            for (int i = offset; i < end; ++i) {
                auto t = plan.trades[static_cast<std::size_t>(i)].trade;
                t.change_reason_code = std::string(reason::codes::external_data_import);
                batch.push_back(std::move(t));
            }

            BOOST_LOG_SEV(lg(), debug) << "Sending trade batch "
                << (offset / trade_batch_size + 1) << "/" << num_batches
                << ": " << batch.size() << " trades";

            auto req = trading::messaging::save_trade_request::from(std::move(batch));
            const auto resp = cm_->process_authenticated_request(std::move(req));
            if (!resp || !resp->success)
                return {.success=false, .error=make_error(resp, "Trade save"),
                        .currencies=res.currencies, .portfolios=res.portfolios,
                        .books=res.books};
            res.trades += end - offset;
        }
        if (total_trades > 0)
            BOOST_LOG_SEV(lg(), info) << "Saved " << res.trades << " trades";

        // Step 5: save instruments (non-fatal — collect errors, continue)
        BOOST_LOG_SEV(lg(), debug) << "Saving instruments for "
            << plan.trades.size() << " trade(s)";
        for (const auto& item : plan.trades) {
            using namespace ores::trading::messaging;
            using namespace ores::ore::domain;
            std::visit([&](const auto& r) {
                using T = std::decay_t<decltype(r)>;
                if constexpr (std::is_same_v<T, std::monostate>) {
                    // No instrument mapping for this trade type.
                } else {
                    const auto record_error = [&](const auto& resp) {
                        const auto msg = resp ? resp->message : "no response from server";
                        BOOST_LOG_SEV(lg(), warn)
                            << "Instrument save failed for trade "
                            << item.trade.external_id << ": " << msg;
                        res.instrument_errors.push_back(
                            {item.trade.external_id, std::string(msg)});
                    };

                    if constexpr (std::is_same_v<T, swap_mapping_result>) {
                        std::visit([&](const auto& instr) {
                            using InstrT = std::decay_t<decltype(instr)>;
                            using ores::trading::domain::fra_instrument;
                            using ores::trading::domain::vanilla_swap_instrument;
                            using ores::trading::domain::cap_floor_instrument;
                            using ores::trading::domain::swaption_instrument;
                            using ores::trading::domain::balance_guaranteed_swap_instrument;
                            using ores::trading::domain::callable_swap_instrument;
                            using ores::trading::domain::knock_out_swap_instrument;
                            using ores::trading::domain::inflation_swap_instrument;
                            using ores::trading::domain::rpa_instrument;
                            if constexpr (std::is_same_v<InstrT, fra_instrument>) {
                                save_fra_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            } else if constexpr (std::is_same_v<InstrT, vanilla_swap_instrument>) {
                                save_vanilla_swap_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            } else if constexpr (std::is_same_v<InstrT, cap_floor_instrument>) {
                                save_cap_floor_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            } else if constexpr (std::is_same_v<InstrT, swaption_instrument>) {
                                save_swaption_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            } else if constexpr (std::is_same_v<InstrT, balance_guaranteed_swap_instrument>) {
                                save_balance_guaranteed_swap_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            } else if constexpr (std::is_same_v<InstrT, callable_swap_instrument>) {
                                save_callable_swap_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            } else if constexpr (std::is_same_v<InstrT, knock_out_swap_instrument>) {
                                save_knock_out_swap_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            } else if constexpr (std::is_same_v<InstrT, inflation_swap_instrument>) {
                                save_inflation_swap_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            } else if constexpr (std::is_same_v<InstrT, rpa_instrument>) {
                                save_rpa_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            }
                        }, r.instrument);
                    } else if constexpr (std::is_same_v<T, fx_mapping_result>) {
                        std::visit([&](const auto& instr) {
                            using InstrT = std::decay_t<decltype(instr)>;
                            using ores::trading::domain::fx_forward_instrument;
                            using ores::trading::domain::fx_vanilla_option_instrument;
                            using ores::trading::domain::fx_barrier_option_instrument;
                            using ores::trading::domain::fx_digital_option_instrument;
                            using ores::trading::domain::fx_asian_forward_instrument;
                            using ores::trading::domain::fx_accumulator_instrument;
                            using ores::trading::domain::fx_variance_swap_instrument;
                            if constexpr (std::is_same_v<InstrT, fx_forward_instrument>) {
                                save_fx_forward_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            } else if constexpr (std::is_same_v<InstrT, fx_vanilla_option_instrument>) {
                                save_fx_vanilla_option_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            } else if constexpr (std::is_same_v<InstrT, fx_barrier_option_instrument>) {
                                save_fx_barrier_option_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            } else if constexpr (std::is_same_v<InstrT, fx_digital_option_instrument>) {
                                save_fx_digital_option_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            } else if constexpr (std::is_same_v<InstrT, fx_asian_forward_instrument>) {
                                save_fx_asian_forward_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            } else if constexpr (std::is_same_v<InstrT, fx_accumulator_instrument>) {
                                save_fx_accumulator_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            } else if constexpr (std::is_same_v<InstrT, fx_variance_swap_instrument>) {
                                save_fx_variance_swap_instrument_request req;
                                req.data = instr;
                                const auto resp =
                                    cm_->process_authenticated_request(std::move(req));
                                if (!resp || !resp->success) record_error(resp);
                                else ++res.instruments;
                            }
                        }, r.instrument);
                    } else if constexpr (std::is_same_v<T, bond_mapping_result>) {
                        save_bond_instrument_request req;
                        req.data = r.instrument;
                        const auto resp = cm_->process_authenticated_request(std::move(req));
                        if (!resp || !resp->success) record_error(resp);
                        else ++res.instruments;
                    } else if constexpr (std::is_same_v<T, credit_mapping_result>) {
                        save_credit_instrument_request req;
                        req.data = r.instrument;
                        const auto resp = cm_->process_authenticated_request(std::move(req));
                        if (!resp || !resp->success) record_error(resp);
                        else ++res.instruments;
                    } else if constexpr (std::is_same_v<T, equity_mapping_result>) {
                        save_equity_instrument_request req;
                        req.data = r.instrument;
                        const auto resp = cm_->process_authenticated_request(std::move(req));
                        if (!resp || !resp->success) record_error(resp);
                        else ++res.instruments;
                    } else if constexpr (std::is_same_v<T, commodity_mapping_result>) {
                        save_commodity_instrument_request req;
                        req.data = r.instrument;
                        const auto resp = cm_->process_authenticated_request(std::move(req));
                        if (!resp || !resp->success) record_error(resp);
                        else ++res.instruments;
                    } else if constexpr (std::is_same_v<T, composite_mapping_result>) {
                        save_composite_instrument_request req;
                        req.data = r.instrument;
                        const auto resp = cm_->process_authenticated_request(std::move(req));
                        if (!resp || !resp->success) record_error(resp);
                        else ++res.instruments;
                    } else if constexpr (std::is_same_v<T, scripted_mapping_result>) {
                        save_scripted_instrument_request req;
                        req.data = r.instrument;
                        const auto resp = cm_->process_authenticated_request(std::move(req));
                        if (!resp || !resp->success) record_error(resp);
                        else ++res.instruments;
                    }
                }
            }, item.instrument);
        }
        if (res.instruments > 0)
            BOOST_LOG_SEV(lg(), info) << "Saved " << res.instruments << " instruments";
        if (!res.instrument_errors.empty())
            BOOST_LOG_SEV(lg(), warn) << res.instrument_errors.size()
                << " instrument(s) failed to save";

        res.success = true;
        return res;

    } catch (const std::exception& ex) {
        BOOST_LOG_SEV(lg(), error) << "Import exception: " << ex.what();
        return {.success=false, .error=ex.what()};
    }
}

}
