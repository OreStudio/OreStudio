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
#include "ores.trading.core/service/bond_instrument_reader.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/bond_issue_leg.hpp"
#include "ores.trading.api/domain/bond_issue_leg_amortization.hpp"
#include "ores.trading.api/domain/bond_issue_leg_amount.hpp"
#include "ores.trading.api/domain/bond_issue_leg_rate.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule_date.hpp"
#include "ores.trading.core/repository/instrument_underlying_repository.hpp"
#include "ores.trading.core/repository/parent_scoped_queries.hpp"
#include "ores.trading.core/service/ascot_service.hpp"
#include "ores.trading.core/service/bond_future_service.hpp"
#include "ores.trading.core/service/bond_instrument_service.hpp"
#include "ores.trading.core/service/bond_issue_service.hpp"
#include "ores.trading.core/service/bond_option_service.hpp"
#include "ores.trading.core/service/bond_repo_service.hpp"
#include "ores.trading.core/service/bond_trs_service.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

namespace ores::trading::service {

using namespace ores::logging;

namespace {

std::unordered_map<std::string, domain::bond_trade_rows>
read_family_rows(const ores::database::context& ctx, const std::vector<std::string>& trade_ids) {
    std::unordered_map<std::string, domain::bond_trade_rows> rows;

    for (auto& row : repository::read_legs_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].legs.push_back(std::move(row));

    for (auto& row : repository::read_leg_amounts_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].amounts.push_back(std::move(row));

    for (auto& row : repository::read_leg_rates_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].rates.push_back(std::move(row));

    for (auto& row : repository::read_leg_amortizations_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].amortizations.push_back(std::move(row));

    for (auto& row : repository::read_schedules_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].schedules.push_back(std::move(row));

    for (auto& row : repository::read_schedule_dates_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].schedule_dates.push_back(std::move(row));

    for (auto& row : repository::read_options_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].option = std::move(row);

    for (auto& row : repository::read_option_premiums_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].option_premiums.push_back(std::move(row));

    for (auto& row : repository::read_option_exercise_fees_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].option_exercise_fees.push_back(std::move(row));

    for (auto& row : repository::read_option_payment_dates_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].option_payment_dates.push_back(std::move(row));

    for (auto& row : repository::read_option_exercise_prices_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].option_exercise_prices.push_back(
            std::move(row));

    for (auto& row : repository::read_strikes_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].strike = std::move(row);

    for (auto& row : repository::read_forwards_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].forward = std::move(row);

    return rows;
}

/**
 * @brief The security-owned rows of a set of issues, keyed by issue.
 *
 * Every instrument of one ISIN shares these rows, so the caller copies
 * them into each instrument's container rather than moving them.
 */
std::unordered_map<std::string, domain::issue_leg_rows>
read_issue_family_rows(const ores::database::context& ctx,
                       const std::vector<std::string>& issue_ids) {
    std::unordered_map<std::string, domain::issue_leg_rows> rows;
    if (issue_ids.empty())
        return rows;

    for (auto& row : repository::read_issue_legs_by_issue_ids(ctx, issue_ids))
        rows[boost::uuids::to_string(row.issue_id)].legs.push_back(std::move(row));

    for (auto& row : repository::read_issue_leg_amounts_by_issue_ids(ctx, issue_ids))
        rows[boost::uuids::to_string(row.issue_id)].amounts.push_back(std::move(row));

    for (auto& row : repository::read_issue_leg_rates_by_issue_ids(ctx, issue_ids))
        rows[boost::uuids::to_string(row.issue_id)].rates.push_back(std::move(row));

    for (auto& row : repository::read_issue_leg_amortizations_by_issue_ids(ctx, issue_ids))
        rows[boost::uuids::to_string(row.issue_id)].amortizations.push_back(std::move(row));

    for (auto& row : repository::read_issue_schedules_by_issue_ids(ctx, issue_ids))
        rows[boost::uuids::to_string(row.issue_id)].schedules.push_back(std::move(row));

    for (auto& row : repository::read_issue_schedule_dates_by_issue_ids(ctx, issue_ids))
        rows[boost::uuids::to_string(row.issue_id)].schedule_dates.push_back(std::move(row));

    return rows;
}

}

bond_instrument_reader::bond_instrument_reader(const context& ctx)
    : ctx_(ctx) {}

std::unordered_map<std::string, domain::bond_instrument_data>
bond_instrument_reader::read_instruments(const std::vector<std::string>& trade_ids) const {
    std::unordered_map<std::string, domain::bond_instrument_data> result;
    if (trade_ids.empty())
        return result;

    bond_instrument_service instrument_svc(ctx_);
    bond_issue_service issue_svc(ctx_);
    bond_option_service option_svc(ctx_);
    bond_trs_service trs_svc(ctx_);
    bond_repo_service repo_svc(ctx_);
    bond_future_service future_svc(ctx_);
    ascot_service ascot_svc(ctx_);

    auto rows = instrument_svc.get_bond_instruments(trade_ids);

    std::unordered_map<std::string, domain::bond_issue> issue_cache;
    std::vector<std::string> issue_ids;
    for (const auto& row : rows) {
        const auto issue_id = boost::uuids::to_string(row.issue_id);
        if (issue_cache.contains(issue_id))
            continue;
        issue_ids.push_back(issue_id);
        if (auto issue = issue_svc.get_issue(row.issue_id))
            issue_cache[issue_id] = *issue;
    }

    std::unordered_map<std::string, std::vector<domain::bond_issue_call_date>> call_dates;
    for (auto& row : repository::read_call_dates_by_issue_ids(ctx_, issue_ids))
        call_dates[boost::uuids::to_string(row.issue_id)].push_back(std::move(row));

    std::unordered_map<std::string, std::vector<domain::bond_issue_conversion_target>>
        conversion_targets;
    for (auto& row : repository::read_conversion_targets_by_issue_ids(ctx_, issue_ids))
        conversion_targets[boost::uuids::to_string(row.issue_id)].push_back(std::move(row));

    const auto leg_family = read_family_rows(ctx_, trade_ids);
    const auto issue_family = read_issue_family_rows(ctx_, issue_ids);

    // An instrument with no rows of its own still reads, so the two
    // lookups fall back to empty row sets rather than skipping the build.
    static const domain::bond_trade_rows no_trade_rows;
    static const domain::issue_leg_rows no_issue_rows;

    std::unordered_map<std::string, std::vector<domain::instrument_underlying>> underlyings;
    for (auto& row :
         repository::instrument_underlying_repository().read_by_instruments_batch(ctx_, trade_ids))
        underlyings[boost::uuids::to_string(row.trade_id)].push_back(std::move(row));

    for (auto& row : rows) {
        const auto id = boost::uuids::to_string(row.identity.trade_id);
        const auto issue_id = boost::uuids::to_string(row.issue_id);
        domain::bond_instrument_data data;
        data.instrument = std::move(row);
        if (auto it = issue_cache.find(issue_id); it != issue_cache.end())
            data.issue = it->second;
        // Copied, not moved: one issue serves every instrument of a
        // security, so a moved list would leave the next one empty.
        if (auto it = call_dates.find(issue_id); it != call_dates.end())
            data.call_dates = it->second;
        if (auto it = conversion_targets.find(issue_id); it != conversion_targets.end())
            data.conversion_targets = it->second;
        const auto family = leg_family.find(id);
        const auto issue_family_row = issue_family.find(issue_id);
        const auto& trade_rows = family == leg_family.end() ? no_trade_rows : family->second;
        const auto& security_rows =
            issue_family_row == issue_family.end() ? no_issue_rows : issue_family_row->second;
        auto own = trade_rows;
        if (auto it = underlyings.find(id); it != underlyings.end())
            own.underlyings = std::move(it->second);
        const auto& ttc = data.instrument.identity.trade_type_code;
        const auto& trade_id = data.instrument.identity.trade_id;
        if (ttc == "BondOption")
            own.bond_option = option_svc.get_option(trade_id);
        else if (ttc == "BondTRS")
            own.trs = trs_svc.get_trs(trade_id);
        else if (ttc == "BondRepo")
            own.repo = repo_svc.get_repo(trade_id);
        else if (ttc == "BondFuture")
            own.future = future_svc.get_future(trade_id);
        else if (ttc == "Ascot")
            own.ascot = ascot_svc.get_ascot(trade_id);
        domain::assemble_bond(data, own, security_rows);

        result.emplace(id, std::move(data));
    }

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " bond instruments.";
    return result;
}

}
