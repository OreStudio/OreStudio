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
#include "ores.trading.core/repository/parent_scoped_queries.hpp"
#include "ores.trading.core/service/ascot_service.hpp"
#include "ores.trading.core/service/bond_future_service.hpp"
#include "ores.trading.core/service/bond_instrument_service.hpp"
#include "ores.trading.core/service/bond_issue_service.hpp"
#include "ores.trading.core/service/bond_option_service.hpp"
#include "ores.trading.core/service/bond_repo_service.hpp"
#include "ores.trading.core/service/bond_trs_service.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <utility>

namespace ores::trading::service {

using namespace ores::logging;

bond_instrument_reader::bond_instrument_reader(context ctx)
    : ctx_(std::move(ctx)) {}

std::unordered_map<std::string, domain::bond_instrument_data>
bond_instrument_reader::read_instruments(
    const std::vector<std::string>& instrument_ids) const {
    std::unordered_map<std::string, domain::bond_instrument_data> result;
    if (instrument_ids.empty())
        return result;

    bond_instrument_service instrument_svc(ctx_);
    bond_issue_service issue_svc(ctx_);
    bond_option_service option_svc(ctx_);
    bond_trs_service trs_svc(ctx_);
    bond_repo_service repo_svc(ctx_);
    bond_future_service future_svc(ctx_);
    ascot_service ascot_svc(ctx_);

    auto rows = instrument_svc.get_bond_instruments(instrument_ids);

    std::unordered_map<std::string, domain::bond_issue> issue_cache;
    std::vector<std::string> issue_ids;
    for (const auto& row : rows) {
        const auto issue_id = boost::uuids::to_string(row.issue_id);
        if (issue_cache.contains(issue_id))
            continue;
        issue_ids.push_back(issue_id);
        if (auto issue = issue_svc.get_issue(issue_id))
            issue_cache[issue_id] = *issue;
    }

    std::unordered_map<std::string, std::vector<domain::bond_issue_call_date>> call_dates;
    for (auto& row : repository::read_call_dates_by_issue_ids(ctx_, issue_ids))
        call_dates[boost::uuids::to_string(row.issue_id)].push_back(std::move(row));

    std::unordered_map<std::string, std::vector<domain::bond_issue_conversion_target>>
        conversion_targets;
    for (auto& row : repository::read_conversion_targets_by_issue_ids(ctx_, issue_ids))
        conversion_targets[boost::uuids::to_string(row.issue_id)].push_back(std::move(row));

    for (auto& row : rows) {
        const auto id = boost::uuids::to_string(row.identity.instrument_id);
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

        const auto& ttc = data.instrument.identity.trade_type_code;
        if (ttc == "BondOption")
            data.option = option_svc.get_option(id);
        else if (ttc == "BondTRS")
            data.trs = trs_svc.get_trs(id);
        else if (ttc == "BondRepo")
            data.repo = repo_svc.get_repo(id);
        else if (ttc == "BondFuture")
            data.future = future_svc.get_future(id);
        else if (ttc == "Ascot")
            data.ascot = ascot_svc.get_ascot(id);

        result.emplace(id, std::move(data));
    }

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " bond instruments.";
    return result;
}

}
