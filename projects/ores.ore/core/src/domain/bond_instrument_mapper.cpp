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
#include "ores.ore.core/domain/bond_instrument_mapper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <stdexcept>

namespace ores::ore::domain {

using namespace ores::logging;
using ores::trading::domain::bond_issue;
using ores::trading::domain::bond_instrument_data;
using ores::trading::domain::bond_option;
using ores::trading::domain::bond_repo;
using ores::trading::domain::bond_trs;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

namespace {

std::string first_tenor(const xsd::optional<scheduleData>& sd) {
    if (!sd || sd->Rules.empty())
        return {};
    return std::string(sd->Rules.front().Tenor);
}

// The instrument header, the issue and the fact rows each carry the audit
// columns; every row of a mapped trade shares the import provenance.
template <typename T>
void stamp_audit(T& row) {
    row.modified_by = "ores";
    row.performed_by = "ores";
    row.change_reason_code = "system.external_data_import";
    row.change_commentary = "Imported from ORE XML";
}

bond_instrument_data make_base(const std::string& trade_type_code) {
    bond_instrument_data result;
    result.instrument.identity.trade_type_code = trade_type_code;
    stamp_audit(result.instrument.audit);

    // One mapped trade carries its own minted issue row. Finding an
    // existing issue by security_id is the database boundary of the
    // mapping task.
    const auto issue_id = boost::uuids::random_generator()();
    result.instrument.issue_id = issue_id;
    result.issue.issue_id = issue_id;
    stamp_audit(result.issue);
    return result;
}

} // namespace

// ---------------------------------------------------------------------------
// Common: map bondData fields → bond_issue
// ---------------------------------------------------------------------------

void bond_instrument_mapper::map_bond_data(const bondData& bd, bond_issue& issue) {
    issue.security_id = std::string(bd.SecurityId);
    if (bd.IssuerId)
        issue.issuer = std::string(*bd.IssuerId);
    if (bd.IssueDate)
        issue.issue_date = std::string(*bd.IssueDate);
    if (bd.SettlementDays) {
        const std::string settlement_days_str(*bd.SettlementDays);
        if (!settlement_days_str.empty())
            issue.settlement_days = std::stoi(settlement_days_str);
    }

    if (!bd.LegData.empty()) {
        const auto& ld = bd.LegData.front();
        if (ld.Currency)
            issue.currency = std::string(*ld.Currency);
        if (ld.Notionals && !ld.Notionals->Notional.empty())
            issue.face_value = static_cast<double>(ld.Notionals->Notional.front());
        if (ld.DayCounter)
            issue.day_count_code = to_string(*ld.DayCounter);
        issue.coupon_frequency_code = first_tenor(ld.ScheduleData);

        if (ld.legDataType && ld.legDataType->FixedLegData &&
            !ld.legDataType->FixedLegData->Rates.Rate.empty())
            issue.coupon_rate =
                static_cast<double>(ld.legDataType->FixedLegData->Rates.Rate.front());

        if (ld.ScheduleData && !ld.ScheduleData->Rules.empty()) {
            const auto& rule = ld.ScheduleData->Rules.front();
            if (rule.EndDate)
                issue.maturity_date = std::string(*rule.EndDate);
        }
    }
}

// ---------------------------------------------------------------------------
// Common: bond_issue → bondData
// ---------------------------------------------------------------------------

bondData bond_instrument_mapper::reverse_bond_data(const bond_issue& issue) {
    bondData bd;

    static_cast<std::string&>(bd.SecurityId) = issue.security_id;
    if (!issue.issuer.empty()) {
        bondData_IssuerId_t id;
        static_cast<std::string&>(id) = issue.issuer;
        bd.IssuerId = std::move(id);
    }
    if (!issue.issue_date.empty()) {
        bondData_IssueDate_t d;
        static_cast<std::string&>(d) = issue.issue_date;
        bd.IssueDate = std::move(d);
    }
    if (issue.settlement_days != 0) {
        bondData_SettlementDays_t sd;
        static_cast<std::string&>(sd) = std::to_string(issue.settlement_days);
        bd.SettlementDays = std::move(sd);
    }

    if (!issue.currency.empty() || issue.face_value != 0.0) {
        legData ld;
        ld.LegType = legType::Fixed;

        if (!issue.currency.empty())
            ld.Currency = issue.currency;

        if (issue.face_value != 0.0) {
            legData_Notionals_t n;
            legData_Notionals_t_Notional_t nv;
            static_cast<float&>(nv) = static_cast<float>(issue.face_value);
            n.Notional.push_back(nv);
            ld.Notionals = std::move(n);
        }

        if (!issue.maturity_date.empty() || !issue.coupon_frequency_code.empty()) {
            scheduleData_Rules_t rule;
            if (!issue.maturity_date.empty()) {
                domain::date d;
                static_cast<std::string&>(d) = issue.maturity_date;
                rule.EndDate = xsd::optional<domain::date>(d);
            }
            if (!issue.coupon_frequency_code.empty())
                static_cast<std::string&>(rule.Tenor) = issue.coupon_frequency_code;
            if (!issue.issue_date.empty()) {
                domain::date sd;
                static_cast<std::string&>(sd) = issue.issue_date;
                rule.StartDate = xsd::optional<domain::date>(sd);
            }
            scheduleData sched;
            sched.Rules.push_back(std::move(rule));
            ld.ScheduleData = std::move(sched);
        }

        if (issue.coupon_rate != 0.0) {
            _FixedLegData_t fld;
            _FixedLegData_t_Rates_t_Rate_t rate;
            static_cast<float&>(rate) = static_cast<float>(issue.coupon_rate);
            fld.Rates.Rate.push_back(rate);
            legDataType_group_t ldt;
            ldt.FixedLegData = std::move(fld);
            ld.legDataType = std::move(ldt);
        }

        bd.LegData.push_back(std::move(ld));
    }

    return bd;
}

// ---------------------------------------------------------------------------
// Forward: Bond
// ---------------------------------------------------------------------------

bond_instrument_data bond_instrument_mapper::forward_bond(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping Bond: " << std::string(t.id);
    bond_instrument_data result = make_base("Bond");
    if (t.BondData)
        map_bond_data(*t.BondData, result.issue);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: ForwardBond
// ---------------------------------------------------------------------------

bond_instrument_data bond_instrument_mapper::forward_forward_bond(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping ForwardBond: " << std::string(t.id);
    bond_instrument_data result = make_base("ForwardBond");
    if (t.ForwardBondData)
        map_bond_data(t.ForwardBondData->BondData, result.issue);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: CallableBond
// ---------------------------------------------------------------------------

bond_instrument_data bond_instrument_mapper::forward_callable_bond(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CallableBond: " << std::string(t.id);
    bond_instrument_data result = make_base("CallableBond");
    if (t.CallableBondData)
        map_bond_data(t.CallableBondData->BondData, result.issue);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: ConvertibleBond
// ---------------------------------------------------------------------------

bond_instrument_data bond_instrument_mapper::forward_convertible_bond(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping ConvertibleBond: " << std::string(t.id);
    bond_instrument_data result = make_base("ConvertibleBond");
    if (t.ConvertibleBondData)
        map_bond_data(t.ConvertibleBondData->BondData, result.issue);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: BondOption
// ---------------------------------------------------------------------------

bond_instrument_data bond_instrument_mapper::forward_bond_option(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping BondOption: " << std::string(t.id);
    bond_instrument_data result = make_base("BondOption");
    if (!t.BondOptionData)
        return result;
    const auto& d = *t.BondOptionData;

    map_bond_data(d.BondData, result.issue);

    bond_option option;
    if (d.OptionData.OptionType)
        option.option_type = std::string(*d.OptionData.OptionType);
    if (d.strikeGroup.Strike) {
        const std::string s(*d.strikeGroup.Strike);
        if (!s.empty())
            option.option_strike = std::stod(s);
    }
    stamp_audit(option);
    result.option = option;

    if (d.OptionData.exerciseDatesGroup && d.OptionData.exerciseDatesGroup->ExerciseDates &&
        !d.OptionData.exerciseDatesGroup->ExerciseDates->ExerciseDate.empty())
        result.option_expiry_date =
            std::string(d.OptionData.exerciseDatesGroup->ExerciseDates->ExerciseDate.front());

    return result;
}

// ---------------------------------------------------------------------------
// Forward: BondTRS
// ---------------------------------------------------------------------------

bond_instrument_data bond_instrument_mapper::forward_bond_trs(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping BondTRS: " << std::string(t.id);
    bond_instrument_data result = make_base("BondTRS");
    if (!t.BondTRSData)
        return result;
    const auto& d = *t.BondTRSData;

    map_bond_data(d.BondData, result.issue);

    bond_trs trs;
    trs.return_type = "TotalReturn";
    const auto& ld = d.FundingData.LegData;
    if (ld.legDataType) {
        if (ld.legDataType->FloatingLegData) {
            trs.funding_leg_type = "Floating";
            trs.funding_index = std::string(ld.legDataType->FloatingLegData->Index);
        } else if (ld.legDataType->FixedLegData) {
            trs.funding_leg_type = "Fixed";
            if (!ld.legDataType->FixedLegData->Rates.Rate.empty())
                trs.funding_rate =
                    static_cast<double>(ld.legDataType->FixedLegData->Rates.Rate.front());
        }
    }
    stamp_audit(trs);
    result.trs = trs;
    return result;
}

// ---------------------------------------------------------------------------
// Forward: BondRepo
// ---------------------------------------------------------------------------

bond_instrument_data bond_instrument_mapper::forward_bond_repo(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping BondRepo: " << std::string(t.id);
    bond_instrument_data result = make_base("BondRepo");
    if (!t.BondRepoData)
        return result;
    const auto& d = *t.BondRepoData;

    map_bond_data(d.BondData, result.issue);

    // The repo row captures the financing leg; the collateral terms sit
    // in the issue row. The legacy mapper overwrote the issue's coupon
    // frequency with the leg type here; that corruption does not survive
    // the shared issue row.
    bond_repo repo;
    repo.repo_type = (d.RepoData.LegData.LegType == legType::Floating) ? "Floating" : "Fixed";
    if (d.RepoData.LegData.legDataType) {
        const auto& rl = d.RepoData.LegData.legDataType;
        if (rl->FixedLegData && !rl->FixedLegData->Rates.Rate.empty())
            repo.repo_rate = static_cast<double>(rl->FixedLegData->Rates.Rate.front());
        else if (rl->FloatingLegData)
            repo.repo_index = std::string(rl->FloatingLegData->Index);
    }
    stamp_audit(repo);
    result.repo = repo;
    return result;
}

// ---------------------------------------------------------------------------
// Reverse: Bond
// ---------------------------------------------------------------------------

trade bond_instrument_mapper::reverse_bond(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping Bond";
    trade t;
    t.TradeType = oreTradeType::Bond;
    t.BondData = reverse_bond_data(data.issue);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: ForwardBond
// ---------------------------------------------------------------------------

trade bond_instrument_mapper::reverse_forward_bond(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping ForwardBond";
    trade t;
    t.TradeType = oreTradeType::ForwardBond;
    forwardBondData fbd;
    fbd.BondData = reverse_bond_data(data.issue);
    t.ForwardBondData = std::move(fbd);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: CallableBond
// ---------------------------------------------------------------------------

trade bond_instrument_mapper::reverse_callable_bond(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CallableBond";
    trade t;
    t.TradeType = oreTradeType::CallableBond;
    callableBondData cbd;
    cbd.BondData = reverse_bond_data(data.issue);
    t.CallableBondData = std::move(cbd);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: ConvertibleBond
// ---------------------------------------------------------------------------

trade bond_instrument_mapper::reverse_convertible_bond(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping ConvertibleBond";
    trade t;
    t.TradeType = oreTradeType::ConvertibleBond;
    convertibleBondData cvbd;
    cvbd.BondData = reverse_bond_data(data.issue);
    t.ConvertibleBondData = std::move(cvbd);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: BondOption
// ---------------------------------------------------------------------------

trade bond_instrument_mapper::reverse_bond_option(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping BondOption";
    trade t;
    t.TradeType = oreTradeType::BondOption;
    bondOptionData d;
    d.BondData = reverse_bond_data(data.issue);
    if (data.option) {
        if (!data.option->option_type.empty()) {
            optionData_OptionType_t ot;
            static_cast<std::string&>(ot) = data.option->option_type;
            d.OptionData.OptionType = std::move(ot);
        }
        if (data.option->option_strike != 0.0) {
            _Strike_t s;
            static_cast<std::string&>(s) = std::to_string(data.option->option_strike);
            d.strikeGroup.Strike = std::move(s);
        }
    }
    if (!data.option_expiry_date.empty()) {
        _ExerciseDates_t exd;
        date ed;
        static_cast<std::string&>(ed) = data.option_expiry_date;
        exd.ExerciseDate.push_back(ed);
        exerciseDatesGroup_group_t eg;
        eg.ExerciseDates = std::move(exd);
        d.OptionData.exerciseDatesGroup = std::move(eg);
    }
    t.BondOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: BondTRS
// ---------------------------------------------------------------------------

trade bond_instrument_mapper::reverse_bond_trs(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping BondTRS";
    trade t;
    t.TradeType = oreTradeType::BondTRS;
    bondTRSData d;
    d.BondData = reverse_bond_data(data.issue);
    // Minimal TotalReturnData — schedule required by XSD
    totalReturnData_PriceType_t pt;
    static_cast<std::string&>(pt) = "Dirty";
    d.TotalReturnData.PriceType = std::move(pt);
    // Reconstruct funding leg from the captured type and index.
    d.FundingData.LegData.Payer = false;
    const bool fixed = !data.trs || data.trs->funding_leg_type.empty() ||
                       data.trs->funding_leg_type == "Fixed";
    if (fixed) {
        d.FundingData.LegData.LegType = legType::Fixed;
    } else if (data.trs) {
        d.FundingData.LegData.LegType = legType::Floating;
        _FloatingLegData_t fld;
        static_cast<std::string&>(fld.Index) = data.trs->funding_index;
        legDataType_group_t ldt;
        ldt.FloatingLegData = std::move(fld);
        d.FundingData.LegData.legDataType = std::move(ldt);
    }
    t.BondTRSData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: BondRepo
// ---------------------------------------------------------------------------

trade bond_instrument_mapper::reverse_bond_repo(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping BondRepo";
    trade t;
    t.TradeType = oreTradeType::BondRepo;
    bondRepoData d;
    d.BondData = reverse_bond_data(data.issue);
    // Reconstruct a minimal repo leg (fixed, non-payer)
    d.RepoData.LegData.Payer = false;
    d.RepoData.LegData.LegType = legType::Fixed;
    t.BondRepoData = std::move(d);
    return t;
}

}
