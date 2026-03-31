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
#include "ores.ore/domain/bond_instrument_mapper.hpp"

#include <stdexcept>

namespace ores::ore::domain {

using namespace ores::logging;
using ores::trading::domain::bond_instrument;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

namespace {

bond_instrument make_base(const std::string& trade_type_code) {
    bond_instrument r;
    r.trade_type_code = trade_type_code;
    r.modified_by = "ores";
    r.performed_by = "ores";
    r.change_reason_code = "system.external_data_import";
    r.change_commentary = "Imported from ORE XML";
    return r;
}

std::string first_tenor(const xsd::optional<scheduleData>& sd) {
    if (!sd || sd->Rules.empty()) return {};
    return std::string(sd->Rules.front().Tenor);
}

} // namespace

// ---------------------------------------------------------------------------
// Common: map bondData fields → bond_instrument
// ---------------------------------------------------------------------------

void bond_instrument_mapper::map_bond_data(
        const bondData& bd, bond_instrument& instr) {
    instr.security_id = std::string(bd.SecurityId);
    if (bd.IssuerId)
        instr.issuer = std::string(*bd.IssuerId);
    if (bd.IssueDate)
        instr.issue_date = std::string(*bd.IssueDate);
    if (bd.SettlementDays) {
        const std::string settlement_days_str(*bd.SettlementDays);
        if (!settlement_days_str.empty())
            instr.settlement_days = std::stoi(settlement_days_str);
    }

    if (!bd.LegData.empty()) {
        const auto& ld = bd.LegData.front();
        if (ld.Currency)
            instr.currency = std::string(*ld.Currency);
        if (ld.Notionals && !ld.Notionals->Notional.empty())
            instr.face_value = static_cast<double>(
                ld.Notionals->Notional.front());
        if (ld.DayCounter)
            instr.day_count_code = to_string(*ld.DayCounter);
        instr.coupon_frequency_code = first_tenor(ld.ScheduleData);

        if (ld.legDataType && ld.legDataType->FixedLegData &&
                !ld.legDataType->FixedLegData->Rates.Rate.empty())
            instr.coupon_rate = static_cast<double>(
                ld.legDataType->FixedLegData->Rates.Rate.front());

        if (ld.ScheduleData && !ld.ScheduleData->Rules.empty()) {
            const auto& rule = ld.ScheduleData->Rules.front();
            if (rule.EndDate)
                instr.maturity_date = std::string(*rule.EndDate);
        }
    }
}

// ---------------------------------------------------------------------------
// Common: bond_instrument → bondData
// ---------------------------------------------------------------------------

bondData bond_instrument_mapper::reverse_bond_data(
        const bond_instrument& instr) {
    bondData bd;

    static_cast<std::string&>(bd.SecurityId) = instr.security_id;
    if (!instr.issuer.empty()) {
        bondData_IssuerId_t id;
        static_cast<std::string&>(id) = instr.issuer;
        bd.IssuerId = std::move(id);
    }
    if (!instr.issue_date.empty()) {
        bondData_IssueDate_t d;
        static_cast<std::string&>(d) = instr.issue_date;
        bd.IssueDate = std::move(d);
    }
    if (instr.settlement_days != 0) {
        bondData_SettlementDays_t sd;
        static_cast<std::string&>(sd) = std::to_string(instr.settlement_days);
        bd.SettlementDays = std::move(sd);
    }

    if (!instr.currency.empty() || instr.face_value != 0.0) {
        legData ld;
        ld.LegType = legType::Fixed;

        if (!instr.currency.empty())
            ld.Currency = instr.currency;

        if (instr.face_value != 0.0) {
            legData_Notionals_t n;
            legData_Notionals_t_Notional_t nv;
            static_cast<float&>(nv) = static_cast<float>(instr.face_value);
            n.Notional.push_back(nv);
            ld.Notionals = std::move(n);
        }

        if (!instr.maturity_date.empty() || !instr.coupon_frequency_code.empty()) {
            scheduleData_Rules_t rule;
            if (!instr.maturity_date.empty()) {
                domain::date d;
                static_cast<std::string&>(d) = instr.maturity_date;
                rule.EndDate = xsd::optional<domain::date>(d);
            }
            if (!instr.coupon_frequency_code.empty())
                static_cast<std::string&>(rule.Tenor) = instr.coupon_frequency_code;
            if (!instr.issue_date.empty()) {
                domain::date sd;
                static_cast<std::string&>(sd) = instr.issue_date;
                rule.StartDate = xsd::optional<domain::date>(sd);
            }
            scheduleData sched;
            sched.Rules.push_back(std::move(rule));
            ld.ScheduleData = std::move(sched);
        }

        if (instr.coupon_rate != 0.0) {
            _FixedLegData_t fld;
            _FixedLegData_t_Rates_t_Rate_t rate;
            static_cast<float&>(rate) = static_cast<float>(instr.coupon_rate);
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

bond_mapping_result bond_instrument_mapper::forward_bond(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping Bond: " << std::string(t.id);
    bond_mapping_result result;
    result.instrument = make_base("Bond");
    if (t.BondData)
        map_bond_data(*t.BondData, result.instrument);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: ForwardBond
// ---------------------------------------------------------------------------

bond_mapping_result bond_instrument_mapper::forward_forward_bond(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping ForwardBond: "
                               << std::string(t.id);
    bond_mapping_result result;
    result.instrument = make_base("ForwardBond");
    if (t.ForwardBondData)
        map_bond_data(t.ForwardBondData->BondData, result.instrument);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: CallableBond
// ---------------------------------------------------------------------------

bond_mapping_result bond_instrument_mapper::forward_callable_bond(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CallableBond: "
                               << std::string(t.id);
    bond_mapping_result result;
    result.instrument = make_base("CallableBond");
    if (t.CallableBondData)
        map_bond_data(t.CallableBondData->BondData, result.instrument);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: ConvertibleBond
// ---------------------------------------------------------------------------

bond_mapping_result bond_instrument_mapper::forward_convertible_bond(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping ConvertibleBond: "
                               << std::string(t.id);
    bond_mapping_result result;
    result.instrument = make_base("ConvertibleBond");
    if (t.ConvertibleBondData)
        map_bond_data(t.ConvertibleBondData->BondData, result.instrument);
    return result;
}

// ---------------------------------------------------------------------------
// Reverse: Bond
// ---------------------------------------------------------------------------

trade bond_instrument_mapper::reverse_bond(const bond_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping Bond";
    trade t;
    t.TradeType = oreTradeType::Bond;
    t.BondData = reverse_bond_data(instr);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: ForwardBond
// ---------------------------------------------------------------------------

trade bond_instrument_mapper::reverse_forward_bond(
        const bond_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping ForwardBond";
    trade t;
    t.TradeType = oreTradeType::ForwardBond;
    forwardBondData fbd;
    fbd.BondData = reverse_bond_data(instr);
    t.ForwardBondData = std::move(fbd);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: CallableBond
// ---------------------------------------------------------------------------

trade bond_instrument_mapper::reverse_callable_bond(
        const bond_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CallableBond";
    trade t;
    t.TradeType = oreTradeType::CallableBond;
    callableBondData cbd;
    cbd.BondData = reverse_bond_data(instr);
    t.CallableBondData = std::move(cbd);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: ConvertibleBond
// ---------------------------------------------------------------------------

trade bond_instrument_mapper::reverse_convertible_bond(
        const bond_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping ConvertibleBond";
    trade t;
    t.TradeType = oreTradeType::ConvertibleBond;
    convertibleBondData cvbd;
    cvbd.BondData = reverse_bond_data(instr);
    t.ConvertibleBondData = std::move(cvbd);
    return t;
}

// ---------------------------------------------------------------------------
// Forward: BondOption
// ---------------------------------------------------------------------------

bond_mapping_result bond_instrument_mapper::forward_bond_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping BondOption: "
                               << std::string(t.id);
    bond_mapping_result result;
    result.instrument = make_base("BondOption");
    if (!t.BondOptionData) return result;
    const auto& d = *t.BondOptionData;

    map_bond_data(d.BondData, result.instrument);

    // Option fields
    if (d.OptionData.OptionType)
        result.instrument.option_type = std::string(*d.OptionData.OptionType);
    if (d.OptionData.exerciseDatesGroup &&
            d.OptionData.exerciseDatesGroup->ExerciseDates &&
            !d.OptionData.exerciseDatesGroup->ExerciseDates->ExerciseDate.empty())
        result.instrument.option_expiry_date = std::string(
            d.OptionData.exerciseDatesGroup->ExerciseDates->ExerciseDate.front());
    if (d.strikeGroup.Strike) {
        const std::string s(*d.strikeGroup.Strike);
        if (!s.empty())
            result.instrument.option_strike = std::stod(s);
    }

    return result;
}

// ---------------------------------------------------------------------------
// Forward: BondTRS
// ---------------------------------------------------------------------------

bond_mapping_result bond_instrument_mapper::forward_bond_trs(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping BondTRS: "
                               << std::string(t.id);
    bond_mapping_result result;
    result.instrument = make_base("BondTRS");
    if (!t.BondTRSData) return result;
    const auto& d = *t.BondTRSData;

    map_bond_data(d.BondData, result.instrument);

    result.instrument.trs_return_type = "TotalReturn";

    // Funding leg: capture index if floating, otherwise note fixed rate
    const auto& ld = d.FundingData.LegData;
    if (ld.legDataType) {
        if (ld.legDataType->FloatingLegData)
            result.instrument.trs_funding_leg_code =
                std::string(ld.legDataType->FloatingLegData->Index);
        else if (ld.legDataType->FixedLegData &&
                !ld.legDataType->FixedLegData->Rates.Rate.empty())
            result.instrument.trs_funding_leg_code = "Fixed";
    }

    return result;
}

// ---------------------------------------------------------------------------
// Reverse: BondOption
// ---------------------------------------------------------------------------

trade bond_instrument_mapper::reverse_bond_option(
        const bond_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping BondOption";
    trade t;
    t.TradeType = oreTradeType::BondOption;
    bondOptionData d;
    d.BondData = reverse_bond_data(instr);
    if (!instr.option_type.empty()) {
        optionData_OptionType_t ot;
        static_cast<std::string&>(ot) = instr.option_type;
        d.OptionData.OptionType = std::move(ot);
    }
    if (!instr.option_expiry_date.empty()) {
        _ExerciseDates_t exd;
        date ed;
        static_cast<std::string&>(ed) = instr.option_expiry_date;
        exd.ExerciseDate.push_back(ed);
        exerciseDatesGroup_group_t eg;
        eg.ExerciseDates = std::move(exd);
        d.OptionData.exerciseDatesGroup = std::move(eg);
    }
    if (instr.option_strike) {
        _Strike_t s;
        static_cast<std::string&>(s) = std::to_string(*instr.option_strike);
        d.strikeGroup.Strike = std::move(s);
    }
    t.BondOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: BondTRS
// ---------------------------------------------------------------------------

trade bond_instrument_mapper::reverse_bond_trs(
        const bond_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping BondTRS";
    trade t;
    t.TradeType = oreTradeType::BondTRS;
    bondTRSData d;
    d.BondData = reverse_bond_data(instr);
    // Minimal TotalReturnData — schedule required by XSD
    totalReturnData_PriceType_t pt;
    static_cast<std::string&>(pt) = "Dirty";
    d.TotalReturnData.PriceType = std::move(pt);
    // Reconstruct funding leg from captured code.
    d.FundingData.LegData.Payer = false;
    if (instr.trs_funding_leg_code.empty() ||
            instr.trs_funding_leg_code == "Fixed") {
        d.FundingData.LegData.LegType = legType::Fixed;
    } else {
        d.FundingData.LegData.LegType = legType::Floating;
        _FloatingLegData_t fld;
        static_cast<std::string&>(fld.Index) = instr.trs_funding_leg_code;
        legDataType_group_t ldt;
        ldt.FloatingLegData = std::move(fld);
        d.FundingData.LegData.legDataType = std::move(ldt);
    }
    t.BondTRSData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Forward: BondRepo
// ---------------------------------------------------------------------------

bond_mapping_result bond_instrument_mapper::forward_bond_repo(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping BondRepo: "
                               << std::string(t.id);
    bond_mapping_result result;
    result.instrument = make_base("BondRepo");
    if (!t.BondRepoData) return result;
    const auto& d = *t.BondRepoData;
    map_bond_data(d.BondData, result.instrument);
    // Repo leg: capture payment frequency from leg type
    if (d.RepoData.LegData.LegType == legType::Floating) {
        result.instrument.coupon_frequency_code = "Quarterly";
    } else {
        result.instrument.coupon_frequency_code = "Maturity";
    }
    return result;
}

// ---------------------------------------------------------------------------
// Reverse: BondRepo
// ---------------------------------------------------------------------------

trade bond_instrument_mapper::reverse_bond_repo(const bond_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping BondRepo";
    trade t;
    t.TradeType = oreTradeType::BondRepo;
    bondRepoData d;
    d.BondData = reverse_bond_data(instr);
    // Reconstruct a minimal repo leg (fixed, non-payer)
    d.RepoData.LegData.Payer = false;
    d.RepoData.LegData.LegType = legType::Fixed;
    t.BondRepoData = std::move(d);
    return t;
}

}
