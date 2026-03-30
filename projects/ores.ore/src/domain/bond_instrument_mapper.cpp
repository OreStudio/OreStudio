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
    if (bd.IssuerId)
        instr.issuer = std::string(*bd.IssuerId);
    if (bd.IssueDate)
        instr.issue_date = std::string(*bd.IssueDate);
    if (bd.SettlementDays && !std::string(*bd.SettlementDays).empty())
        instr.settlement_days = std::stoi(std::string(*bd.SettlementDays));

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

}
