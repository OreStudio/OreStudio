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
#include "ores.ore/domain/credit_instrument_mapper.hpp"

namespace ores::ore::domain {

using namespace ores::logging;
using ores::trading::domain::credit_instrument;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

namespace {

credit_instrument make_base(const std::string& trade_type_code) {
    credit_instrument r;
    r.trade_type_code = trade_type_code;
    r.modified_by = "ores";
    r.performed_by = "ores";
    r.change_reason_code = "system.external_data_import";
    r.change_commentary = "Imported from ORE XML";
    return r;
}

std::string first_exercise_date(const optionData& opt) {
    if (!opt.exerciseDatesGroup) return {};
    if (!opt.exerciseDatesGroup->ExerciseDates) return {};
    if (opt.exerciseDatesGroup->ExerciseDates->ExerciseDate.empty()) return {};
    return std::string(
        opt.exerciseDatesGroup->ExerciseDates->ExerciseDate.front());
}

} // namespace

// ---------------------------------------------------------------------------
// Common: legData → credit_instrument economic fields
// ---------------------------------------------------------------------------

void credit_instrument_mapper::map_cds_leg(
        const legData& ld, credit_instrument& instr) {
    if (ld.Currency)
        instr.currency = std::string(*ld.Currency);
    if (ld.Notionals && !ld.Notionals->Notional.empty())
        instr.notional = static_cast<double>(ld.Notionals->Notional.front());
    if (ld.DayCounter)
        instr.day_count_code = to_string(*ld.DayCounter);
    if (ld.legDataType && ld.legDataType->FixedLegData &&
            !ld.legDataType->FixedLegData->Rates.Rate.empty())
        instr.spread = static_cast<double>(
            ld.legDataType->FixedLegData->Rates.Rate.front());
    if (!ld.ScheduleData || ld.ScheduleData->Rules.empty()) return;
    const auto& rule = ld.ScheduleData->Rules.front();
    instr.start_date = std::string(rule.StartDate);
    if (rule.EndDate)
        instr.maturity_date = std::string(*rule.EndDate);
    instr.payment_frequency_code = std::string(rule.Tenor);
}

// ---------------------------------------------------------------------------
// Common: credit_instrument → legData
// ---------------------------------------------------------------------------

legData credit_instrument_mapper::reverse_cds_leg(
        const credit_instrument& instr) {
    legData ld;
    ld.LegType = legType::Fixed;
    ld.Payer = true;
    if (!instr.currency.empty())
        ld.Currency = instr.currency;
    if (instr.notional != 0.0) {
        legData_Notionals_t n;
        legData_Notionals_t_Notional_t nv;
        static_cast<float&>(nv) = static_cast<float>(instr.notional);
        n.Notional.push_back(nv);
        ld.Notionals = std::move(n);
    }
    if (instr.spread != 0.0) {
        _FixedLegData_t fld;
        _FixedLegData_t_Rates_t_Rate_t rate;
        static_cast<float&>(rate) = static_cast<float>(instr.spread);
        fld.Rates.Rate.push_back(rate);
        legDataType_group_t ldt;
        ldt.FixedLegData = std::move(fld);
        ld.legDataType = std::move(ldt);
    }
    if (!instr.start_date.empty() || !instr.maturity_date.empty()) {
        scheduleData_Rules_t rule;
        if (!instr.start_date.empty()) {
            date sd;
            static_cast<std::string&>(sd) = instr.start_date;
            rule.StartDate = xsd::optional<date>(sd);
        }
        if (!instr.maturity_date.empty()) {
            date ed;
            static_cast<std::string&>(ed) = instr.maturity_date;
            rule.EndDate = xsd::optional<date>(ed);
        }
        if (!instr.payment_frequency_code.empty())
            static_cast<std::string&>(rule.Tenor) = instr.payment_frequency_code;
        scheduleData sched;
        sched.Rules.push_back(std::move(rule));
        ld.ScheduleData = std::move(sched);
    }
    return ld;
}

// ---------------------------------------------------------------------------
// Forward: CreditDefaultSwap
// ---------------------------------------------------------------------------

credit_mapping_result credit_instrument_mapper::forward_cds(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CreditDefaultSwap: "
                               << std::string(t.id);
    credit_mapping_result result;
    result.instrument = make_base("CreditDefaultSwap");
    if (!t.CreditDefaultSwapData) return result;
    const auto& d = *t.CreditDefaultSwapData;
    if (d.IssuerId)
        result.instrument.reference_entity = std::string(*d.IssuerId);
    else if (d.creditCurveIdType.CreditCurveId)
        result.instrument.reference_entity =
            std::string(*d.creditCurveIdType.CreditCurveId);
    if (d.FixedRecoveryRate)
        result.instrument.recovery_rate =
            static_cast<double>(*d.FixedRecoveryRate);
    map_cds_leg(d.LegData, result.instrument);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: IndexCreditDefaultSwap
// ---------------------------------------------------------------------------

credit_mapping_result
credit_instrument_mapper::forward_index_cds(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping IndexCreditDefaultSwap: "
                               << std::string(t.id);
    credit_mapping_result result;
    result.instrument = make_base("IndexCreditDefaultSwap");
    if (!t.IndexCreditDefaultSwapData) return result;
    const auto& d = *t.IndexCreditDefaultSwapData;
    result.instrument.reference_entity = std::string(d.CreditCurveId);
    result.instrument.index_name = std::string(d.CreditCurveId);
    map_cds_leg(d.LegData, result.instrument);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: IndexCreditDefaultSwapOption
// ---------------------------------------------------------------------------

credit_mapping_result
credit_instrument_mapper::forward_index_cds_option(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping IndexCreditDefaultSwapOption: "
                               << std::string(t.id);
    credit_mapping_result result;
    result.instrument = make_base("IndexCreditDefaultSwapOption");
    if (!t.IndexCreditDefaultSwapOptionData) return result;
    const auto& d = *t.IndexCreditDefaultSwapOptionData;
    result.instrument.reference_entity = std::string(d.IndexCreditDefaultSwapData.CreditCurveId);
    result.instrument.index_name = std::string(d.IndexCreditDefaultSwapData.CreditCurveId);
    if (d.Strike)
        result.instrument.option_strike = static_cast<double>(*d.Strike);
    result.instrument.option_expiry_date = first_exercise_date(d.OptionData);
    map_cds_leg(d.IndexCreditDefaultSwapData.LegData, result.instrument);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: CreditLinkedSwap
// ---------------------------------------------------------------------------

credit_mapping_result
credit_instrument_mapper::forward_credit_linked_swap(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CreditLinkedSwap: "
                               << std::string(t.id);
    credit_mapping_result result;
    result.instrument = make_base("CreditLinkedSwap");
    if (!t.CreditLinkedSwapData) return result;
    const auto& d = *t.CreditLinkedSwapData;
    result.instrument.reference_entity = std::string(d.CreditCurveId);
    result.instrument.linked_asset_code = std::string(d.CreditCurveId);
    if (d.FixedRecoveryRate)
        result.instrument.recovery_rate = static_cast<double>(*d.FixedRecoveryRate);
    // Extract currency/notional from first contingent payment leg if available.
    if (d.ContingentPayments && !d.ContingentPayments->LegData.empty())
        map_cds_leg(d.ContingentPayments->LegData.front(), result.instrument);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: SyntheticCDO
// ---------------------------------------------------------------------------

credit_mapping_result
credit_instrument_mapper::forward_synthetic_cdo(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping SyntheticCDO: "
                               << std::string(t.id);
    credit_mapping_result result;
    result.instrument = make_base("SyntheticCDO");
    if (!t.CdoData) return result;
    const auto& d = *t.CdoData;
    result.instrument.reference_entity = std::string(d.Qualifier);
    result.instrument.start_date = std::string(d.ProtectionStart);
    result.instrument.tranche_attachment = static_cast<double>(d.AttachmentPoint);
    result.instrument.tranche_detachment = static_cast<double>(d.DetachmentPoint);
    if (d.FixedRecoveryRate)
        result.instrument.recovery_rate = static_cast<double>(*d.FixedRecoveryRate);
    map_cds_leg(d.LegData, result.instrument);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: RiskParticipationAgreement
// ---------------------------------------------------------------------------

credit_mapping_result
credit_instrument_mapper::forward_rpa(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping RiskParticipationAgreement: "
                               << std::string(t.id);
    credit_mapping_result result;
    result.instrument = make_base("RiskParticipationAgreement");
    if (!t.RiskParticipationAgreementData) return result;
    const auto& d = *t.RiskParticipationAgreementData;
    result.instrument.reference_entity = std::string(d.CreditCurveId);
    if (d.IssuerId)
        result.instrument.reference_entity = std::string(*d.IssuerId);
    result.instrument.start_date = std::string(d.ProtectionStart);
    result.instrument.maturity_date = std::string(d.ProtectionEnd);
    if (d.FixedRecoveryRate)
        result.instrument.recovery_rate = static_cast<double>(*d.FixedRecoveryRate);
    return result;
}

// ---------------------------------------------------------------------------
// Reverse: CreditDefaultSwap
// ---------------------------------------------------------------------------

trade credit_instrument_mapper::reverse_cds(
        const credit_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CreditDefaultSwap";
    trade t;
    t.TradeType = oreTradeType::CreditDefaultSwap;
    creditDefaultSwapData d;
    if (!instr.reference_entity.empty()) {
        _CreditCurveId_t cid;
        static_cast<std::string&>(cid) = instr.reference_entity;
        d.creditCurveIdType.CreditCurveId = std::move(cid);
    }
    if (instr.recovery_rate != 0.0)
        d.FixedRecoveryRate = static_cast<float>(instr.recovery_rate);
    d.LegData = reverse_cds_leg(instr);
    t.CreditDefaultSwapData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: IndexCreditDefaultSwap
// ---------------------------------------------------------------------------

trade credit_instrument_mapper::reverse_index_cds(
        const credit_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping IndexCreditDefaultSwap";
    trade t;
    t.TradeType = oreTradeType::IndexCreditDefaultSwap;
    indexCreditDefaultSwapData d;
    static_cast<std::string&>(d.CreditCurveId) = instr.reference_entity;
    d.LegData = reverse_cds_leg(instr);
    t.IndexCreditDefaultSwapData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: IndexCreditDefaultSwapOption
// ---------------------------------------------------------------------------

trade credit_instrument_mapper::reverse_index_cds_option(
        const credit_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping IndexCreditDefaultSwapOption";
    trade t;
    t.TradeType = oreTradeType::IndexCreditDefaultSwapOption;
    indexCreditDefaultSwapOptionData d;
    static_cast<std::string&>(d.IndexCreditDefaultSwapData.CreditCurveId) =
        instr.reference_entity;
    d.IndexCreditDefaultSwapData.LegData = reverse_cds_leg(instr);
    if (instr.option_strike)
        d.Strike = static_cast<float>(*instr.option_strike);
    if (!instr.option_expiry_date.empty()) {
        _ExerciseDates_t exd;
        date ed;
        static_cast<std::string&>(ed) = instr.option_expiry_date;
        exd.ExerciseDate.push_back(ed);
        exerciseDatesGroup_group_t eg;
        eg.ExerciseDates = std::move(exd);
        d.OptionData.exerciseDatesGroup = std::move(eg);
    }
    t.IndexCreditDefaultSwapOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: CreditLinkedSwap
// ---------------------------------------------------------------------------

trade credit_instrument_mapper::reverse_credit_linked_swap(
        const credit_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CreditLinkedSwap";
    trade t;
    t.TradeType = oreTradeType::CreditLinkedSwap;
    creditLinkedSwapData d;
    static_cast<std::string&>(d.CreditCurveId) = instr.reference_entity;
    if (instr.recovery_rate != 0.0)
        d.FixedRecoveryRate = static_cast<float>(instr.recovery_rate);
    if (!instr.currency.empty() || instr.notional != 0.0) {
        creditLinkedSwapData_ContingentPayments_t cp;
        cp.LegData.push_back(reverse_cds_leg(instr));
        d.ContingentPayments = std::move(cp);
    }
    t.CreditLinkedSwapData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: SyntheticCDO
// ---------------------------------------------------------------------------

trade credit_instrument_mapper::reverse_synthetic_cdo(
        const credit_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping SyntheticCDO";
    trade t;
    t.TradeType = oreTradeType::SyntheticCDO;
    cdoData d;
    static_cast<std::string&>(d.Qualifier) = instr.reference_entity;
    if (!instr.start_date.empty())
        static_cast<std::string&>(d.ProtectionStart) = instr.start_date;
    if (instr.tranche_attachment)
        d.AttachmentPoint = static_cast<float>(*instr.tranche_attachment);
    if (instr.tranche_detachment)
        d.DetachmentPoint = static_cast<float>(*instr.tranche_detachment);
    if (instr.recovery_rate != 0.0)
        d.FixedRecoveryRate = static_cast<float>(instr.recovery_rate);
    d.LegData = reverse_cds_leg(instr);
    t.CdoData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: RiskParticipationAgreement
// ---------------------------------------------------------------------------

trade credit_instrument_mapper::reverse_rpa(
        const credit_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping RiskParticipationAgreement";
    trade t;
    t.TradeType = oreTradeType::RiskParticipationAgreement;
    rpaData d;
    static_cast<std::string&>(d.CreditCurveId) = instr.reference_entity;
    if (!instr.start_date.empty())
        static_cast<std::string&>(d.ProtectionStart) = instr.start_date;
    if (!instr.maturity_date.empty())
        static_cast<std::string&>(d.ProtectionEnd) = instr.maturity_date;
    if (instr.recovery_rate != 0.0)
        d.FixedRecoveryRate = static_cast<float>(instr.recovery_rate);
    t.RiskParticipationAgreementData = std::move(d);
    return t;
}

}
