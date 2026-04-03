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
#include "ores.ore/domain/conventions_mapper.hpp"

#include <algorithm>

namespace ores::ore::domain {

using namespace ores::logging;

namespace {

constexpr std::string_view audit_modified_by      = "ores";
constexpr std::string_view audit_reason_code      = "system.external_data_import";
constexpr std::string_view audit_commentary       = "Imported from ORE XML";

template<typename T>
void set_audit(T& r) {
    r.modified_by        = std::string(audit_modified_by);
    r.change_reason_code = std::string(audit_reason_code);
    r.change_commentary  = std::string(audit_commentary);
}

} // namespace

// ---------------------------------------------------------------------------
// Normalisation helpers
// ---------------------------------------------------------------------------

std::string conventions_mapper::normalize_bdc(domain::businessDayConvention v) {
    using bdc = domain::businessDayConvention;
    switch (v) {
    case bdc::F:
    case bdc::Following:
    case bdc::FOLLOWING:
        return "Following";
    case bdc::MF:
    case bdc::ModifiedFollowing:
    case bdc::Modified_Following:
    case bdc::MODIFIEDF:
    case bdc::MODFOLLOWING:
        return "ModifiedFollowing";
    case bdc::P:
    case bdc::Preceding:
    case bdc::PRECEDING:
        return "Preceding";
    case bdc::MP:
    case bdc::ModifiedPreceding:
    case bdc::Modified_Preceding:
    case bdc::MODIFIEDP:
        return "ModifiedPreceding";
    case bdc::HMMF:
    case bdc::HalfMonthModifiedFollowing:
    case bdc::HalfMonthMF:
    case bdc::Half_Month_Modified_Following:
    case bdc::HALFMONTHMF:
        return "HalfMonthModifiedFollowing";
    case bdc::NEAREST:
        return "Nearest";
    case bdc::NONE:
    case bdc::NotApplicable:
        return "Unadjusted";
    case bdc::U:
    case bdc::Unadjusted:
    case bdc::INDIFF:
    case bdc::_:
    default:
        return "Unadjusted";
    }
}

std::string conventions_mapper::normalize_day_counter(domain::dayCounter v) {
    using dc = domain::dayCounter;
    switch (v) {
    case dc::A360:
    case dc::Actual_360:
    case dc::ACT_360:
    case dc::Act_360:
        return "ACT/360";
    case dc::A360__Incl_Last_:
    case dc::Actual_360__Incl_Last_:
    case dc::ACT_360__Incl_Last_:
        return "ACT/360 (incl. last)";
    case dc::A365:
    case dc::A365F:
    case dc::Actual_365__Fixed_:
    case dc::Actual_365__fixed_:
    case dc::ACT_365_FIXED:
    case dc::ACT_365:
    case dc::Act_365:
        return "ACT/365.FIXED";
    case dc::ACT_365L:
    case dc::Act_365L:
        return "ACT/365L";
    case dc::Act_365__Canadian_Bond_:
        return "ACT/365 (Canadian Bond)";
    case dc::T360:
        return "T360";
    case dc::_30_360:
    case dc::_30_360_US:
    case dc::_30_360__US_:
    case dc::_30_360_NASD:
    case dc::_30U_360:
    case dc::_30US_360:
    case dc::_30_360__Bond_Basis_:
        return "30/360";
    case dc::ACT_nACT:
        return "ACT/nACT";
    case dc::_30E_360__Eurobond_Basis_:
    case dc::_30_360_AIBD__Euro_:
    case dc::_30E_360_ICMA:
    case dc::_30E_360_ICMA_2:
    case dc::_30E_360:
    case dc::_30E_360E:
        return "30E/360";
    case dc::_30E_360_ISDA:
    case dc::_30E_360_ISDA_2:
        return "30E/360.ISDA";
    case dc::_30_360_German:
    case dc::_30_360__German_:
        return "30/360 (German)";
    case dc::_30_360_Italian:
    case dc::_30_360__Italian_:
        return "30/360 (Italian)";
    case dc::ActActISDA:
    case dc::ACT_ACT_ISDA:
    case dc::Actual_Actual__ISDA_:
    case dc::ActualActual__ISDA_:
    case dc::ACT_ACT:
    case dc::Act_Act:
    case dc::ACT29:
    case dc::ACT:
        return "ACT/ACT.ISDA";
    case dc::ActActISMA:
    case dc::Actual_Actual__ISMA_:
    case dc::ActualActual__ISMA_:
    case dc::ACT_ACT_ISMA:
    case dc::ActActICMA:
    case dc::Actual_Actual__ICMA_:
    case dc::ActualActual__ICMA_:
    case dc::ACT_ACT_ICMA:
        return "ACT/ACT.ISMA";
    case dc::ActActAFB:
    case dc::ACT_ACT_AFB:
    case dc::Actual_Actual__AFB_:
        return "ACT/ACT.AFB";
    case dc::_1_1:
        return "1/1";
    case dc::BUS_252:
    case dc::Business_252:
        return "BUS/252";
    case dc::Actual_365__No_Leap_:
    case dc::Act_365__NL_:
    case dc::NL_365:
        return "NL/365";
    case dc::Actual_365__JGB_:
        return "ACT/365 (JGB)";
    case dc::Simple:
        return "Simple";
    case dc::Year:
        return "Year";
    case dc::A364:
    case dc::Actual_364:
    case dc::Act_364:
    case dc::ACT_364:
        return "ACT/364";
    case dc::Month:
        return "Month";
    default:
        return "ACT/365.FIXED";
    }
}

std::string conventions_mapper::normalize_frequency(domain::frequencyType v) {
    using ft = domain::frequencyType;
    switch (v) {
    case ft::Z:
    case ft::Once:
        return "Once";
    case ft::A:
    case ft::Annual:
        return "Annual";
    case ft::S:
    case ft::Semiannual:
        return "Semiannual";
    case ft::Q:
    case ft::Quarterly:
        return "Quarterly";
    case ft::B:
    case ft::Bimonthly:
        return "Bimonthly";
    case ft::M:
    case ft::Monthly:
        return "Monthly";
    case ft::L:
    case ft::Lunarmonth:
        return "Lunarmonth";
    case ft::W:
    case ft::Weekly:
        return "Weekly";
    case ft::D:
    case ft::Daily:
        return "Daily";
    default:
        return "Annual";
    }
}

std::string conventions_mapper::normalize_compounding(domain::compounding v) {
    using cm = domain::compounding;
    switch (v) {
    case cm::Simple:             return "Simple";
    case cm::Compounded:         return "Compounded";
    case cm::Continuous:         return "Continuous";
    case cm::SimpleThenCompounded: return "SimpleThenCompounded";
    case cm::_:
    default:                     return "Compounded";
    }
}

std::string conventions_mapper::normalize_date_rule(domain::dateRule v) {
    using dr = domain::dateRule;
    switch (v) {
    case dr::Backward:               return "Backward";
    case dr::Forward:                return "Forward";
    case dr::Zero:                   return "Zero";
    case dr::ThirdWednesday:         return "ThirdWednesday";
    case dr::Twentieth:              return "Twentieth";
    case dr::TwentiethIMM:           return "TwentiethIMM";
    case dr::OldCDS:                 return "OldCDS";
    case dr::CDS:                    return "CDS";
    case dr::CDS2015:                return "CDS2015";
    case dr::ThirdThursday:          return "ThirdThursday";
    case dr::ThirdFriday:            return "ThirdFriday";
    case dr::MondayAfterThirdFriday: return "MondayAfterThirdFriday";
    case dr::TuesdayAfterThirdFriday:return "TuesdayAfterThirdFriday";
    case dr::LastWednesday:          return "LastWednesday";
    case dr::EveryThursday:          return "EveryThursday";
    case dr::_:
    default:                         return "Backward";
    }
}

bool conventions_mapper::parse_bool(domain::bool_ v) {
    using b = domain::bool_;
    switch (v) {
    case b::Y:
    case b::YES:
    case b::TRUE_:
    case b::True:
    case b::true_:
    case b::_1:
        return true;
    default:
        return false;
    }
}

// ---------------------------------------------------------------------------
// Individual type mappers
// ---------------------------------------------------------------------------

refdata::domain::zero_convention
conventions_mapper::map_zero(const zeroType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping zero convention: " << std::string(v.Id);

    refdata::domain::zero_convention r;
    r.id = std::string(v.Id);
    r.tenor_based = parse_bool(v.TenorBased);
    r.day_count_fraction = normalize_day_counter(v.DayCounter);

    if (v.Compounding)
        r.compounding = normalize_compounding(*v.Compounding);

    if (v.CompoundingFrequency)
        r.compounding_frequency = normalize_frequency(*v.CompoundingFrequency);

    if (v.TenorCalendar)
        r.tenor_calendar = std::string(*v.TenorCalendar);

    if (v.SpotLag)
        r.spot_lag = static_cast<int>(*v.SpotLag);

    if (v.SpotCalendar)
        r.spot_calendar = std::string(*v.SpotCalendar);

    if (v.RollConvention)
        r.roll_convention = normalize_bdc(*v.RollConvention);

    if (v.EOM)
        r.end_of_month = parse_bool(*v.EOM);

    set_audit(r);
    return r;
}

refdata::domain::deposit_convention
conventions_mapper::map_deposit(const depositType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping deposit convention: " << std::string(v.Id);

    refdata::domain::deposit_convention r;
    r.id = std::string(v.Id);
    r.index_based = parse_bool(v.IndexBased);

    if (v.Index)
        r.index = std::string(*v.Index);

    if (v.Calendar)
        r.calendar = std::string(*v.Calendar);

    if (v.Convention)
        r.convention = normalize_bdc(*v.Convention);

    if (v.EOM)
        r.end_of_month = parse_bool(*v.EOM);

    if (v.DayCounter)
        r.day_count_fraction = normalize_day_counter(*v.DayCounter);

    if (v.SettlementDays)
        r.settlement_days = static_cast<int>(*v.SettlementDays);

    set_audit(r);
    return r;
}

refdata::domain::swap_convention
conventions_mapper::map_swap(const swapType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping swap convention: " << std::string(v.Id);

    refdata::domain::swap_convention r;
    r.id = std::string(v.Id);

    if (v.FixedCalendar)
        r.fixed_calendar = std::string(*v.FixedCalendar);

    r.fixed_frequency = normalize_frequency(v.FixedFrequency);

    if (v.FixedConvention)
        r.fixed_convention = normalize_bdc(*v.FixedConvention);

    r.fixed_day_count_fraction = normalize_day_counter(v.FixedDayCounter);
    r.index = std::string(v.Index);

    if (v.FloatFrequency)
        r.float_frequency = normalize_frequency(*v.FloatFrequency);

    if (v.SubPeriodsCouponType) {
        using sp = domain::subPeriodsCouponType;
        r.sub_periods_coupon_type =
            (*v.SubPeriodsCouponType == sp::Compounding) ? "Compounding" : "Averaging";
    }

    set_audit(r);
    return r;
}

refdata::domain::ois_convention
conventions_mapper::map_ois(const oisType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping OIS convention: " << std::string(v.Id);

    refdata::domain::ois_convention r;
    r.id = std::string(v.Id);
    r.spot_lag = static_cast<int>(v.SpotLag);
    r.index = std::string(v.Index);
    r.fixed_day_count_fraction = normalize_day_counter(v.FixedDayCounter);

    if (v.FixedCalendar)
        r.fixed_calendar = std::string(*v.FixedCalendar);

    if (v.PaymentLag)
        r.payment_lag = static_cast<int>(*v.PaymentLag);

    if (v.EOM)
        r.end_of_month = parse_bool(*v.EOM);

    if (v.FixedFrequency)
        r.fixed_frequency = normalize_frequency(*v.FixedFrequency);

    if (v.FixedConvention)
        r.fixed_convention = normalize_bdc(*v.FixedConvention);

    if (v.FixedPaymentConvention)
        r.fixed_payment_convention = normalize_bdc(*v.FixedPaymentConvention);

    if (v.Rule)
        r.rule = normalize_date_rule(*v.Rule);

    if (v.PaymentCalendar)
        r.payment_calendar = std::string(*v.PaymentCalendar);

    if (v.RateCutoff)
        r.rate_cutoff = static_cast<int>(*v.RateCutoff);

    set_audit(r);
    return r;
}

refdata::domain::fra_convention
conventions_mapper::map_fra(const fraType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping FRA convention: " << std::string(v.Id);

    refdata::domain::fra_convention r;
    r.id = std::string(v.Id);
    r.index = std::string(v.Index);
    set_audit(r);
    return r;
}

refdata::domain::ibor_index_convention
conventions_mapper::map_ibor_index(const iborIndexType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping IBOR index convention: " << std::string(v.Id);

    refdata::domain::ibor_index_convention r;
    r.id = std::string(v.Id);
    r.fixing_calendar = std::string(v.FixingCalendar);
    r.day_count_fraction = normalize_day_counter(v.DayCounter);
    r.settlement_days = static_cast<int>(v.SettlementDays);
    r.business_day_convention = normalize_bdc(v.BusinessDayConvention);
    r.end_of_month = parse_bool(v.EndOfMonth);
    set_audit(r);
    return r;
}

refdata::domain::overnight_index_convention
conventions_mapper::map_overnight_index(const overnightIndexType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping overnight index convention: "
                               << std::string(v.Id);

    refdata::domain::overnight_index_convention r;
    r.id = std::string(v.Id);
    r.fixing_calendar = std::string(v.FixingCalendar);
    r.day_count_fraction = normalize_day_counter(v.DayCounter);
    r.settlement_days = static_cast<int>(v.SettlementDays);
    set_audit(r);
    return r;
}

refdata::domain::fx_convention
conventions_mapper::map_fx(const fxType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping FX convention: " << std::string(v.Id);

    refdata::domain::fx_convention r;
    r.id = std::string(v.Id);
    r.spot_days = static_cast<int>(v.SpotDays);
    r.source_currency = to_string(v.SourceCurrency);
    r.target_currency = to_string(v.TargetCurrency);
    r.points_factor = v.PointsFactor;

    if (v.AdvanceCalendar)
        r.advance_calendar = std::string(*v.AdvanceCalendar);

    if (v.SpotRelative)
        r.spot_relative = parse_bool(*v.SpotRelative);

    if (v.EOM)
        r.end_of_month = parse_bool(*v.EOM);

    if (v.Convention)
        r.convention = normalize_bdc(*v.Convention);

    set_audit(r);
    return r;
}

refdata::domain::cds_convention
conventions_mapper::map_cds(const cdsConventionsType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping CDS convention: " << std::string(v.Id);

    refdata::domain::cds_convention r;
    r.id = std::string(v.Id);
    r.settlement_days = static_cast<int>(v.SettlementDays);
    r.calendar = std::string(v.Calendar);
    r.frequency = normalize_frequency(v.Frequency);
    r.payment_convention = normalize_bdc(v.PaymentConvention);
    r.rule = normalize_date_rule(v.Rule);
    r.day_count_fraction = normalize_day_counter(v.DayCounter);

    if (v.UpfrontSettlementDays)
        r.upfront_settlement_days = static_cast<int>(*v.UpfrontSettlementDays);

    r.settles_accrual = parse_bool(v.SettlesAccrual);
    r.pays_at_default_time = parse_bool(v.PaysAtDefaultTime);

    if (v.LastPeriodDayCounter)
        r.last_period_day_count_fraction = normalize_day_counter(*v.LastPeriodDayCounter);

    set_audit(r);
    return r;
}

// ---------------------------------------------------------------------------
// Top-level mapper
// ---------------------------------------------------------------------------

mapped_conventions conventions_mapper::map(const conventions& v) {
    BOOST_LOG_SEV(lg(), debug) << "Mapping ORE conventions. "
        << "Zero=" << v.Zero.size()
        << " Deposit=" << v.Deposit.size()
        << " Swap=" << v.Swap.size()
        << " OIS=" << v.OIS.size()
        << " FRA=" << v.FRA.size()
        << " IborIndex=" << v.IborIndex.size()
        << " OvernightIndex=" << v.OvernightIndex.size()
        << " FX=" << v.FX.size()
        << " CDS=" << v.CDS.size();

    mapped_conventions r;

    r.zero.reserve(v.Zero.size());
    std::ranges::transform(v.Zero, std::back_inserter(r.zero),
        [](const auto& x) { return map_zero(x); });

    r.deposit.reserve(v.Deposit.size());
    std::ranges::transform(v.Deposit, std::back_inserter(r.deposit),
        [](const auto& x) { return map_deposit(x); });

    r.swap.reserve(v.Swap.size());
    std::ranges::transform(v.Swap, std::back_inserter(r.swap),
        [](const auto& x) { return map_swap(x); });

    r.ois.reserve(v.OIS.size());
    std::ranges::transform(v.OIS, std::back_inserter(r.ois),
        [](const auto& x) { return map_ois(x); });

    r.fra.reserve(v.FRA.size());
    std::ranges::transform(v.FRA, std::back_inserter(r.fra),
        [](const auto& x) { return map_fra(x); });

    r.ibor_index.reserve(v.IborIndex.size());
    std::ranges::transform(v.IborIndex, std::back_inserter(r.ibor_index),
        [](const auto& x) { return map_ibor_index(x); });

    r.overnight_index.reserve(v.OvernightIndex.size());
    std::ranges::transform(v.OvernightIndex, std::back_inserter(r.overnight_index),
        [](const auto& x) { return map_overnight_index(x); });

    r.fx.reserve(v.FX.size());
    std::ranges::transform(v.FX, std::back_inserter(r.fx),
        [](const auto& x) { return map_fx(x); });

    r.cds.reserve(v.CDS.size());
    std::ranges::transform(v.CDS, std::back_inserter(r.cds),
        [](const auto& x) { return map_cds(x); });

    BOOST_LOG_SEV(lg(), debug) << "Finished mapping conventions. "
        << "Zero=" << r.zero.size()
        << " Deposit=" << r.deposit.size()
        << " Swap=" << r.swap.size()
        << " OIS=" << r.ois.size()
        << " FRA=" << r.fra.size()
        << " IborIndex=" << r.ibor_index.size()
        << " OvernightIndex=" << r.overnight_index.size()
        << " FX=" << r.fx.size()
        << " CDS=" << r.cds.size();

    return r;
}

}
