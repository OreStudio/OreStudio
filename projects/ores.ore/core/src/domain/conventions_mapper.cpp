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
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include <algorithm>
#include <cmath>
#include <map>
#include <stdexcept>

namespace ores::ore::domain {

using namespace ores::logging;

namespace {

constexpr std::string_view audit_modified_by = "ores";
constexpr std::string_view audit_reason_code = "system.external_data_import";
constexpr std::string_view audit_commentary = "Imported from ORE XML";

template <typename T>
void set_audit(T& r) {
    r.modified_by = std::string(audit_modified_by);
    r.change_reason_code = std::string(audit_reason_code);
    r.change_commentary = std::string(audit_commentary);
}

// ---------------------------------------------------------------------------
// Reverse parse helpers — canonical string → XSD enum
// ---------------------------------------------------------------------------

domain::dayCounter parse_day_counter(const std::string& s) {
    using dc = domain::dayCounter;
    if (s == "ACT/360")
        return dc::Actual_360;
    if (s == "ACT/360 (incl. last)")
        return dc::A360__Incl_Last_;
    if (s == "ACT/365.FIXED")
        return dc::A365F;
    if (s == "ACT/365L")
        return dc::ACT_365L;
    if (s == "ACT/365 (Canadian Bond)")
        return dc::Act_365__Canadian_Bond_;
    if (s == "T360")
        return dc::T360;
    if (s == "30/360")
        return dc::_30_360;
    if (s == "ACT/nACT")
        return dc::ACT_nACT;
    if (s == "30E/360")
        return dc::_30E_360;
    if (s == "30E/360.ISDA")
        return dc::_30E_360_ISDA;
    if (s == "30/360 (German)")
        return dc::_30_360_German;
    if (s == "30/360 (Italian)")
        return dc::_30_360_Italian;
    if (s == "ACT/ACT.ISDA")
        return dc::ActActISDA;
    if (s == "ACT/ACT.ISMA")
        return dc::ActActISMA;
    if (s == "ACT/ACT.AFB")
        return dc::ActActAFB;
    if (s == "1/1")
        return dc::_1_1;
    if (s == "BUS/252")
        return dc::BUS_252;
    if (s == "NL/365")
        return dc::NL_365;
    if (s == "ACT/365 (JGB)")
        return dc::Actual_365__JGB_;
    if (s == "Simple")
        return dc::Simple;
    if (s == "Year")
        return dc::Year;
    if (s == "ACT/364")
        return dc::A364;
    if (s == "Month")
        return dc::Month;
    throw std::runtime_error("parse_day_counter: unrecognised '" + s + "'");
}

domain::businessDayConvention parse_bdc(const std::string& s) {
    using bdc = domain::businessDayConvention;
    if (s == "Following")
        return bdc::Following;
    if (s == "ModifiedFollowing")
        return bdc::ModifiedFollowing;
    if (s == "Preceding")
        return bdc::Preceding;
    if (s == "ModifiedPreceding")
        return bdc::ModifiedPreceding;
    if (s == "HalfMonthModifiedFollowing")
        return bdc::HalfMonthModifiedFollowing;
    if (s == "Nearest")
        return bdc::NEAREST;
    if (s == "Unadjusted")
        return bdc::Unadjusted;
    throw std::runtime_error("parse_bdc: unrecognised '" + s + "'");
}

domain::frequencyType parse_frequency(const std::string& s) {
    using ft = domain::frequencyType;
    if (s == "Once")
        return ft::Once;
    if (s == "Annual")
        return ft::Annual;
    if (s == "Semiannual")
        return ft::Semiannual;
    if (s == "Quarterly")
        return ft::Quarterly;
    if (s == "Bimonthly")
        return ft::Bimonthly;
    if (s == "Monthly")
        return ft::Monthly;
    if (s == "Lunarmonth")
        return ft::Lunarmonth;
    if (s == "Weekly")
        return ft::Weekly;
    if (s == "Daily")
        return ft::Daily;
    throw std::runtime_error("parse_frequency: unrecognised '" + s + "'");
}

domain::compounding parse_compounding(const std::string& s) {
    using cm = domain::compounding;
    if (s == "Simple")
        return cm::Simple;
    if (s == "Compounded")
        return cm::Compounded;
    if (s == "Continuous")
        return cm::Continuous;
    if (s == "SimpleThenCompounded")
        return cm::SimpleThenCompounded;
    throw std::runtime_error("parse_compounding: unrecognised '" + s + "'");
}

domain::dateRule parse_date_rule(const std::string& s) {
    using dr = domain::dateRule;
    if (s == "Backward")
        return dr::Backward;
    if (s == "Forward")
        return dr::Forward;
    if (s == "Zero")
        return dr::Zero;
    if (s == "ThirdWednesday")
        return dr::ThirdWednesday;
    if (s == "Twentieth")
        return dr::Twentieth;
    if (s == "TwentiethIMM")
        return dr::TwentiethIMM;
    if (s == "OldCDS")
        return dr::OldCDS;
    if (s == "CDS")
        return dr::CDS;
    if (s == "CDS2015")
        return dr::CDS2015;
    if (s == "ThirdThursday")
        return dr::ThirdThursday;
    if (s == "ThirdFriday")
        return dr::ThirdFriday;
    if (s == "MondayAfterThirdFriday")
        return dr::MondayAfterThirdFriday;
    if (s == "TuesdayAfterThirdFriday")
        return dr::TuesdayAfterThirdFriday;
    if (s == "LastWednesday")
        return dr::LastWednesday;
    if (s == "EveryThursday")
        return dr::EveryThursday;
    throw std::runtime_error("parse_date_rule: unrecognised '" + s + "'");
}

domain::bool_ make_bool(bool v) {
    return v ? domain::bool_::True : domain::bool_::False;
}

domain::currencyCode parse_currency_code(const std::string& s) {
    using cc = domain::currencyCode;
    static const std::map<std::string, cc> kMap = {
        {"AED", cc::AED}, {"AFN", cc::AFN}, {"ALL", cc::ALL}, {"AMD", cc::AMD}, {"ANG", cc::ANG},
        {"AOA", cc::AOA}, {"ARS", cc::ARS}, {"AUD", cc::AUD}, {"AWG", cc::AWG}, {"AZN", cc::AZN},
        {"BAM", cc::BAM}, {"BBD", cc::BBD}, {"BDT", cc::BDT}, {"BGN", cc::BGN}, {"BHD", cc::BHD},
        {"BIF", cc::BIF}, {"BMD", cc::BMD}, {"BND", cc::BND}, {"BOB", cc::BOB}, {"BOV", cc::BOV},
        {"BRL", cc::BRL}, {"BSD", cc::BSD}, {"BTN", cc::BTN}, {"BWP", cc::BWP}, {"BYN", cc::BYN},
        {"BZD", cc::BZD}, {"CAD", cc::CAD}, {"CDF", cc::CDF}, {"CHE", cc::CHE}, {"CHF", cc::CHF},
        {"CHW", cc::CHW}, {"CLF", cc::CLF}, {"CLP", cc::CLP}, {"CNH", cc::CNH}, {"CNT", cc::CNT},
        {"CNY", cc::CNY}, {"COP", cc::COP}, {"COU", cc::COU}, {"CRC", cc::CRC}, {"CUC", cc::CUC},
        {"CUP", cc::CUP}, {"CVE", cc::CVE}, {"CYP", cc::CYP}, {"CZK", cc::CZK}, {"DJF", cc::DJF},
        {"DKK", cc::DKK}, {"DOP", cc::DOP}, {"DZD", cc::DZD}, {"EGP", cc::EGP}, {"ERN", cc::ERN},
        {"ETB", cc::ETB}, {"EUR", cc::EUR}, {"FJD", cc::FJD}, {"FKP", cc::FKP}, {"GBP", cc::GBP},
        {"GEL", cc::GEL}, {"GGP", cc::GGP}, {"GHS", cc::GHS}, {"GIP", cc::GIP}, {"GMD", cc::GMD},
        {"GNF", cc::GNF}, {"GTQ", cc::GTQ}, {"GYD", cc::GYD}, {"HKD", cc::HKD}, {"HNL", cc::HNL},
        {"HRK", cc::HRK}, {"HTG", cc::HTG}, {"HUF", cc::HUF}, {"IDR", cc::IDR}, {"ILS", cc::ILS},
        {"IMP", cc::IMP}, {"INR", cc::INR}, {"IQD", cc::IQD}, {"IRR", cc::IRR}, {"ISK", cc::ISK},
        {"JEP", cc::JEP}, {"JMD", cc::JMD}, {"JOD", cc::JOD}, {"JPY", cc::JPY}, {"KES", cc::KES},
        {"KGS", cc::KGS}, {"KHR", cc::KHR}, {"KID", cc::KID}, {"KMF", cc::KMF}, {"KPW", cc::KPW},
        {"KRW", cc::KRW}, {"KWD", cc::KWD}, {"KYD", cc::KYD}, {"KZT", cc::KZT}, {"LAK", cc::LAK},
        {"LBP", cc::LBP}, {"LKR", cc::LKR}, {"LRD", cc::LRD}, {"LSL", cc::LSL}, {"LTL", cc::LTL},
        {"LVL", cc::LVL}, {"LYD", cc::LYD}, {"MAD", cc::MAD}, {"MDL", cc::MDL}, {"MGA", cc::MGA},
        {"MKD", cc::MKD}, {"MMK", cc::MMK}, {"MNT", cc::MNT}, {"MOP", cc::MOP}, {"MRU", cc::MRU},
        {"MUR", cc::MUR}, {"MVR", cc::MVR}, {"MWK", cc::MWK}, {"MXN", cc::MXN}, {"MXV", cc::MXV},
        {"MYR", cc::MYR}, {"MZN", cc::MZN}, {"NAD", cc::NAD}, {"NGN", cc::NGN}, {"NIO", cc::NIO},
        {"NOK", cc::NOK}, {"NPR", cc::NPR}, {"NZD", cc::NZD}, {"OMR", cc::OMR}, {"PAB", cc::PAB},
        {"PEN", cc::PEN}, {"PGK", cc::PGK}, {"PHP", cc::PHP}, {"PKR", cc::PKR}, {"PLN", cc::PLN},
        {"PYG", cc::PYG}, {"QAR", cc::QAR}, {"RON", cc::RON}, {"RSD", cc::RSD}, {"RUB", cc::RUB},
        {"RWF", cc::RWF}, {"SAR", cc::SAR}, {"SBD", cc::SBD}, {"SCR", cc::SCR}, {"SDG", cc::SDG},
        {"SEK", cc::SEK}, {"SGD", cc::SGD}, {"SHP", cc::SHP}, {"SLL", cc::SLL}, {"SOS", cc::SOS},
        {"SRD", cc::SRD}, {"STN", cc::STN}, {"SVC", cc::SVC}, {"SYP", cc::SYP}, {"SZL", cc::SZL},
        {"THB", cc::THB}, {"TJS", cc::TJS}, {"TMT", cc::TMT}, {"TND", cc::TND}, {"TOP", cc::TOP},
        {"TRY", cc::TRY}, {"TTD", cc::TTD}, {"TWD", cc::TWD}, {"TZS", cc::TZS}, {"UAH", cc::UAH},
        {"UGX", cc::UGX}, {"USD", cc::USD}, {"USN", cc::USN}, {"UYI", cc::UYI}, {"UYU", cc::UYU},
        {"UYW", cc::UYW}, {"UZS", cc::UZS}, {"VES", cc::VES}, {"VND", cc::VND}, {"VUV", cc::VUV},
        {"WST", cc::WST}, {"XAF", cc::XAF}, {"XAG", cc::XAG}, {"XAU", cc::XAU}, {"XCD", cc::XCD},
        {"XOF", cc::XOF}, {"XPD", cc::XPD}, {"XPF", cc::XPF}, {"XPT", cc::XPT}, {"XSU", cc::XSU},
        {"XUA", cc::XUA}, {"YER", cc::YER}, {"ZAR", cc::ZAR}, {"ZMW", cc::ZMW}, {"ZWL", cc::ZWL},
        {"BTC", cc::BTC}, {"ETH", cc::ETH}, {"XBT", cc::XBT}, {"ETC", cc::ETC}, {"BCH", cc::BCH},
        {"XRP", cc::XRP}, {"LTC", cc::LTC}, {"ZUR", cc::ZUR}, {"ZUG", cc::ZUG},
    };
    const auto it = kMap.find(s);
    if (it == kMap.end())
        throw std::runtime_error("parse_currency_code: unrecognised '" + s + "'");
    return it->second;
}

// ---------------------------------------------------------------------------
// Individual type reverse mappers
// ---------------------------------------------------------------------------

zeroType reverse_zero(const refdata::domain::zero_convention& v) {
    zeroType r;
    static_cast<std::string&>(r.Id) = v.id;
    r.TenorBased = make_bool(v.tenor_based);
    r.DayCounter = parse_day_counter(v.day_count_fraction);
    if (v.compounding)
        r.Compounding = parse_compounding(*v.compounding);
    if (v.compounding_frequency)
        r.CompoundingFrequency = parse_frequency(*v.compounding_frequency);
    if (v.tenor_calendar) {
        zeroType_TenorCalendar_t tc;
        static_cast<std::string&>(tc) = *v.tenor_calendar;
        r.TenorCalendar = tc;
    }
    if (v.spot_lag)
        r.SpotLag = static_cast<int64_t>(*v.spot_lag);
    if (v.spot_calendar) {
        zeroType_SpotCalendar_t sc;
        static_cast<std::string&>(sc) = *v.spot_calendar;
        r.SpotCalendar = sc;
    }
    if (v.roll_convention)
        r.RollConvention = parse_bdc(*v.roll_convention);
    if (v.end_of_month)
        r.EOM = make_bool(*v.end_of_month);
    return r;
}

depositType reverse_deposit(const refdata::domain::deposit_convention& v) {
    depositType r;
    static_cast<std::string&>(r.Id) = v.id;
    r.IndexBased = make_bool(v.index_based);
    if (v.index) {
        depositType_Index_t idx;
        static_cast<std::string&>(idx) = *v.index;
        r.Index = idx;
    }
    if (v.calendar) {
        depositType_Calendar_t cal;
        static_cast<std::string&>(cal) = *v.calendar;
        r.Calendar = cal;
    }
    if (v.convention)
        r.Convention = parse_bdc(*v.convention);
    if (v.end_of_month)
        r.EOM = make_bool(*v.end_of_month);
    if (v.day_count_fraction)
        r.DayCounter = parse_day_counter(*v.day_count_fraction);
    if (v.settlement_days)
        r.SettlementDays = static_cast<uint64_t>(*v.settlement_days);
    return r;
}

swapType reverse_swap(const refdata::domain::swap_convention& v) {
    swapType r;
    static_cast<std::string&>(r.Id) = v.id;
    if (v.fixed_calendar) {
        swapType_FixedCalendar_t fc;
        static_cast<std::string&>(fc) = *v.fixed_calendar;
        r.FixedCalendar = fc;
    }
    r.FixedFrequency = parse_frequency(v.fixed_frequency);
    if (v.fixed_convention)
        r.FixedConvention = parse_bdc(*v.fixed_convention);
    r.FixedDayCounter = parse_day_counter(v.fixed_day_count_fraction);
    static_cast<std::string&>(r.Index) = v.index;
    if (v.float_frequency)
        r.FloatFrequency = parse_frequency(*v.float_frequency);
    if (v.sub_periods_coupon_type) {
        const auto& s = *v.sub_periods_coupon_type;
        if (s == "Compounding")
            r.SubPeriodsCouponType = subPeriodsCouponType::Compounding;
        else if (s == "Averaging")
            r.SubPeriodsCouponType = subPeriodsCouponType::Averaging;
        else
            throw std::runtime_error("reverse_swap: unknown sub_periods_coupon_type: " + s);
    }
    return r;
}

oisType reverse_ois(const refdata::domain::ois_convention& v) {
    oisType r;
    static_cast<std::string&>(r.Id) = v.id;
    r.SpotLag = static_cast<int64_t>(v.spot_lag);
    static_cast<std::string&>(r.Index) = v.index;
    r.FixedDayCounter = parse_day_counter(v.fixed_day_count_fraction);
    if (v.fixed_calendar) {
        oisType_FixedCalendar_t fc;
        static_cast<std::string&>(fc) = *v.fixed_calendar;
        r.FixedCalendar = fc;
    }
    if (v.payment_lag)
        r.PaymentLag = static_cast<int64_t>(*v.payment_lag);
    if (v.end_of_month)
        r.EOM = make_bool(*v.end_of_month);
    if (v.fixed_frequency)
        r.FixedFrequency = parse_frequency(*v.fixed_frequency);
    if (v.fixed_convention)
        r.FixedConvention = parse_bdc(*v.fixed_convention);
    if (v.fixed_payment_convention)
        r.FixedPaymentConvention = parse_bdc(*v.fixed_payment_convention);
    if (v.rule)
        r.Rule = parse_date_rule(*v.rule);
    if (v.payment_calendar) {
        oisType_PaymentCalendar_t pc;
        static_cast<std::string&>(pc) = *v.payment_calendar;
        r.PaymentCalendar = pc;
    }
    if (v.rate_cutoff)
        r.RateCutoff = static_cast<int64_t>(*v.rate_cutoff);
    return r;
}

fraType reverse_fra(const refdata::domain::fra_convention& v) {
    fraType r;
    static_cast<std::string&>(r.Id) = v.id;
    static_cast<std::string&>(r.Index) = v.index;
    return r;
}

iborIndexType reverse_ibor_index(const refdata::domain::ibor_index_convention& v) {
    iborIndexType r;
    static_cast<std::string&>(r.Id) = v.id;
    static_cast<std::string&>(r.FixingCalendar) = v.fixing_calendar;
    r.DayCounter = parse_day_counter(v.day_count_fraction);
    r.SettlementDays = static_cast<int64_t>(v.settlement_days);
    r.BusinessDayConvention = parse_bdc(v.business_day_convention);
    r.EndOfMonth = make_bool(v.end_of_month);
    return r;
}

overnightIndexType reverse_overnight_index(const refdata::domain::overnight_index_convention& v) {
    overnightIndexType r;
    static_cast<std::string&>(r.Id) = v.id;
    static_cast<std::string&>(r.FixingCalendar) = v.fixing_calendar;
    r.DayCounter = parse_day_counter(v.day_count_fraction);
    r.SettlementDays = static_cast<int64_t>(v.settlement_days);
    return r;
}

std::string fx_convention_id(const std::string& base_currency, const std::string& quote_currency) {
    return base_currency + "-" + quote_currency + "-FX-CONVENTIONS";
}

fxType reverse_fx(const domain::mapped_fx& v) {
    fxType r;
    static_cast<std::string&>(r.Id) = fx_convention_id(v.pair.base_currency, v.pair.quote_currency);
    r.SpotDays = static_cast<int64_t>(v.spot_days);
    r.SourceCurrency = parse_currency_code(v.pair.base_currency);
    r.TargetCurrency = parse_currency_code(v.pair.quote_currency);
    r.PointsFactor = v.convention.pip_factor != 0.0 ? 1.0 / v.convention.pip_factor : 0.0;
    if (v.convention.advance_calendar) {
        fxType_AdvanceCalendar_t ac;
        static_cast<std::string&>(ac) = *v.convention.advance_calendar;
        r.AdvanceCalendar = ac;
    }
    if (v.convention.spot_relative)
        r.SpotRelative = make_bool(*v.convention.spot_relative);
    if (v.convention.end_of_month)
        r.EOM = make_bool(*v.convention.end_of_month);
    if (v.convention.business_day_convention)
        r.Convention = parse_bdc(*v.convention.business_day_convention);
    return r;
}

cdsConventionsType reverse_cds(const refdata::domain::cds_convention& v) {
    cdsConventionsType r;
    static_cast<std::string&>(r.Id) = v.id;
    r.SettlementDays = static_cast<int64_t>(v.settlement_days);
    static_cast<std::string&>(r.Calendar) = v.calendar;
    r.Frequency = parse_frequency(v.frequency);
    r.PaymentConvention = parse_bdc(v.payment_convention);
    r.Rule = parse_date_rule(v.rule);
    r.DayCounter = parse_day_counter(v.day_count_fraction);
    if (v.upfront_settlement_days)
        r.UpfrontSettlementDays = static_cast<uint64_t>(*v.upfront_settlement_days);
    r.SettlesAccrual = make_bool(v.settles_accrual);
    r.PaysAtDefaultTime = make_bool(v.pays_at_default_time);
    if (v.last_period_day_count_fraction)
        r.LastPeriodDayCounter = parse_day_counter(*v.last_period_day_count_fraction);
    return r;
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
            return "Unadjusted";
        case bdc::_:
        default:
            throw std::runtime_error("Unknown business day convention enum value");
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
            throw std::runtime_error("Unknown day counter enum value");
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
            throw std::runtime_error("Unknown frequency type enum value");
    }
}

std::string conventions_mapper::normalize_compounding(domain::compounding v) {
    using cm = domain::compounding;
    switch (v) {
        case cm::Simple:
            return "Simple";
        case cm::Compounded:
            return "Compounded";
        case cm::Continuous:
            return "Continuous";
        case cm::SimpleThenCompounded:
            return "SimpleThenCompounded";
        case cm::_:
        default:
            throw std::runtime_error("Unknown compounding enum value");
    }
}

std::string conventions_mapper::normalize_date_rule(domain::dateRule v) {
    using dr = domain::dateRule;
    switch (v) {
        case dr::Backward:
            return "Backward";
        case dr::Forward:
            return "Forward";
        case dr::Zero:
            return "Zero";
        case dr::ThirdWednesday:
            return "ThirdWednesday";
        case dr::Twentieth:
            return "Twentieth";
        case dr::TwentiethIMM:
            return "TwentiethIMM";
        case dr::OldCDS:
            return "OldCDS";
        case dr::CDS:
            return "CDS";
        case dr::CDS2015:
            return "CDS2015";
        case dr::ThirdThursday:
            return "ThirdThursday";
        case dr::ThirdFriday:
            return "ThirdFriday";
        case dr::MondayAfterThirdFriday:
            return "MondayAfterThirdFriday";
        case dr::TuesdayAfterThirdFriday:
            return "TuesdayAfterThirdFriday";
        case dr::LastWednesday:
            return "LastWednesday";
        case dr::EveryThursday:
            return "EveryThursday";
        case dr::_:
        default:
            throw std::runtime_error("Unknown date rule enum value");
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

refdata::domain::zero_convention conventions_mapper::map_zero(const zeroType& v) {
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

refdata::domain::deposit_convention conventions_mapper::map_deposit(const depositType& v) {
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

refdata::domain::swap_convention conventions_mapper::map_swap(const swapType& v) {
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
        switch (*v.SubPeriodsCouponType) {
            case sp::Compounding:
                r.sub_periods_coupon_type = "Compounding";
                break;
            case sp::Averaging:
                r.sub_periods_coupon_type = "Averaging";
                break;
            default:
                throw std::runtime_error("Unknown sub-periods coupon type enum value");
        }
    }

    set_audit(r);
    return r;
}

refdata::domain::ois_convention conventions_mapper::map_ois(const oisType& v) {
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

refdata::domain::fra_convention conventions_mapper::map_fra(const fraType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping FRA convention: " << std::string(v.Id);

    refdata::domain::fra_convention r;
    r.id = std::string(v.Id);
    r.index = std::string(v.Index);
    set_audit(r);
    return r;
}

refdata::domain::ibor_index_convention conventions_mapper::map_ibor_index(const iborIndexType& v) {
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
    BOOST_LOG_SEV(lg(), trace) << "Mapping overnight index convention: " << std::string(v.Id);

    refdata::domain::overnight_index_convention r;
    r.id = std::string(v.Id);
    r.fixing_calendar = std::string(v.FixingCalendar);
    r.day_count_fraction = normalize_day_counter(v.DayCounter);
    r.settlement_days = static_cast<int>(v.SettlementDays);
    set_audit(r);
    return r;
}

mapped_fx conventions_mapper::map_fx(const fxType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping FX convention: " << std::string(v.Id);

    mapped_fx r;
    r.spot_days = static_cast<int>(v.SpotDays);

    auto& pair = r.pair;
    pair.base_currency = to_string(v.SourceCurrency);
    pair.quote_currency = to_string(v.TargetCurrency);
    pair.pair_code = pair.base_currency + "/" + pair.quote_currency;
    set_audit(pair);

    auto& convention = r.convention;
    convention.pair_code = pair.pair_code;
    convention.pip_factor = v.PointsFactor != 0.0 ? 1.0 / v.PointsFactor : 0.0;
    convention.tick_size = 1.0;
    convention.decimal_places =
        v.PointsFactor > 0.0 ? static_cast<int>(std::lround(std::log10(v.PointsFactor))) : 0;

    if (v.AdvanceCalendar)
        convention.advance_calendar = std::string(*v.AdvanceCalendar);

    if (v.SpotRelative)
        convention.spot_relative = parse_bool(*v.SpotRelative);

    if (v.EOM)
        convention.end_of_month = parse_bool(*v.EOM);

    if (v.Convention)
        convention.business_day_convention = normalize_bdc(*v.Convention);

    set_audit(convention);
    return r;
}

refdata::domain::cds_convention conventions_mapper::map_cds(const cdsConventionsType& v) {
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
    BOOST_LOG_SEV(lg(), debug) << "Mapping ORE conventions. " << "Zero=" << v.Zero.size()
                               << " Deposit=" << v.Deposit.size() << " Swap=" << v.Swap.size()
                               << " OIS=" << v.OIS.size() << " FRA=" << v.FRA.size()
                               << " IborIndex=" << v.IborIndex.size()
                               << " OvernightIndex=" << v.OvernightIndex.size()
                               << " FX=" << v.FX.size() << " CDS=" << v.CDS.size();

    mapped_conventions r;

    r.zero.reserve(v.Zero.size());
    std::ranges::transform(
        v.Zero, std::back_inserter(r.zero), [](const auto& x) { return map_zero(x); });

    r.deposit.reserve(v.Deposit.size());
    std::ranges::transform(
        v.Deposit, std::back_inserter(r.deposit), [](const auto& x) { return map_deposit(x); });

    r.swap.reserve(v.Swap.size());
    std::ranges::transform(
        v.Swap, std::back_inserter(r.swap), [](const auto& x) { return map_swap(x); });

    r.ois.reserve(v.OIS.size());
    std::ranges::transform(
        v.OIS, std::back_inserter(r.ois), [](const auto& x) { return map_ois(x); });

    r.fra.reserve(v.FRA.size());
    std::ranges::transform(
        v.FRA, std::back_inserter(r.fra), [](const auto& x) { return map_fra(x); });

    r.ibor_index.reserve(v.IborIndex.size());
    std::ranges::transform(v.IborIndex, std::back_inserter(r.ibor_index), [](const auto& x) {
        return map_ibor_index(x);
    });

    r.overnight_index.reserve(v.OvernightIndex.size());
    std::ranges::transform(v.OvernightIndex,
                           std::back_inserter(r.overnight_index),
                           [](const auto& x) { return map_overnight_index(x); });

    r.fx.reserve(v.FX.size());
    std::ranges::transform(v.FX, std::back_inserter(r.fx), [](const auto& x) { return map_fx(x); });

    r.cds.reserve(v.CDS.size());
    std::ranges::transform(
        v.CDS, std::back_inserter(r.cds), [](const auto& x) { return map_cds(x); });

    BOOST_LOG_SEV(lg(), debug) << "Finished mapping conventions. " << "Zero=" << r.zero.size()
                               << " Deposit=" << r.deposit.size() << " Swap=" << r.swap.size()
                               << " OIS=" << r.ois.size() << " FRA=" << r.fra.size()
                               << " IborIndex=" << r.ibor_index.size()
                               << " OvernightIndex=" << r.overnight_index.size()
                               << " FX=" << r.fx.size() << " CDS=" << r.cds.size();

    return r;
}

// ---------------------------------------------------------------------------
// Reverse — mapped_conventions → ORE XML conventions
// ---------------------------------------------------------------------------

conventions conventions_mapper::reverse(const mapped_conventions& v) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping conventions. " << "Zero=" << v.zero.size()
                               << " Deposit=" << v.deposit.size() << " Swap=" << v.swap.size()
                               << " OIS=" << v.ois.size() << " FRA=" << v.fra.size()
                               << " IborIndex=" << v.ibor_index.size()
                               << " OvernightIndex=" << v.overnight_index.size()
                               << " FX=" << v.fx.size() << " CDS=" << v.cds.size();

    conventions r;

    r.Zero.reserve(v.zero.size());
    for (const auto& x : v.zero)
        r.Zero.push_back(reverse_zero(x));

    r.Deposit.reserve(v.deposit.size());
    for (const auto& x : v.deposit)
        r.Deposit.push_back(reverse_deposit(x));

    r.Swap.reserve(v.swap.size());
    for (const auto& x : v.swap)
        r.Swap.push_back(reverse_swap(x));

    r.OIS.reserve(v.ois.size());
    for (const auto& x : v.ois)
        r.OIS.push_back(reverse_ois(x));

    r.FRA.reserve(v.fra.size());
    for (const auto& x : v.fra)
        r.FRA.push_back(reverse_fra(x));

    r.IborIndex.reserve(v.ibor_index.size());
    for (const auto& x : v.ibor_index)
        r.IborIndex.push_back(reverse_ibor_index(x));

    r.OvernightIndex.reserve(v.overnight_index.size());
    for (const auto& x : v.overnight_index)
        r.OvernightIndex.push_back(reverse_overnight_index(x));

    r.FX.reserve(v.fx.size());
    for (const auto& x : v.fx)
        r.FX.push_back(reverse_fx(x));

    r.CDS.reserve(v.cds.size());
    for (const auto& x : v.cds)
        r.CDS.push_back(reverse_cds(x));

    BOOST_LOG_SEV(lg(), debug) << "Finished reverse-mapping conventions.";
    return r;
}

}
