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
#include <sstream>
#include <stdexcept>

namespace ores::ore::domain {

using namespace ores::logging;

namespace {

domain::futureDateGenerationRule parse_date_generation_rule(const std::string& v) {
    using rule = domain::futureDateGenerationRule;
    if (v == "IMM")
        return rule::IMM;
    if (v == "FirstDayOfMonth")
        return rule::FirstDayOfMonth;
    if (v == "IMMAUD")
        return rule::IMMAUD;
    if (v == "SecondThursday")
        return rule::SecondThursday;
    if (v == "IMMNZD")
        return rule::IMMNZD;
    if (v == "IMMCAD")
        return rule::IMMCAD;
    if (v == "IMMEUR")
        return rule::IMMEUR;
    throw std::runtime_error("Unknown future date generation rule: " + v);
}

domain::overnightIndexFutureNettingType parse_future_netting_type(const std::string& v) {
    using netting = domain::overnightIndexFutureNettingType;
    if (v == "Averaging")
        return netting::Averaging;
    if (v == "Compounding")
        return netting::Compounding;
    throw std::runtime_error("Unknown overnight index future netting type: " + v);
}


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

domain::monthType parse_month(const std::string& s) {
    using m = domain::monthType;
    if (s == "Jan")
        return m::Jan;
    if (s == "Feb")
        return m::Feb;
    if (s == "Mar")
        return m::Mar;
    if (s == "Apr")
        return m::Apr;
    if (s == "May")
        return m::May;
    if (s == "Jun")
        return m::Jun;
    if (s == "Jul")
        return m::Jul;
    if (s == "Aug")
        return m::Aug;
    if (s == "Sep")
        return m::Sep;
    if (s == "Oct")
        return m::Oct;
    if (s == "Nov")
        return m::Nov;
    if (s == "Dec")
        return m::Dec;
    throw std::runtime_error("parse_month: unrecognised '" + s + "'");
}

domain::averagingDataPeriodType parse_averaging_period(const std::string& s) {
    using p = domain::averagingDataPeriodType;
    if (s == "PreviousMonth")
        return p::PreviousMonth;
    if (s == "ExpiryToExpiry")
        return p::ExpiryToExpiry;
    throw std::runtime_error("parse_averaging_period: unrecognised '" + s + "'");
}

domain::weekdayType parse_weekday(const std::string& s) {
    using w = domain::weekdayType;
    if (s == "Mon")
        return w::Mon;
    if (s == "Tue")
        return w::Tue;
    if (s == "Wed")
        return w::Wed;
    if (s == "Thu")
        return w::Thu;
    if (s == "Fri")
        return w::Fri;
    if (s == "Sat")
        return w::Sat;
    if (s == "Sun")
        return w::Sun;
    throw std::runtime_error("parse_weekday: unrecognised '" + s + "'");
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
        {"SEK", cc::SEK}, {"SGD", cc::SGD}, {"SHP", cc::SHP}, {"SLL", cc::SLL}, {"SKK", cc::SKK},
        {"SOS", cc::SOS}, {"SRD", cc::SRD}, {"SSP", cc::SSP}, {"STN", cc::STN}, {"SVC", cc::SVC},
        {"SYP", cc::SYP}, {"SZL", cc::SZL}, {"THB", cc::THB}, {"TJS", cc::TJS}, {"TMT", cc::TMT},
        {"TND", cc::TND}, {"TOP", cc::TOP}, {"TRY", cc::TRY}, {"TTD", cc::TTD}, {"TWD", cc::TWD},
        {"TZS", cc::TZS}, {"UAH", cc::UAH}, {"UGX", cc::UGX}, {"USD", cc::USD}, {"USN", cc::USN},
        {"UYI", cc::UYI}, {"UYU", cc::UYU}, {"UYW", cc::UYW}, {"UZS", cc::UZS}, {"VES", cc::VES},
        {"VND", cc::VND}, {"VUV", cc::VUV}, {"WST", cc::WST}, {"XAF", cc::XAF}, {"XAG", cc::XAG},
        {"XAU", cc::XAU}, {"XCD", cc::XCD}, {"XOF", cc::XOF}, {"XPD", cc::XPD}, {"XPF", cc::XPF},
        {"XPT", cc::XPT}, {"XSU", cc::XSU}, {"XUA", cc::XUA}, {"YER", cc::YER}, {"ZAR", cc::ZAR},
        {"ZMW", cc::ZMW}, {"ZWL", cc::ZWL}, {"BTC", cc::BTC}, {"ETH", cc::ETH}, {"XBT", cc::XBT},
        {"ETC", cc::ETC}, {"BCH", cc::BCH}, {"XRP", cc::XRP}, {"LTC", cc::LTC}, {"ZUR", cc::ZUR},
        {"ZUG", cc::ZUG},
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

commodityForwardType
reverse_commodity_forward(const refdata::domain::commodity_forward_convention& v) {
    commodityForwardType r;
    static_cast<std::string&>(r.Id) = v.id;
    if (v.spot_days)
        r.SpotDays = static_cast<int64_t>(*v.spot_days);
    if (v.points_factor)
        r.PointsFactor = *v.points_factor;
    if (v.advance_calendar) {
        commodityForwardType_AdvanceCalendar_t x;
        static_cast<std::string&>(x) = *v.advance_calendar;
        r.AdvanceCalendar = x;
    }
    if (v.spot_relative)
        r.SpotRelative = make_bool(*v.spot_relative);
    if (v.delivery_location) {
        commodityForwardType_DeliveryLocation_t x;
        static_cast<std::string&>(x) = *v.delivery_location;
        r.DeliveryLocation = x;
    }
    if (v.business_day_convention)
        r.BusinessDayConvention = parse_bdc(*v.business_day_convention);
    if (v.outright)
        r.Outright = make_bool(*v.outright);
    return r;
}

bondYield reverse_bond_yield(const refdata::domain::bond_yield_convention& v) {
    bondYield r;
    static_cast<std::string&>(r.Id) = v.id;
    static_cast<std::string&>(r.Compounding) = v.compounding;
    if (v.frequency)
        r.Frequency = parse_frequency(*v.frequency);
    if (v.price_type) {
        bondYield_PriceType_t x;
        static_cast<std::string&>(x) = *v.price_type;
        r.PriceType = x;
    }
    if (v.accuracy)
        r.Accuracy = static_cast<float>(*v.accuracy);
    if (v.max_evaluations)
        r.MaxEvaluations = static_cast<int64_t>(*v.max_evaluations);
    if (v.guess)
        r.Guess = static_cast<float>(*v.guess);
    return r;
}

commodityFutureType
reverse_commodity_future(const refdata::domain::commodity_future_convention& v) {
    commodityFutureType r;
    static_cast<std::string&>(r.Id) = v.id;
    r.ContractFrequency = parse_frequency(v.contract_frequency);
    static_cast<std::string&>(r.Calendar) = v.calendar;
    if (v.expiry_calendar) {
        commodityFutureType_ExpiryCalendar_t x;
        static_cast<std::string&>(x) = *v.expiry_calendar;
        r.ExpiryCalendar = x;
    }
    if (v.expiry_month_lag)
        r.ExpiryMonthLag = static_cast<int64_t>(*v.expiry_month_lag);
    if (v.one_contract_month)
        r.OneContractMonth = parse_month(*v.one_contract_month);
    if (v.offset_days)
        r.OffsetDays = static_cast<int64_t>(*v.offset_days);
    if (v.business_day_convention)
        r.BusinessDayConvention = parse_bdc(*v.business_day_convention);
    if (v.adjust_before_offset)
        r.AdjustBeforeOffset = make_bool(*v.adjust_before_offset);
    if (v.is_averaging)
        r.IsAveraging = make_bool(*v.is_averaging);
    if (v.valid_contract_months) {
        commodityFutureType_ValidContractMonths_t months;
        std::istringstream in(*v.valid_contract_months);
        std::string token;
        while (std::getline(in, token, ',')) {
            if (!token.empty())
                months.Month.push_back(parse_month(token));
        }
        r.ValidContractMonths = months;
    }
    if (v.anchor_nth_nth || v.anchor_day_of_month || v.anchor_calendar_days_before ||
        v.anchor_business_days_after || v.anchor_nth_weekday || v.anchor_last_weekday ||
        v.anchor_weekly_day_of_the_week) {
        commodityFutureType_AnchorDay_t a;
        if (v.anchor_nth_nth || v.anchor_nth_weekday) {
            nthWeekdayType nth;
            nth.Nth = v.anchor_nth_nth ? static_cast<int64_t>(*v.anchor_nth_nth) : 0;
            nth.Weekday = parse_weekday(v.anchor_nth_weekday.value_or("Mon"));
            a.NthWeekday = nth;
        }
        if (v.anchor_day_of_month)
            a.DayOfMonth = static_cast<int64_t>(*v.anchor_day_of_month);
        if (v.anchor_calendar_days_before)
            a.CalendarDaysBefore = static_cast<uint64_t>(*v.anchor_calendar_days_before);
        if (v.anchor_last_weekday)
            a.LastWeekday = parse_weekday(*v.anchor_last_weekday);
        if (v.anchor_weekly_day_of_the_week)
            a.WeeklyDayOfTheWeek = parse_weekday(*v.anchor_weekly_day_of_the_week);
        if (v.anchor_business_days_after)
            a.BusinessDaysAfter = static_cast<int64_t>(*v.anchor_business_days_after);
        r.AnchorDay = a;
    }
    if (v.option_expiry_month_lag)
        r.OptionExpiryMonthLag = static_cast<int64_t>(*v.option_expiry_month_lag);
    if (v.option_contract_frequency)
        r.OptionContractFrequency = parse_frequency(*v.option_contract_frequency);
    if (v.option_expiry_offset)
        r.OptionExpiryOffset = static_cast<uint64_t>(*v.option_expiry_offset);
    if (v.option_calendar_days_before)
        r.OptionCalendarDaysBefore = static_cast<uint64_t>(*v.option_calendar_days_before);
    if (v.option_min_business_days_before)
        r.OptionMinBusinessDaysBefore = static_cast<uint64_t>(*v.option_min_business_days_before);
    if (v.option_expiry_day)
        r.OptionExpiryDay = static_cast<int64_t>(*v.option_expiry_day);
    if (v.option_nth_nth || v.option_nth_weekday) {
        nthWeekdayType nth;
        nth.Nth = v.option_nth_nth ? static_cast<int64_t>(*v.option_nth_nth) : 0;
        nth.Weekday = parse_weekday(v.option_nth_weekday.value_or("Mon"));
        r.OptionNthWeekday = nth;
    }
    if (v.option_expiry_last_weekday_of_month)
        r.OptionExpiryLastWeekdayOfMonth = parse_weekday(*v.option_expiry_last_weekday_of_month);
    if (v.option_expiry_weekly_day_of_the_week)
        r.OptionExpiryWeeklyDayOfTheWeek = parse_weekday(*v.option_expiry_weekly_day_of_the_week);
    if (v.option_business_day_convention)
        r.OptionBusinessDayConvention = parse_bdc(*v.option_business_day_convention);
    if (v.hours_per_day)
        r.HoursPerDay = static_cast<uint64_t>(*v.hours_per_day);
    if (v.off_peak_index && v.peak_index && v.off_peak_hours && v.peak_calendar) {
        offPeakPowerIndexDataType off_peak;
        static_cast<std::string&>(off_peak.OffPeakIndex) = *v.off_peak_index;
        static_cast<std::string&>(off_peak.PeakIndex) = *v.peak_index;
        off_peak.OffPeakHours = *v.off_peak_hours;
        static_cast<std::string&>(off_peak.PeakCalendar) = *v.peak_calendar;
        r.OffPeakPowerIndexData = off_peak;
    }
    if (v.index_name) {
        commodityFutureType_IndexName_t x;
        static_cast<std::string&>(x) = *v.index_name;
        r.IndexName = x;
    }
    if (v.savings_time) {
        commodityFutureType_SavingsTime_t x;
        static_cast<std::string&>(x) = *v.savings_time;
        r.SavingsTime = x;
    }
    if (v.delivery_location) {
        commodityFutureType_DeliveryLocation_t x;
        static_cast<std::string&>(x) = *v.delivery_location;
        r.DeliveryLocation = x;
    }
    if (v.balance_of_the_month)
        r.BalanceOfTheMonth = make_bool(*v.balance_of_the_month);
    if (v.balance_of_the_month_pricing_calendar) {
        commodityFutureType_BalanceOfTheMonthPricingCalendar_t x;
        static_cast<std::string&>(x) = *v.balance_of_the_month_pricing_calendar;
        r.BalanceOfTheMonthPricingCalendar = x;
    }
    if (v.option_underlying_future_convention) {
        commodityFutureType_OptionUnderlyingFutureConvention_t x;
        static_cast<std::string&>(x) = *v.option_underlying_future_convention;
        r.OptionUnderlyingFutureConvention = x;
    }
    if (v.averaging_commodity_name && v.averaging_period && v.averaging_pricing_calendar &&
        v.averaging_conventions) {
        averagingDataType a;
        static_cast<std::string&>(a.CommodityName) = *v.averaging_commodity_name;
        a.Period = parse_averaging_period(*v.averaging_period);
        static_cast<std::string&>(a.PricingCalendar) = *v.averaging_pricing_calendar;
        static_cast<std::string&>(a.Conventions) = *v.averaging_conventions;
        if (v.averaging_use_business_days)
            a.UseBusinessDays = make_bool(*v.averaging_use_business_days);
        if (v.averaging_delivery_roll_days)
            a.DeliveryRollDays = static_cast<uint64_t>(*v.averaging_delivery_roll_days);
        if (v.averaging_future_month_offset)
            a.FutureMonthOffset = static_cast<uint64_t>(*v.averaging_future_month_offset);
        if (v.averaging_daily_expiry_offset)
            a.DailyExpiryOffset = static_cast<uint64_t>(*v.averaging_daily_expiry_offset);
        r.AveragingData = a;
    }
    if (v.prohibited_expiries) {
        prohibitedExpiriesType prohibited;
        std::istringstream in(*v.prohibited_expiries);
        std::string token;
        while (std::getline(in, token, ',')) {
            if (!token.empty()) {
                prohibitedExpiriesType_Dates_t_Date_t d;
                static_cast<std::string&>(d) = token;
                prohibited.Dates.Date.push_back(d);
            }
        }
        r.ProhibitedExpiries = prohibited;
    }
    if (v.future_continuation_mappings) {
        continuationMappingsType mappings;
        std::istringstream in(*v.future_continuation_mappings);
        std::string token;
        while (std::getline(in, token, ',')) {
            const auto colon = token.find(':');
            if (colon == std::string::npos)
                continue;
            continuationMappingType m;
            m.From = static_cast<uint64_t>(std::stoull(token.substr(0, colon)));
            m.To = static_cast<uint64_t>(std::stoull(token.substr(colon + 1)));
            mappings.ContinuationMapping.push_back(m);
        }
        r.FutureContinuationMappings = mappings;
    }
    if (v.option_continuation_mappings) {
        continuationMappingsType mappings;
        std::istringstream in(*v.option_continuation_mappings);
        std::string token;
        while (std::getline(in, token, ',')) {
            const auto colon = token.find(':');
            if (colon == std::string::npos)
                continue;
            continuationMappingType m;
            m.From = static_cast<uint64_t>(std::stoull(token.substr(0, colon)));
            m.To = static_cast<uint64_t>(std::stoull(token.substr(colon + 1)));
            mappings.ContinuationMapping.push_back(m);
        }
        r.OptionContinuationMappings = mappings;
    }
    return r;
}

cmsSpreadOptionType reverse_cms_spread_option(const refdata::domain::cms_spread_option_convention& v) {
    cmsSpreadOptionType r;
    static_cast<std::string&>(r.Id) = v.id;
    static_cast<std::string&>(r.ForwardStart) = v.forward_start;
    static_cast<std::string&>(r.SpotDays) = v.spot_days;
    static_cast<std::string&>(r.SwapTenor) = v.swap_tenor;
    r.FixingDays = static_cast<int64_t>(v.fixing_days);
    static_cast<std::string&>(r.Calendar) = v.calendar;
    r.DayCounter = parse_day_counter(v.day_count_fraction);
    r.RollConvention = parse_bdc(v.roll_convention);
    return r;
}

crossCurrencyFixFloatType
reverse_cross_currency_fix_float(const refdata::domain::cross_currency_fix_float_convention& v) {
    crossCurrencyFixFloatType r;
    static_cast<std::string&>(r.Id) = v.id;
    r.SettlementDays = static_cast<int64_t>(v.settlement_days);
    static_cast<std::string&>(r.SettlementCalendar) = v.settlement_calendar;
    r.SettlementConvention = parse_bdc(v.settlement_convention);
    r.FixedCurrency = parse_currency_code(v.fixed_currency);
    r.FixedFrequency = parse_frequency(v.fixed_frequency);
    r.FixedConvention = parse_bdc(v.fixed_convention);
    r.FixedDayCounter = parse_day_counter(v.fixed_day_count_fraction);
    static_cast<std::string&>(r.Index) = v.index;
    if (v.eom)
        r.EOM = make_bool(*v.eom);
    if (v.is_resettable)
        r.IsResettable = make_bool(*v.is_resettable);
    if (v.float_index_is_resettable)
        r.FloatIndexIsResettable = make_bool(*v.float_index_is_resettable);
    if (v.include_spread)
        r.IncludeSpread = make_bool(*v.include_spread);
    if (v.lookback) {
        crossCurrencyFixFloatType_Lookback_t x;
        static_cast<std::string&>(x) = *v.lookback;
        r.Lookback = x;
    }
    if (v.fixing_days)
        r.FixingDays = static_cast<int64_t>(*v.fixing_days);
    if (v.rate_cutoff)
        r.RateCutoff = static_cast<int64_t>(*v.rate_cutoff);
    if (v.is_averaged)
        r.IsAveraged = make_bool(*v.is_averaged);
    if (v.observation_shift)
        r.ObservationShift = make_bool(*v.observation_shift);
    return r;
}

inflationswapType reverse_inflation_swap(const refdata::domain::inflation_swap_convention& v) {
    inflationswapType r;
    static_cast<std::string&>(r.Id) = v.id;
    static_cast<std::string&>(r.FixCalendar) = v.fix_calendar;
    r.FixConvention = parse_bdc(v.fix_convention);
    r.DayCounter = parse_day_counter(v.day_count_fraction);
    static_cast<std::string&>(r.Index) = v.index;
    r.Interpolated = make_bool(v.interpolated);
    static_cast<std::string&>(r.ObservationLag) = v.observation_lag;
    r.AdjustInflationObservationDates = make_bool(v.adjust_inflation_observation_dates);
    static_cast<std::string&>(r.InflationCalendar) = v.inflation_calendar;
    r.InflationConvention = parse_bdc(v.inflation_convention);
    if (v.publication_roll) {
        const auto& s = *v.publication_roll;
        if (s == "None")
            r.PublicationRoll = publicationRoll::None;
        else if (s == "OnPublicationDate")
            r.PublicationRoll = publicationRoll::OnPublicationDate;
        else if (s == "AfterPublicationDate")
            r.PublicationRoll = publicationRoll::AfterPublicationDate;
        else
            throw std::runtime_error("reverse_inflation_swap: unknown publication_roll: " + s);
    }
    if (v.start_delay) {
        inflationswapType_StartDelay_t x;
        static_cast<std::string&>(x) = *v.start_delay;
        r.StartDelay = x;
    }
    if (v.start_delay_convention)
        r.StartDelayConvention = parse_bdc(*v.start_delay_convention);
    return r;
}

bmaBasisSwapType reverse_bma_basis_swap(const refdata::domain::bma_basis_swap_convention& v) {
    bmaBasisSwapType r;
    static_cast<std::string&>(r.Id) = v.id;
    static_cast<std::string&>(r.Index) = v.index;
    static_cast<std::string&>(r.BMAIndex) = v.bma_index;
    if (v.bma_payment_calendar) {
        bmaBasisSwapType_BMAPaymentCalendar_t x;
        static_cast<std::string&>(x) = *v.bma_payment_calendar;
        r.BMAPaymentCalendar = x;
    }
    if (v.bma_payment_convention)
        r.BMAPaymentConvention = parse_bdc(*v.bma_payment_convention);
    if (v.bma_payment_lag)
        r.BMAPaymentLag = static_cast<int64_t>(*v.bma_payment_lag);
    if (v.index_payment_calendar) {
        bmaBasisSwapType_IndexPaymentCalendar_t x;
        static_cast<std::string&>(x) = *v.index_payment_calendar;
        r.IndexPaymentCalendar = x;
    }
    if (v.index_payment_convention)
        r.IndexPaymentConvention = parse_bdc(*v.index_payment_convention);
    if (v.index_payment_lag)
        r.IndexPaymentLag = static_cast<int64_t>(*v.index_payment_lag);
    if (v.index_settlement_days)
        r.IndexSettlementDays = static_cast<int64_t>(*v.index_settlement_days);
    if (v.index_payment_period)
        r.IndexPaymentPeriod = *v.index_payment_period;
    if (v.overnight_lockout_days)
        r.OvernightLockoutDays = static_cast<int64_t>(*v.overnight_lockout_days);
    return r;
}

zeroInflationIndexType
reverse_zero_inflation_index(const refdata::domain::zero_inflation_index_convention& v) {
    zeroInflationIndexType r;
    static_cast<std::string&>(r.Id) = v.id;
    static_cast<std::string&>(r.RegionName) = v.region_name;
    static_cast<std::string&>(r.RegionCode) = v.region_code;
    r.Revised = make_bool(v.revised);
    r.Frequency = parse_frequency(v.frequency);
    static_cast<std::string&>(r.AvailabilityLag) = v.availability_lag;
    r.Currency = parse_currency_code(v.currency);
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

tenorBasisTwoSwapType reverse_tenor_basis_two_swap(
    const refdata::domain::tenor_basis_two_swap_convention& v) {
    tenorBasisTwoSwapType r;
    static_cast<std::string&>(r.Id) = v.id;
    static_cast<std::string&>(r.Calendar) = v.calendar;
    r.LongFixedFrequency = parse_frequency(v.long_fixed_frequency);
    r.LongFixedConvention = parse_bdc(v.long_fixed_convention);
    r.LongFixedDayCounter = parse_day_counter(v.long_fixed_day_count_fraction);
    static_cast<std::string&>(r.LongIndex) = v.long_index;
    r.ShortFixedFrequency = parse_frequency(v.short_fixed_frequency);
    r.ShortFixedConvention = parse_bdc(v.short_fixed_convention);
    r.ShortFixedDayCounter = parse_day_counter(v.short_fixed_day_count_fraction);
    static_cast<std::string&>(r.ShortIndex) = v.short_index;
    if (v.long_minus_short)
        r.LongMinusShort = make_bool(*v.long_minus_short);
    return r;
}

tenorBasisSwapType reverse_tenor_basis_swap(const refdata::domain::tenor_basis_swap_convention& v) {
    tenorBasisSwapType r;
    static_cast<std::string&>(r.Id) = v.id;
    if (v.pay_index) {
        tenorBasisSwapType_PayIndex_t x;
        static_cast<std::string&>(x) = *v.pay_index;
        r.PayIndex = x;
    }
    if (v.pay_frequency)
        r.PayFrequency = *v.pay_frequency;
    if (v.receive_index) {
        tenorBasisSwapType_ReceiveIndex_t x;
        static_cast<std::string&>(x) = *v.receive_index;
        r.ReceiveIndex = x;
    }
    if (v.receive_frequency)
        r.ReceiveFrequency = *v.receive_frequency;
    if (v.spread_on_rec)
        r.SpreadOnRec = make_bool(*v.spread_on_rec);
    if (v.include_spread)
        r.IncludeSpread = make_bool(*v.include_spread);
    if (v.sub_periods_coupon_type) {
        const auto& s = *v.sub_periods_coupon_type;
        if (s == "Compounding")
            r.SubPeriodsCouponType = subPeriodsCouponType::Compounding;
        else if (s == "Averaging")
            r.SubPeriodsCouponType = subPeriodsCouponType::Averaging;
        else
            throw std::runtime_error("reverse_tenor_basis_swap: unknown sub_periods_coupon_type: " +
                                     s);
    }
    if (v.pay_is_averaged)
        r.PayIsAveraged = make_bool(*v.pay_is_averaged);
    if (v.rec_is_averaged)
        r.RecIsAveraged = make_bool(*v.rec_is_averaged);
    if (v.long_index) {
        tenorBasisSwapType_LongIndex_t x;
        static_cast<std::string&>(x) = *v.long_index;
        r.LongIndex = x;
    }
    if (v.long_pay_tenor)
        r.LongPayTenor = *v.long_pay_tenor;
    if (v.short_index) {
        tenorBasisSwapType_ShortIndex_t x;
        static_cast<std::string&>(x) = *v.short_index;
        r.ShortIndex = x;
    }
    if (v.short_pay_tenor)
        r.ShortPayTenor = *v.short_pay_tenor;
    if (v.spread_on_short)
        r.SpreadOnShort = make_bool(*v.spread_on_short);
    return r;
}

crossCurrencyBasisType reverse_cross_currency_basis(
    const refdata::domain::cross_currency_basis_convention& v) {
    crossCurrencyBasisType r;
    static_cast<std::string&>(r.Id) = v.id;
    r.SettlementDays = static_cast<int64_t>(v.settlement_days);
    if (v.settlement_calendar) {
        crossCurrencyBasisType_SettlementCalendar_t value;
        static_cast<std::string&>(value) = *v.settlement_calendar;
        r.SettlementCalendar = value;
    }
    r.RollConvention = parse_bdc(v.roll_convention);
    static_cast<std::string&>(r.FlatIndex) = v.flat_index;
    static_cast<std::string&>(r.SpreadIndex) = v.spread_index;
    if (v.eom)
        r.EOM = make_bool(*v.eom);
    if (v.is_resettable)
        r.IsResettable = make_bool(*v.is_resettable);
    if (v.flat_index_is_resettable)
        r.FlatIndexIsResettable = make_bool(*v.flat_index_is_resettable);
    if (v.flat_tenor) {
        crossCurrencyBasisType_FlatTenor_t value;
        static_cast<std::string&>(value) = *v.flat_tenor;
        r.FlatTenor = value;
    }
    if (v.spread_tenor) {
        crossCurrencyBasisType_SpreadTenor_t value;
        static_cast<std::string&>(value) = *v.spread_tenor;
        r.SpreadTenor = value;
    }
    if (v.spread_payment_lag)
        r.SpreadPaymentLag = static_cast<int64_t>(*v.spread_payment_lag);
    if (v.flat_payment_lag)
        r.FlatPaymentLag = static_cast<int64_t>(*v.flat_payment_lag);
    if (v.spread_include_spread)
        r.SpreadIncludeSpread = make_bool(*v.spread_include_spread);
    if (v.spread_lookback) {
        crossCurrencyBasisType_SpreadLookback_t value;
        static_cast<std::string&>(value) = *v.spread_lookback;
        r.SpreadLookback = value;
    }
    if (v.spread_fixing_days)
        r.SpreadFixingDays = static_cast<int64_t>(*v.spread_fixing_days);
    if (v.spread_rate_cutoff)
        r.SpreadRateCutoff = static_cast<int64_t>(*v.spread_rate_cutoff);
    if (v.spread_is_averaged)
        r.SpreadIsAveraged = make_bool(*v.spread_is_averaged);
    if (v.spread_observation_shift)
        r.SpreadObservationShift = make_bool(*v.spread_observation_shift);
    if (v.flat_include_spread)
        r.FlatIncludeSpread = make_bool(*v.flat_include_spread);
    if (v.flat_lookback) {
        crossCurrencyBasisType_FlatLookback_t value;
        static_cast<std::string&>(value) = *v.flat_lookback;
        r.FlatLookback = value;
    }
    if (v.flat_fixing_days)
        r.FlatFixingDays = static_cast<int64_t>(*v.flat_fixing_days);
    if (v.flat_rate_cutoff)
        r.FlatRateCutoff = static_cast<int64_t>(*v.flat_rate_cutoff);
    if (v.flat_is_averaged)
        r.FlatIsAveraged = make_bool(*v.flat_is_averaged);
    if (v.flat_observation_shift)
        r.FlatObservationShift = make_bool(*v.flat_observation_shift);
    return r;
}

averageOISType reverse_average_ois(const refdata::domain::average_ois_convention& v) {
    averageOISType r;
    static_cast<std::string&>(r.Id) = v.id;
    r.SpotLag = static_cast<int64_t>(v.spot_lag);
    static_cast<std::string&>(r.FixedTenor) = v.fixed_tenor;
    r.FixedDayCounter = parse_day_counter(v.fixed_day_count_fraction);
    if (v.fixed_calendar) {
        averageOISType_FixedCalendar_t calendar;
        static_cast<std::string&>(calendar) = *v.fixed_calendar;
        r.FixedCalendar = calendar;
    }
    if (v.fixed_convention)
        r.FixedConvention = parse_bdc(*v.fixed_convention);
    if (v.fixed_payment_convention)
        r.FixedPaymentConvention = parse_bdc(*v.fixed_payment_convention);
    if (v.fixed_frequency)
        r.FixedFrequency = parse_frequency(*v.fixed_frequency);
    static_cast<std::string&>(r.Index) = v.index;
    static_cast<std::string&>(r.OnTenor) = v.on_tenor;
    static_cast<std::string&>(r.RateCutoff) = v.rate_cutoff;
    return r;
}

fxOption reverse_fx_option(const refdata::domain::fx_option_convention& v) {
    fxOption r;
    static_cast<std::string&>(r.Id) = v.id;
    if (v.fx_convention_id) {
        fxOption_FXConventionID_t conventions;
        static_cast<std::string&>(conventions) = *v.fx_convention_id;
        r.FXConventionID = conventions;
    }
    static_cast<std::string&>(r.AtmType) = v.atm_type;
    static_cast<std::string&>(r.DeltaType) = v.delta_type;
    if (v.switch_tenor) {
        fxOption_SwitchTenor_t tenor;
        static_cast<std::string&>(tenor) = *v.switch_tenor;
        r.SwitchTenor = tenor;
    }
    if (v.long_term_atm_type) {
        fxOption_LongTermAtmType_t atm;
        static_cast<std::string&>(atm) = *v.long_term_atm_type;
        r.LongTermAtmType = atm;
    }
    if (v.long_term_delta_type) {
        fxOption_LongTermDeltaType_t delta;
        static_cast<std::string&>(delta) = *v.long_term_delta_type;
        r.LongTermDeltaType = delta;
    }
    if (v.risk_reversal_in_favor_of) {
        fxOption_RiskReversalInFavorOf_t favour;
        static_cast<std::string&>(favour) = *v.risk_reversal_in_favor_of;
        r.RiskReversalInFavorOf = favour;
    }
    if (v.butterfly_style) {
        fxOption_ButterflyStyle_t style;
        static_cast<std::string&>(style) = *v.butterfly_style;
        r.ButterflyStyle = style;
    }
    return r;
}

futureType reverse_future(const refdata::domain::future_convention& v) {
    futureType r;
    static_cast<std::string&>(r.Id) = v.id;
    static_cast<std::string&>(r.Index) = v.index;
    if (v.date_generation_rule)
        r.DateGenerationRule = parse_date_generation_rule(*v.date_generation_rule);
    if (v.netting_type)
        r.OvernightIndexFutureNettingType = parse_future_netting_type(*v.netting_type);
    if (v.calendar) {
        futureType_Calendar_t calendar;
        static_cast<std::string&>(calendar) = *v.calendar;
        r.Calendar = calendar;
    }
    if (v.overnight_index_tenor) {
        futureType_OvernightIndexTenor_t tenor;
        static_cast<std::string&>(tenor) = *v.overnight_index_tenor;
        r.OvernightIndexTenor = tenor;
    }
    return r;
}

swapIndexType reverse_swap_index(const refdata::domain::swap_index_convention& v) {
    swapIndexType r;
    static_cast<std::string&>(r.Id) = v.id;
    static_cast<std::string&>(r.Conventions) = v.conventions;
    if (v.fixing_calendar) {
        swapIndexType_FixingCalendar_t calendar;
        static_cast<std::string&>(calendar) = *v.fixing_calendar;
        r.FixingCalendar = calendar;
    }
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
    if (!v.advance_calendars.empty()) {
        std::string joined;
        for (const auto& code : v.advance_calendars) {
            if (!joined.empty())
                joined += ",";
            joined += code;
        }
        fxType_AdvanceCalendar_t ac;
        static_cast<std::string&>(ac) = joined;
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
    cdsConventionsType_Calendar_t calendar;
    static_cast<std::string&>(calendar) = v.calendar;
    r.Calendar = calendar;
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

refdata::domain::swap_index_convention
conventions_mapper::map_swap_index(const swapIndexType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping swap index convention: " << std::string(v.Id);

    refdata::domain::swap_index_convention r;
    r.id = std::string(v.Id);
    r.conventions = std::string(v.Conventions);
    if (v.FixingCalendar)
        r.fixing_calendar = std::string(*v.FixingCalendar);
    set_audit(r);
    return r;
}

refdata::domain::future_convention conventions_mapper::map_future(const futureType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping future convention: " << std::string(v.Id);

    refdata::domain::future_convention r;
    r.id = std::string(v.Id);
    r.index = std::string(v.Index);
    if (v.DateGenerationRule)
        r.date_generation_rule = to_string(*v.DateGenerationRule);
    if (v.OvernightIndexFutureNettingType)
        r.netting_type = to_string(*v.OvernightIndexFutureNettingType);
    if (v.Calendar)
        r.calendar = std::string(*v.Calendar);
    if (v.OvernightIndexTenor)
        r.overnight_index_tenor = std::string(*v.OvernightIndexTenor);
    set_audit(r);
    return r;
}

refdata::domain::tenor_basis_two_swap_convention
conventions_mapper::map_tenor_basis_two_swap(const tenorBasisTwoSwapType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping two-tenor basis swap convention: " << std::string(v.Id);

    refdata::domain::tenor_basis_two_swap_convention r;
    r.id = std::string(v.Id);
    r.calendar = std::string(v.Calendar);
    r.long_fixed_frequency = normalize_frequency(v.LongFixedFrequency);
    r.long_fixed_convention = normalize_bdc(v.LongFixedConvention);
    r.long_fixed_day_count_fraction = normalize_day_counter(v.LongFixedDayCounter);
    r.long_index = std::string(v.LongIndex);
    r.short_fixed_frequency = normalize_frequency(v.ShortFixedFrequency);
    r.short_fixed_convention = normalize_bdc(v.ShortFixedConvention);
    r.short_fixed_day_count_fraction = normalize_day_counter(v.ShortFixedDayCounter);
    r.short_index = std::string(v.ShortIndex);
    if (v.LongMinusShort)
        r.long_minus_short = parse_bool(*v.LongMinusShort);
    set_audit(r);
    return r;
}

refdata::domain::tenor_basis_swap_convention
conventions_mapper::map_tenor_basis_swap(const tenorBasisSwapType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping tenor basis swap convention: " << std::string(v.Id);

    refdata::domain::tenor_basis_swap_convention r;
    r.id = std::string(v.Id);

    if (v.PayIndex)
        r.pay_index = std::string(*v.PayIndex);

    if (v.PayFrequency)
        r.pay_frequency = std::string(*v.PayFrequency);

    if (v.ReceiveIndex)
        r.receive_index = std::string(*v.ReceiveIndex);

    if (v.ReceiveFrequency)
        r.receive_frequency = std::string(*v.ReceiveFrequency);

    if (v.SpreadOnRec)
        r.spread_on_rec = parse_bool(*v.SpreadOnRec);

    if (v.IncludeSpread)
        r.include_spread = parse_bool(*v.IncludeSpread);

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

    if (v.PayIsAveraged)
        r.pay_is_averaged = parse_bool(*v.PayIsAveraged);

    if (v.RecIsAveraged)
        r.rec_is_averaged = parse_bool(*v.RecIsAveraged);

    if (v.LongIndex)
        r.long_index = std::string(*v.LongIndex);

    if (v.LongPayTenor)
        r.long_pay_tenor = std::string(*v.LongPayTenor);

    if (v.ShortIndex)
        r.short_index = std::string(*v.ShortIndex);

    if (v.ShortPayTenor)
        r.short_pay_tenor = std::string(*v.ShortPayTenor);

    if (v.SpreadOnShort)
        r.spread_on_short = parse_bool(*v.SpreadOnShort);

    set_audit(r);
    return r;
}

refdata::domain::cross_currency_basis_convention
conventions_mapper::map_cross_currency_basis(const crossCurrencyBasisType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping cross-currency basis convention: " << std::string(v.Id);

    refdata::domain::cross_currency_basis_convention r;
    r.id = std::string(v.Id);
    r.settlement_days = static_cast<int>(v.SettlementDays);
    if (v.SettlementCalendar)
        r.settlement_calendar = std::string(*v.SettlementCalendar);
    r.roll_convention = normalize_bdc(v.RollConvention);
    r.flat_index = std::string(v.FlatIndex);
    r.spread_index = std::string(v.SpreadIndex);
    if (v.EOM)
        r.eom = parse_bool(*v.EOM);
    if (v.IsResettable)
        r.is_resettable = parse_bool(*v.IsResettable);
    if (v.FlatIndexIsResettable)
        r.flat_index_is_resettable = parse_bool(*v.FlatIndexIsResettable);
    if (v.FlatTenor)
        r.flat_tenor = std::string(*v.FlatTenor);
    if (v.SpreadTenor)
        r.spread_tenor = std::string(*v.SpreadTenor);
    if (v.SpreadPaymentLag)
        r.spread_payment_lag = static_cast<int>(*v.SpreadPaymentLag);
    if (v.FlatPaymentLag)
        r.flat_payment_lag = static_cast<int>(*v.FlatPaymentLag);
    if (v.SpreadIncludeSpread)
        r.spread_include_spread = parse_bool(*v.SpreadIncludeSpread);
    if (v.SpreadLookback)
        r.spread_lookback = std::string(*v.SpreadLookback);
    if (v.SpreadFixingDays)
        r.spread_fixing_days = static_cast<int>(*v.SpreadFixingDays);
    if (v.SpreadRateCutoff)
        r.spread_rate_cutoff = static_cast<int>(*v.SpreadRateCutoff);
    if (v.SpreadIsAveraged)
        r.spread_is_averaged = parse_bool(*v.SpreadIsAveraged);
    if (v.SpreadObservationShift)
        r.spread_observation_shift = parse_bool(*v.SpreadObservationShift);
    if (v.FlatIncludeSpread)
        r.flat_include_spread = parse_bool(*v.FlatIncludeSpread);
    if (v.FlatLookback)
        r.flat_lookback = std::string(*v.FlatLookback);
    if (v.FlatFixingDays)
        r.flat_fixing_days = static_cast<int>(*v.FlatFixingDays);
    if (v.FlatRateCutoff)
        r.flat_rate_cutoff = static_cast<int>(*v.FlatRateCutoff);
    if (v.FlatIsAveraged)
        r.flat_is_averaged = parse_bool(*v.FlatIsAveraged);
    if (v.FlatObservationShift)
        r.flat_observation_shift = parse_bool(*v.FlatObservationShift);
    set_audit(r);
    return r;
}

refdata::domain::average_ois_convention
conventions_mapper::map_average_ois(const averageOISType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping averaging OIS convention: " << std::string(v.Id);

    refdata::domain::average_ois_convention r;
    r.id = std::string(v.Id);
    r.spot_lag = static_cast<int>(v.SpotLag);
    r.fixed_tenor = std::string(v.FixedTenor);
    r.fixed_day_count_fraction = normalize_day_counter(v.FixedDayCounter);
    // ORE makes the first three of these required, so the binding holds them by
    // value and every document in the corpus carries them. Only the frequency
    // is optional, and the corpus never sets it.
    r.fixed_calendar = std::string(v.FixedCalendar);
    r.fixed_convention = normalize_bdc(v.FixedConvention);
    r.fixed_payment_convention = normalize_bdc(v.FixedPaymentConvention);
    if (v.FixedFrequency)
        r.fixed_frequency = normalize_frequency(*v.FixedFrequency);
    r.index = std::string(v.Index);
    r.on_tenor = std::string(v.OnTenor);
    r.rate_cutoff = std::string(v.RateCutoff);
    set_audit(r);
    return r;
}

refdata::domain::fx_option_convention conventions_mapper::map_fx_option(const fxOption& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping FX option convention: " << std::string(v.Id);

    refdata::domain::fx_option_convention r;
    r.id = std::string(v.Id);
    if (v.FXConventionID)
        r.fx_convention_id = std::string(*v.FXConventionID);
    r.atm_type = std::string(v.AtmType);
    r.delta_type = std::string(v.DeltaType);
    if (v.SwitchTenor)
        r.switch_tenor = std::string(*v.SwitchTenor);
    if (v.LongTermAtmType)
        r.long_term_atm_type = std::string(*v.LongTermAtmType);
    if (v.LongTermDeltaType)
        r.long_term_delta_type = std::string(*v.LongTermDeltaType);
    if (v.RiskReversalInFavorOf)
        r.risk_reversal_in_favor_of = std::string(*v.RiskReversalInFavorOf);
    if (v.ButterflyStyle)
        r.butterfly_style = std::string(*v.ButterflyStyle);
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

refdata::domain::commodity_forward_convention
conventions_mapper::map_commodity_forward(const commodityForwardType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping commodity forward convention: " << std::string(v.Id);

    refdata::domain::commodity_forward_convention r;
    r.id = std::string(v.Id);

    if (v.SpotDays)
        r.spot_days = static_cast<int>(*v.SpotDays);

    if (v.PointsFactor)
        r.points_factor = *v.PointsFactor;

    if (v.AdvanceCalendar)
        r.advance_calendar = std::string(*v.AdvanceCalendar);

    if (v.SpotRelative)
        r.spot_relative = parse_bool(*v.SpotRelative);

    if (v.DeliveryLocation)
        r.delivery_location = std::string(*v.DeliveryLocation);

    if (v.BusinessDayConvention)
        r.business_day_convention = normalize_bdc(*v.BusinessDayConvention);

    if (v.Outright)
        r.outright = parse_bool(*v.Outright);

    set_audit(r);
    return r;
}

refdata::domain::bond_yield_convention
conventions_mapper::map_bond_yield(const bondYield& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping bond yield convention: " << std::string(v.Id);

    refdata::domain::bond_yield_convention r;
    r.id = std::string(v.Id);
    r.compounding = std::string(v.Compounding);

    if (v.Frequency)
        r.frequency = normalize_frequency(*v.Frequency);

    if (v.PriceType)
        r.price_type = std::string(*v.PriceType);

    if (v.Accuracy)
        r.accuracy = static_cast<double>(*v.Accuracy);

    if (v.MaxEvaluations)
        r.max_evaluations = static_cast<int>(*v.MaxEvaluations);

    if (v.Guess)
        r.guess = static_cast<double>(*v.Guess);

    set_audit(r);
    return r;
}

refdata::domain::commodity_future_convention
conventions_mapper::map_commodity_future(const commodityFutureType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping commodity future convention: " << std::string(v.Id);

    refdata::domain::commodity_future_convention r;
    r.id = std::string(v.Id);
    r.contract_frequency = normalize_frequency(v.ContractFrequency);
    r.calendar = std::string(v.Calendar);

    if (v.ExpiryCalendar)
        r.expiry_calendar = std::string(*v.ExpiryCalendar);

    if (v.ExpiryMonthLag)
        r.expiry_month_lag = static_cast<int>(*v.ExpiryMonthLag);

    if (v.OneContractMonth)
        r.one_contract_month = to_string(*v.OneContractMonth);

    if (v.OffsetDays)
        r.offset_days = static_cast<int>(*v.OffsetDays);

    if (v.BusinessDayConvention)
        r.business_day_convention = normalize_bdc(*v.BusinessDayConvention);

    if (v.AdjustBeforeOffset)
        r.adjust_before_offset = parse_bool(*v.AdjustBeforeOffset);

    if (v.IsAveraging)
        r.is_averaging = parse_bool(*v.IsAveraging);

    if (v.ValidContractMonths) {
        std::string months;
        for (const auto& month : v.ValidContractMonths->Month) {
            if (!months.empty())
                months += ",";
            months += to_string(month);
        }
        r.valid_contract_months = months;
    }

    if (v.AnchorDay) {
        const auto& a = *v.AnchorDay;
        if (a.NthWeekday) {
            r.anchor_nth_nth = static_cast<int>(a.NthWeekday->Nth);
            r.anchor_nth_weekday = to_string(a.NthWeekday->Weekday);
        }
        if (a.DayOfMonth)
            r.anchor_day_of_month = static_cast<int>(*a.DayOfMonth);
        if (a.CalendarDaysBefore)
            r.anchor_calendar_days_before = static_cast<int>(*a.CalendarDaysBefore);
        if (a.LastWeekday)
            r.anchor_last_weekday = to_string(*a.LastWeekday);
        if (a.WeeklyDayOfTheWeek)
            r.anchor_weekly_day_of_the_week = to_string(*a.WeeklyDayOfTheWeek);
        if (a.BusinessDaysAfter)
            r.anchor_business_days_after = static_cast<int>(*a.BusinessDaysAfter);
    }

    if (v.OptionExpiryMonthLag)
        r.option_expiry_month_lag = static_cast<int>(*v.OptionExpiryMonthLag);

    if (v.OptionContractFrequency)
        r.option_contract_frequency = normalize_frequency(*v.OptionContractFrequency);

    if (v.OptionExpiryOffset)
        r.option_expiry_offset = static_cast<int>(*v.OptionExpiryOffset);

    if (v.OptionCalendarDaysBefore)
        r.option_calendar_days_before = static_cast<int>(*v.OptionCalendarDaysBefore);

    if (v.OptionMinBusinessDaysBefore)
        r.option_min_business_days_before = static_cast<int>(*v.OptionMinBusinessDaysBefore);

    if (v.OptionExpiryDay)
        r.option_expiry_day = static_cast<int>(*v.OptionExpiryDay);

    if (v.OptionNthWeekday) {
        r.option_nth_nth = static_cast<int>(v.OptionNthWeekday->Nth);
        r.option_nth_weekday = to_string(v.OptionNthWeekday->Weekday);
    }

    if (v.OptionExpiryLastWeekdayOfMonth)
        r.option_expiry_last_weekday_of_month = to_string(*v.OptionExpiryLastWeekdayOfMonth);

    if (v.OptionExpiryWeeklyDayOfTheWeek)
        r.option_expiry_weekly_day_of_the_week = to_string(*v.OptionExpiryWeeklyDayOfTheWeek);

    if (v.OptionBusinessDayConvention)
        r.option_business_day_convention = normalize_bdc(*v.OptionBusinessDayConvention);

    if (v.HoursPerDay)
        r.hours_per_day = static_cast<int>(*v.HoursPerDay);

    if (v.OffPeakPowerIndexData) {
        const auto& x = *v.OffPeakPowerIndexData;
        r.off_peak_index = std::string(x.OffPeakIndex);
        r.peak_index = std::string(x.PeakIndex);
        r.off_peak_hours = static_cast<double>(x.OffPeakHours);
        r.peak_calendar = std::string(x.PeakCalendar);
    }

    if (v.IndexName)
        r.index_name = std::string(*v.IndexName);

    if (v.SavingsTime)
        r.savings_time = std::string(*v.SavingsTime);

    if (v.DeliveryLocation)
        r.delivery_location = std::string(*v.DeliveryLocation);

    if (v.BalanceOfTheMonth)
        r.balance_of_the_month = parse_bool(*v.BalanceOfTheMonth);

    if (v.BalanceOfTheMonthPricingCalendar)
        r.balance_of_the_month_pricing_calendar = std::string(*v.BalanceOfTheMonthPricingCalendar);

    if (v.OptionUnderlyingFutureConvention)
        r.option_underlying_future_convention = std::string(*v.OptionUnderlyingFutureConvention);

    if (v.AveragingData) {
        const auto& a = *v.AveragingData;
        r.averaging_commodity_name = std::string(a.CommodityName);
        r.averaging_period = to_string(a.Period);
        r.averaging_pricing_calendar = std::string(a.PricingCalendar);
        r.averaging_conventions = std::string(a.Conventions);
        if (a.UseBusinessDays)
            r.averaging_use_business_days = parse_bool(*a.UseBusinessDays);
        if (a.DeliveryRollDays)
            r.averaging_delivery_roll_days = static_cast<int>(*a.DeliveryRollDays);
        if (a.FutureMonthOffset)
            r.averaging_future_month_offset = static_cast<int>(*a.FutureMonthOffset);
        if (a.DailyExpiryOffset)
            r.averaging_daily_expiry_offset = static_cast<int>(*a.DailyExpiryOffset);
    }

    if (v.ProhibitedExpiries) {
        std::string dates;
        for (const auto& d : v.ProhibitedExpiries->Dates.Date) {
            if (!dates.empty())
                dates += ",";
            dates += static_cast<const std::string&>(d);
        }
        r.prohibited_expiries = dates;
    }

    if (v.FutureContinuationMappings) {
        std::string mappings;
        for (const auto& m : v.FutureContinuationMappings->ContinuationMapping) {
            if (!mappings.empty())
                mappings += ",";
            mappings += std::to_string(m.From) + ":" + std::to_string(m.To);
        }
        r.future_continuation_mappings = mappings;
    }

    if (v.OptionContinuationMappings) {
        std::string mappings;
        for (const auto& m : v.OptionContinuationMappings->ContinuationMapping) {
            if (!mappings.empty())
                mappings += ",";
            mappings += std::to_string(m.From) + ":" + std::to_string(m.To);
        }
        r.option_continuation_mappings = mappings;
    }

    set_audit(r);
    return r;
}

refdata::domain::cms_spread_option_convention
conventions_mapper::map_cms_spread_option(const cmsSpreadOptionType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping CMS spread option convention: " << std::string(v.Id);

    refdata::domain::cms_spread_option_convention r;
    r.id = std::string(v.Id);
    r.forward_start = std::string(v.ForwardStart);
    r.spot_days = std::string(v.SpotDays);
    r.swap_tenor = std::string(v.SwapTenor);
    r.fixing_days = static_cast<int>(v.FixingDays);
    r.calendar = std::string(v.Calendar);
    r.day_count_fraction = normalize_day_counter(v.DayCounter);
    r.roll_convention = normalize_bdc(v.RollConvention);

    set_audit(r);
    return r;
}

refdata::domain::cross_currency_fix_float_convention
conventions_mapper::map_cross_currency_fix_float(const crossCurrencyFixFloatType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping cross-currency fix-float convention: "
                               << std::string(v.Id);

    refdata::domain::cross_currency_fix_float_convention r;
    r.id = std::string(v.Id);
    r.settlement_days = static_cast<int>(v.SettlementDays);
    r.settlement_calendar = std::string(v.SettlementCalendar);
    r.settlement_convention = normalize_bdc(v.SettlementConvention);
    r.fixed_currency = to_string(v.FixedCurrency);
    r.fixed_frequency = normalize_frequency(v.FixedFrequency);
    r.fixed_convention = normalize_bdc(v.FixedConvention);
    r.fixed_day_count_fraction = normalize_day_counter(v.FixedDayCounter);
    r.index = std::string(v.Index);

    if (v.EOM)
        r.eom = parse_bool(*v.EOM);

    if (v.IsResettable)
        r.is_resettable = parse_bool(*v.IsResettable);

    if (v.FloatIndexIsResettable)
        r.float_index_is_resettable = parse_bool(*v.FloatIndexIsResettable);

    if (v.IncludeSpread)
        r.include_spread = parse_bool(*v.IncludeSpread);

    if (v.Lookback)
        r.lookback = std::string(*v.Lookback);

    if (v.FixingDays)
        r.fixing_days = static_cast<int>(*v.FixingDays);

    if (v.RateCutoff)
        r.rate_cutoff = static_cast<int>(*v.RateCutoff);

    if (v.IsAveraged)
        r.is_averaged = parse_bool(*v.IsAveraged);

    if (v.ObservationShift)
        r.observation_shift = parse_bool(*v.ObservationShift);

    set_audit(r);
    return r;
}

refdata::domain::inflation_swap_convention
conventions_mapper::map_inflation_swap(const inflationswapType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping inflation swap convention: " << std::string(v.Id);

    refdata::domain::inflation_swap_convention r;
    r.id = std::string(v.Id);
    r.fix_calendar = std::string(v.FixCalendar);
    r.fix_convention = normalize_bdc(v.FixConvention);
    r.day_count_fraction = normalize_day_counter(v.DayCounter);
    r.index = std::string(v.Index);
    r.interpolated = parse_bool(v.Interpolated);
    r.observation_lag = std::string(v.ObservationLag);
    r.adjust_inflation_observation_dates = parse_bool(v.AdjustInflationObservationDates);
    r.inflation_calendar = std::string(v.InflationCalendar);
    r.inflation_convention = normalize_bdc(v.InflationConvention);

    if (v.PublicationRoll) {
        using pr = domain::publicationRoll;
        switch (*v.PublicationRoll) {
            case pr::None:
                r.publication_roll = "None";
                break;
            case pr::OnPublicationDate:
                r.publication_roll = "OnPublicationDate";
                break;
            case pr::AfterPublicationDate:
                r.publication_roll = "AfterPublicationDate";
                break;
            default:
                throw std::runtime_error("Unknown publication roll enum value");
        }
    }

    if (v.StartDelay)
        r.start_delay = std::string(*v.StartDelay);

    if (v.StartDelayConvention)
        r.start_delay_convention = normalize_bdc(*v.StartDelayConvention);

    set_audit(r);
    return r;
}

refdata::domain::bma_basis_swap_convention
conventions_mapper::map_bma_basis_swap(const bmaBasisSwapType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping BMA basis swap convention: " << std::string(v.Id);

    refdata::domain::bma_basis_swap_convention r;
    r.id = std::string(v.Id);
    r.index = std::string(v.Index);
    r.bma_index = std::string(v.BMAIndex);

    if (v.BMAPaymentCalendar)
        r.bma_payment_calendar = std::string(*v.BMAPaymentCalendar);

    if (v.BMAPaymentConvention)
        r.bma_payment_convention = normalize_bdc(*v.BMAPaymentConvention);

    if (v.BMAPaymentLag)
        r.bma_payment_lag = static_cast<int>(*v.BMAPaymentLag);

    if (v.IndexPaymentCalendar)
        r.index_payment_calendar = std::string(*v.IndexPaymentCalendar);

    if (v.IndexPaymentConvention)
        r.index_payment_convention = normalize_bdc(*v.IndexPaymentConvention);

    if (v.IndexPaymentLag)
        r.index_payment_lag = static_cast<int>(*v.IndexPaymentLag);

    if (v.IndexSettlementDays)
        r.index_settlement_days = static_cast<int>(*v.IndexSettlementDays);

    if (v.IndexPaymentPeriod)
        r.index_payment_period = std::string(*v.IndexPaymentPeriod);

    if (v.OvernightLockoutDays)
        r.overnight_lockout_days = static_cast<int>(*v.OvernightLockoutDays);

    set_audit(r);
    return r;
}

refdata::domain::zero_inflation_index_convention
conventions_mapper::map_zero_inflation_index(const zeroInflationIndexType& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping zero inflation index convention: "
                               << std::string(v.Id);

    refdata::domain::zero_inflation_index_convention r;
    r.id = std::string(v.Id);
    r.region_name = std::string(v.RegionName);
    r.region_code = std::string(v.RegionCode);
    r.revised = parse_bool(v.Revised);
    r.frequency = normalize_frequency(v.Frequency);
    r.availability_lag = std::string(v.AvailabilityLag);
    r.currency = to_string(v.Currency);

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

    if (v.AdvanceCalendar) {
        const std::string joined = *v.AdvanceCalendar;
        std::size_t start = 0;
        while (start <= joined.size()) {
            const auto comma = joined.find(',', start);
            const auto end = comma == std::string::npos ? joined.size() : comma;
            if (end > start)
                r.advance_calendars.push_back(joined.substr(start, end - start));
            if (comma == std::string::npos)
                break;
            start = comma + 1;
        }
    }

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
    if (v.SettlementDays)
        r.settlement_days = static_cast<int>(*v.SettlementDays);
    if (v.Calendar)
        r.calendar = std::string(*v.Calendar);
    if (v.Frequency)
        r.frequency = normalize_frequency(*v.Frequency);
    if (v.PaymentConvention)
        r.payment_convention = normalize_bdc(*v.PaymentConvention);
    if (v.Rule)
        r.rule = normalize_date_rule(*v.Rule);
    if (v.DayCounter)
        r.day_count_fraction = normalize_day_counter(*v.DayCounter);

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

    r.commodity_forward.reserve(v.CommodityForward.size());
    std::ranges::transform(v.CommodityForward,
                           std::back_inserter(r.commodity_forward),
                           [](const auto& x) { return map_commodity_forward(x); });

    r.bond_yield.reserve(v.BondYield.size());
    std::ranges::transform(v.BondYield,
                           std::back_inserter(r.bond_yield),
                           [](const auto& x) { return map_bond_yield(x); });

    r.commodity_future.reserve(v.CommodityFuture.size());
    std::ranges::transform(v.CommodityFuture,
                           std::back_inserter(r.commodity_future),
                           [](const auto& x) { return map_commodity_future(x); });

    r.cms_spread_option.reserve(v.CmsSpreadOption.size());
    std::ranges::transform(v.CmsSpreadOption,
                           std::back_inserter(r.cms_spread_option),
                           [](const auto& x) { return map_cms_spread_option(x); });

    r.cross_currency_fix_float.reserve(v.CrossCurrencyFixFloat.size());
    std::ranges::transform(v.CrossCurrencyFixFloat,
                           std::back_inserter(r.cross_currency_fix_float),
                           [](const auto& x) { return map_cross_currency_fix_float(x); });

    r.inflation_swap.reserve(v.InflationSwap.size());
    std::ranges::transform(v.InflationSwap,
                           std::back_inserter(r.inflation_swap),
                           [](const auto& x) { return map_inflation_swap(x); });

    r.bma_basis_swap.reserve(v.BMABasisSwap.size());
    std::ranges::transform(v.BMABasisSwap,
                           std::back_inserter(r.bma_basis_swap),
                           [](const auto& x) { return map_bma_basis_swap(x); });

    r.zero_inflation_index.reserve(v.ZeroInflationIndex.size());
    std::ranges::transform(v.ZeroInflationIndex,
                           std::back_inserter(r.zero_inflation_index),
                           [](const auto& x) { return map_zero_inflation_index(x); });

    r.overnight_index.reserve(v.OvernightIndex.size());
    std::ranges::transform(v.OvernightIndex,
                           std::back_inserter(r.overnight_index),
                           [](const auto& x) { return map_overnight_index(x); });

    r.fx.reserve(v.FX.size());
    std::ranges::transform(v.FX, std::back_inserter(r.fx), [](const auto& x) { return map_fx(x); });

    r.cds.reserve(v.CDS.size());
    std::ranges::transform(
        v.CDS, std::back_inserter(r.cds), [](const auto& x) { return map_cds(x); });

    r.tenor_basis_swap.reserve(v.TenorBasisSwap.size());
    std::ranges::transform(v.TenorBasisSwap,
                           std::back_inserter(r.tenor_basis_swap),
                           [](const auto& x) { return map_tenor_basis_swap(x); });

    r.tenor_basis_two_swap.reserve(v.TenorBasisTwoSwap.size());
    std::ranges::transform(v.TenorBasisTwoSwap,
                           std::back_inserter(r.tenor_basis_two_swap),
                           [](const auto& x) { return map_tenor_basis_two_swap(x); });

    r.cross_currency_basis.reserve(v.CrossCurrencyBasis.size());
    std::ranges::transform(v.CrossCurrencyBasis,
                           std::back_inserter(r.cross_currency_basis),
                           [](const auto& x) { return map_cross_currency_basis(x); });

    r.average_ois.reserve(v.AverageOIS.size());
    std::ranges::transform(v.AverageOIS, std::back_inserter(r.average_ois), [](const auto& x) {
        return map_average_ois(x);
    });

    r.fx_option.reserve(v.FxOption.size());
    std::ranges::transform(v.FxOption, std::back_inserter(r.fx_option), [](const auto& x) {
        return map_fx_option(x);
    });

    r.future.reserve(v.Future.size());
    std::ranges::transform(v.Future, std::back_inserter(r.future), [](const auto& x) {
        return map_future(x);
    });

    r.swap_index.reserve(v.SwapIndex.size());
    std::ranges::transform(
        v.SwapIndex, std::back_inserter(r.swap_index), [](const auto& x) {
            return map_swap_index(x);
        });

    // Every category the document carries that this mapper does not model. A
    // skip that is counted is a gap a caller can read; a skip that is silent is
    // a document losing content and saying nothing.
    const auto count_unmodelled = [&r](std::string_view name, std::size_t count) {
        if (count != 0)
            r.unmodelled.emplace(std::string(name), count);
    };
    count_unmodelled("FxOptionTimeWeighting", v.FxOptionTimeWeighting.size());
    count_unmodelled("IntradayPowerLoad", v.IntradayPowerLoad.size());

    std::size_t rebasing_events = 0;
    for (const auto& x : v.ZeroInflationIndex)
        if (x.RebasingEvents)
            ++rebasing_events;
    count_unmodelled("ZeroInflationIndex.RebasingEvents", rebasing_events);

    std::size_t publication_schedules = 0;
    for (const auto& x : v.InflationSwap)
        if (x.PublicationSchedule)
            ++publication_schedules;
    count_unmodelled("InflationSwap.PublicationSchedule", publication_schedules);

    for (const auto& [name, count] : r.unmodelled) {
        BOOST_LOG_SEV(lg(), warn)
            << "Convention category '" << name << "' has " << count
            << " element(s) and no entity to hold them; the document cannot round trip.";
    }

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

    r.CommodityForward.reserve(v.commodity_forward.size());
    for (const auto& x : v.commodity_forward)
        r.CommodityForward.push_back(reverse_commodity_forward(x));

    r.BondYield.reserve(v.bond_yield.size());
    for (const auto& x : v.bond_yield)
        r.BondYield.push_back(reverse_bond_yield(x));

    r.CommodityFuture.reserve(v.commodity_future.size());
    for (const auto& x : v.commodity_future)
        r.CommodityFuture.push_back(reverse_commodity_future(x));

    r.CmsSpreadOption.reserve(v.cms_spread_option.size());
    for (const auto& x : v.cms_spread_option)
        r.CmsSpreadOption.push_back(reverse_cms_spread_option(x));

    r.CrossCurrencyFixFloat.reserve(v.cross_currency_fix_float.size());
    for (const auto& x : v.cross_currency_fix_float)
        r.CrossCurrencyFixFloat.push_back(reverse_cross_currency_fix_float(x));

    r.InflationSwap.reserve(v.inflation_swap.size());
    for (const auto& x : v.inflation_swap)
        r.InflationSwap.push_back(reverse_inflation_swap(x));

    r.BMABasisSwap.reserve(v.bma_basis_swap.size());
    for (const auto& x : v.bma_basis_swap)
        r.BMABasisSwap.push_back(reverse_bma_basis_swap(x));

    r.ZeroInflationIndex.reserve(v.zero_inflation_index.size());
    for (const auto& x : v.zero_inflation_index)
        r.ZeroInflationIndex.push_back(reverse_zero_inflation_index(x));

    r.OvernightIndex.reserve(v.overnight_index.size());
    for (const auto& x : v.overnight_index)
        r.OvernightIndex.push_back(reverse_overnight_index(x));

    r.FX.reserve(v.fx.size());
    for (const auto& x : v.fx)
        r.FX.push_back(reverse_fx(x));

    r.CDS.reserve(v.cds.size());
    for (const auto& x : v.cds)
        r.CDS.push_back(reverse_cds(x));

    r.TenorBasisSwap.reserve(v.tenor_basis_swap.size());
    for (const auto& x : v.tenor_basis_swap)
        r.TenorBasisSwap.push_back(reverse_tenor_basis_swap(x));

    r.TenorBasisTwoSwap.reserve(v.tenor_basis_two_swap.size());
    for (const auto& x : v.tenor_basis_two_swap)
        r.TenorBasisTwoSwap.push_back(reverse_tenor_basis_two_swap(x));

    r.CrossCurrencyBasis.reserve(v.cross_currency_basis.size());
    for (const auto& x : v.cross_currency_basis)
        r.CrossCurrencyBasis.push_back(reverse_cross_currency_basis(x));

    r.AverageOIS.reserve(v.average_ois.size());
    for (const auto& x : v.average_ois)
        r.AverageOIS.push_back(reverse_average_ois(x));

    r.FxOption.reserve(v.fx_option.size());
    for (const auto& x : v.fx_option)
        r.FxOption.push_back(reverse_fx_option(x));

    r.Future.reserve(v.future.size());
    for (const auto& x : v.future)
        r.Future.push_back(reverse_future(x));

    r.SwapIndex.reserve(v.swap_index.size());
    for (const auto& x : v.swap_index)
        r.SwapIndex.push_back(reverse_swap_index(x));

    BOOST_LOG_SEV(lg(), debug) << "Finished reverse-mapping conventions.";
    return r;
}

}
