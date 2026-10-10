#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
"""Generate the DQ populate SQL for the canonical ORE convention set.

Input:  conventions-canonical.tsv, written by tools/ore_conventions/extract.py.
Output: projects/ores.sql/populate/refdata/refdata_conventions_seed_populate.sql

One artefact row is written per canonical entry, into the artefact table of the
entry's kind. The publish function resolves the artefact table to the live table
by name, so the generator only has to map the ORE XML field names in the
signature to the live table's columns.

The mapping and the value normalisers mirror
projects/ores.ore/core/src/domain/conventions_mapper.cpp, which is the
authority for how an ORE convention reaches the store. A value the mapper would
not accept makes this generator fail loudly instead of writing a bad row.

Each seeded row's oresmd_uri comes from oresmd_index_map.tsv in this directory:
the ORE index name the row carries, mapped to the oresmd fixing URI the codecs
produce for it. The map is generated once from the codecs and checked by the
marketdata codec tests, so it cannot drift from them silently. A name the map
does not hold fails the run and is named.
"""

import argparse
import csv
import re
import sys
from pathlib import Path

# --- Normalisers ------------------------------------------------------------
# Each mirrors a normalize_* function in conventions_mapper.cpp.

DAY_COUNTER = {
    "A360": "ACT/360",
    "ACT/360": "ACT/360",
    "Actual/360": "ACT/360",
    "Act/360": "ACT/360",
    "A360 (incl. last)": "ACT/360 (incl. last)",
    "A365": "ACT/365.FIXED",
    "A365F": "ACT/365.FIXED",
    "ACT/365": "ACT/365.FIXED",
    "ACT/365.FIXED": "ACT/365.FIXED",
    "ACT/365L": "ACT/365L",
    "ACT/365 (Canadian Bond)": "ACT/365 (Canadian Bond)",
    "T360": "T360",
    "30/360": "30/360",
    "ACT/nACT": "ACT/nACT",
    "30E/360": "30E/360",
    "30E/360.ISDA": "30E/360.ISDA",
    "30/360 (German)": "30/360 (German)",
    "30/360 (Italian)": "30/360 (Italian)",
    "ACT/ACT": "ACT/ACT.ISDA",
    "ACT/ACT.ISDA": "ACT/ACT.ISDA",
    "Actual/Actual (ISDA)": "ACT/ACT.ISDA",
    "ACT/ACT.ISMA": "ACT/ACT.ISMA",
    "ACT/ACT (ISMA)": "ACT/ACT.ISMA",
    "ACT/ACT (ICMA)": "ACT/ACT.ISMA",
    "ACT/ACT.AFB": "ACT/ACT.AFB",
    "1/1": "1/1",
    "BUS/252": "BUS/252",
    "Business/252": "BUS/252",
    "NL/365": "NL/365",
    "ACT/365 (JGB)": "ACT/365 (JGB)",
    "Simple": "Simple",
    "Year": "Year",
    "Month": "Month",
    "ACT/364": "ACT/364",
}

BDC = {
    "F": "Following",
    "Following": "Following",
    "MF": "ModifiedFollowing",
    "ModifiedFollowing": "ModifiedFollowing",
    "Modified Following": "ModifiedFollowing",
    "P": "Preceding",
    "Preceding": "Preceding",
    "MP": "ModifiedPreceding",
    "ModifiedPreceding": "ModifiedPreceding",
    "Modified Preceding": "ModifiedPreceding",
    "HMMF": "HalfMonthModifiedFollowing",
    "HalfMonthModifiedFollowing": "HalfMonthModifiedFollowing",
    "Half Month Modified Following": "HalfMonthModifiedFollowing",
    "Nearest": "Nearest",
    "Unadjusted": "Unadjusted",
    "U": "Unadjusted",
}

FREQUENCY = {
    "Once": "Once",
    "Z": "Once",
    "Annual": "Annual",
    "A": "Annual",
    "Semiannual": "Semiannual",
    "S": "Semiannual",
    "Quarterly": "Quarterly",
    "Q": "Quarterly",
    "Bimonthly": "Bimonthly",
    "B": "Bimonthly",
    "Monthly": "Monthly",
    "M": "Monthly",
    "Lunarmonth": "Lunarmonth",
    "L": "Lunarmonth",
    "Weekly": "Weekly",
    "W": "Weekly",
    "Daily": "Daily",
    "D": "Daily",
}

COMPOUNDING = {
    "Simple": "Simple",
    "Compounded": "Compounded",
    "Continuous": "Continuous",
    "SimpleThenCompounded": "SimpleThenCompounded",
}

DATE_RULE = {
    "Backward": "Backward",
    "Forward": "Forward",
    "Zero": "Zero",
    "ThirdWednesday": "ThirdWednesday",
    "Twentieth": "Twentieth",
    "TwentiethIMM": "TwentiethIMM",
    "OldCDS": "OldCDS",
    "CDS": "CDS",
    "CDS2015": "CDS2015",
    "ThirdThursday": "ThirdThursday",
    "ThirdFriday": "ThirdFriday",
    "MondayAfterThirdFriday": "MondayAfterThirdFriday",
    "TuesdayAfterThirdFriday": "TuesdayAfterThirdFriday",
    "LastWednesday": "LastWednesday",
    "EveryThursday": "EveryThursday",
}


class GenError(Exception):
    pass


def norm(table, kind, field, value):
    try:
        return table[value]
    except KeyError:
        raise GenError(
            f"{kind}.{field}: cannot normalise {value!r}; "
            "extend the table in generate_dq_seed.py"
        )


def parse_int(kind, field, value):
    try:
        return str(int(value))
    except ValueError:
        raise GenError(f"{kind}.{field}: {value!r} is not an integer")


def parse_float(kind, field, value):
    try:
        return repr(float(value))
    except ValueError:
        raise GenError(f"{kind}.{field}: {value!r} is not a number")


def parse_bool(kind, field, value):
    return "true" if value.strip().lower() in ("true", "y", "yes", "1") else "false"


def parse_text(kind, field, value):
    return value


PARSERS = {
    "text": parse_text,
    "int": parse_int,
    "float": parse_float,
    "bool": parse_bool,
    "daycounter": lambda k, f, v: norm(DAY_COUNTER, k, f, v),
    "bdc": lambda k, f, v: norm(BDC, k, f, v),
    "frequency": lambda k, f, v: norm(FREQUENCY, k, f, v),
    "compounding": lambda k, f, v: norm(COMPOUNDING, k, f, v),
    "daterule": lambda k, f, v: norm(DATE_RULE, k, f, v),
}

# --- Kind specifications ----------------------------------------------------
# kind -> (table stem, [(xml field, column, parser type), ...])
# The table stem expands to ores_refdata_<stem>_conventions_tbl and
# ores_dq_<stem>_conventions_artefact_tbl.

KINDS = {
    "AverageOIS": ("average_ois", [
        ("SpotLag", "spot_lag", "int"),
        ("FixedTenor", "fixed_tenor", "text"),
        ("FixedDayCounter", "fixed_day_count_fraction", "daycounter"),
        ("FixedCalendar", "fixed_calendar", "text"),
        ("FixedConvention", "fixed_convention", "bdc"),
        ("FixedPaymentConvention", "fixed_payment_convention", "bdc"),
        ("FixedFrequency", "fixed_frequency", "frequency"),
        ("Index", "index", "text"),
        ("OnTenor", "on_tenor", "text"),
        ("RateCutoff", "rate_cutoff", "text"),
    ]),
    "BMABasisSwap": ("bma_basis_swap", [
        ("Index", "index", "text"),
        ("BMAIndex", "bma_index", "text"),
        ("BMAPaymentLag", "bma_payment_lag", "int"),
        ("IndexPaymentLag", "index_payment_lag", "int"),
        ("IndexSettlementDays", "index_settlement_days", "int"),
        ("IndexPaymentPeriod", "index_payment_period", "text"),
        ("OvernightLockoutDays", "overnight_lockout_days", "int"),
    ]),
    "BondYield": ("bond_yield", [
        ("Compounding", "compounding", "compounding"),
        ("Frequency", "frequency", "frequency"),
        ("PriceType", "price_type", "text"),
        ("Accuracy", "accuracy", "float"),
        ("MaxEvaluations", "max_evaluations", "int"),
        ("Guess", "guess", "float"),
    ]),
    "CDS": ("cds", [
        ("SettlementDays", "settlement_days", "int"),
        ("Calendar", "calendar", "text"),
        ("Frequency", "frequency", "frequency"),
        ("PaymentConvention", "payment_convention", "bdc"),
        ("Rule", "rule", "daterule"),
        ("DayCounter", "day_count_fraction", "daycounter"),
        ("SettlesAccrual", "settles_accrual", "bool"),
        ("PaysAtDefaultTime", "pays_at_default_time", "bool"),
    ]),
    "CmsSpreadOption": ("cms_spread_option", [
        ("ForwardStart", "forward_start", "text"),
        ("SpotDays", "spot_days", "text"),
        ("SwapTenor", "swap_tenor", "text"),
        ("FixingDays", "fixing_days", "int"),
        ("Calendar", "calendar", "text"),
        ("DayCounter", "day_count_fraction", "daycounter"),
        ("RollConvention", "roll_convention", "bdc"),
    ]),
    "CommodityForward": ("commodity_forward", [
        ("SpotDays", "spot_days", "int"),
        ("PointsFactor", "points_factor", "float"),
        ("AdvanceCalendar", "advance_calendar", "text"),
        ("SpotRelative", "spot_relative", "bool"),
        ("DeliveryLocation", "delivery_location", "text"),
        ("BusinessDayConvention", "business_day_convention", "bdc"),
        ("Outright", "outright", "bool"),
    ]),
    "CommodityFuture": ("commodity_future", [
        ("ContractFrequency", "contract_frequency", "frequency"),
        ("Calendar", "calendar", "text"),
        ("ExpiryCalendar", "expiry_calendar", "text"),
        ("ExpiryMonthLag", "expiry_month_lag", "int"),
        ("OneContractMonth", "one_contract_month", "text"),
        ("OffsetDays", "offset_days", "int"),
        ("BusinessDayConvention", "business_day_convention", "bdc"),
        ("AdjustBeforeOffset", "adjust_before_offset", "bool"),
        ("IsAveraging", "is_averaging", "bool"),
        ("OptionExpiryMonthLag", "option_expiry_month_lag", "int"),
        ("OptionContractFrequency", "option_contract_frequency", "frequency"),
        ("OptionExpiryOffset", "option_expiry_offset", "int"),
        ("OptionCalendarDaysBefore", "option_calendar_days_before", "int"),
        ("OptionMinBusinessDaysBefore", "option_min_business_days_before", "int"),
        ("OptionExpiryDay", "option_expiry_day", "int"),
        ("OptionExpiryLastWeekdayOfMonth", "option_expiry_last_weekday_of_month", "text"),
        ("OptionExpiryWeeklyDayOfTheWeek", "option_expiry_weekly_day_of_the_week", "text"),
        ("OptionBusinessDayConvention", "option_business_day_convention", "bdc"),
        ("HoursPerDay", "hours_per_day", "int"),
        ("IndexName", "index_name", "text"),
        ("SavingsTime", "savings_time", "text"),
        ("DeliveryLocation", "delivery_location", "text"),
        ("BalanceOfTheMonth", "balance_of_the_month", "bool"),
        ("BalanceOfTheMonthPricingCalendar", "balance_of_the_month_pricing_calendar", "text"),
        ("OptionUnderlyingFutureConvention", "option_underlying_future_convention", "text"),
    ]),
    "CrossCurrencyBasis": ("cross_currency_basis", [
        ("SettlementDays", "settlement_days", "int"),
        ("SettlementCalendar", "settlement_calendar", "text"),
        ("RollConvention", "roll_convention", "bdc"),
        ("FlatIndex", "flat_index", "text"),
        ("SpreadIndex", "spread_index", "text"),
        ("EOM", "eom", "bool"),
        ("IsResettable", "is_resettable", "bool"),
        ("FlatIndexIsResettable", "flat_index_is_resettable", "bool"),
        ("FlatTenor", "flat_tenor", "text"),
        ("SpreadTenor", "spread_tenor", "text"),
        ("SpreadPaymentLag", "spread_payment_lag", "int"),
        ("FlatPaymentLag", "flat_payment_lag", "int"),
        ("SpreadIncludeSpread", "spread_include_spread", "bool"),
        ("SpreadLookback", "spread_lookback", "text"),
        ("SpreadFixingDays", "spread_fixing_days", "int"),
        ("SpreadRateCutoff", "spread_rate_cutoff", "int"),
        ("SpreadIsAveraged", "spread_is_averaged", "bool"),
        ("SpreadObservationShift", "spread_observation_shift", "bool"),
        ("FlatIncludeSpread", "flat_include_spread", "bool"),
        ("FlatLookback", "flat_lookback", "text"),
        ("FlatFixingDays", "flat_fixing_days", "int"),
        ("FlatRateCutoff", "flat_rate_cutoff", "int"),
        ("FlatIsAveraged", "flat_is_averaged", "bool"),
        ("FlatObservationShift", "flat_observation_shift", "bool"),
    ]),
    "CrossCurrencyFixFloat": ("cross_currency_fix_float", [
        ("SettlementDays", "settlement_days", "int"),
        ("SettlementCalendar", "settlement_calendar", "text"),
        ("SettlementConvention", "settlement_convention", "bdc"),
        ("FixedCurrency", "fixed_currency", "text"),
        ("FixedFrequency", "fixed_frequency", "frequency"),
        ("FixedConvention", "fixed_convention", "bdc"),
        ("FixedDayCounter", "fixed_day_count_fraction", "daycounter"),
        ("Index", "index", "text"),
        ("EOM", "eom", "bool"),
        ("IsResettable", "is_resettable", "bool"),
        ("FloatIndexIsResettable", "float_index_is_resettable", "bool"),
        ("IncludeSpread", "include_spread", "bool"),
        ("Lookback", "lookback", "text"),
        ("FixingDays", "fixing_days", "int"),
        ("RateCutoff", "rate_cutoff", "int"),
        ("IsAveraged", "is_averaged", "bool"),
        ("ObservationShift", "observation_shift", "bool"),
    ]),
    "Deposit": ("deposit", [
        ("IndexBased", "index_based", "bool"),
        ("Index", "index", "text"),
        ("Calendar", "calendar", "text"),
        ("Convention", "convention", "bdc"),
        ("EOM", "end_of_month", "bool"),
        ("DayCounter", "day_count_fraction", "daycounter"),
        ("SettlementDays", "settlement_days", "int"),
    ]),
    "FRA": ("fra", [
        ("Index", "index", "text"),
    ]),
    "Future": ("future", [
        ("Index", "index", "text"),
        ("DateGenerationRule", "date_generation_rule", "text"),
        ("OvernightIndexFutureNettingType", "netting_type", "text"),
        ("Calendar", "calendar", "text"),
        ("OvernightIndexTenor", "overnight_index_tenor", "text"),
    ]),
    "FxOption": ("fx_option", [
        ("FXConventionID", "fx_convention_id", "text"),
        ("AtmType", "atm_type", "text"),
        ("DeltaType", "delta_type", "text"),
        ("SwitchTenor", "switch_tenor", "text"),
        ("LongTermAtmType", "long_term_atm_type", "text"),
        ("LongTermDeltaType", "long_term_delta_type", "text"),
        ("RiskReversalInFavorOf", "risk_reversal_in_favor_of", "text"),
        ("ButterflyStyle", "butterfly_style", "text"),
    ]),
    "IborIndex": ("ibor_index", [
        ("FixingCalendar", "fixing_calendar", "text"),
        ("DayCounter", "day_count_fraction", "daycounter"),
        ("SettlementDays", "settlement_days", "int"),
        ("BusinessDayConvention", "business_day_convention", "bdc"),
        ("EndOfMonth", "end_of_month", "bool"),
    ]),
    "InflationSwap": ("inflation_swap", [
        ("FixCalendar", "fix_calendar", "text"),
        ("FixConvention", "fix_convention", "bdc"),
        ("DayCounter", "day_count_fraction", "daycounter"),
        ("Index", "index", "text"),
        ("Interpolated", "interpolated", "bool"),
        ("ObservationLag", "observation_lag", "text"),
        ("AdjustInflationObservationDates", "adjust_inflation_observation_dates", "bool"),
        ("InflationCalendar", "inflation_calendar", "text"),
        ("InflationConvention", "inflation_convention", "bdc"),
        ("PublicationRoll", "publication_roll", "text"),
        ("StartDelay", "start_delay", "text"),
        ("StartDelayConvention", "start_delay_convention", "bdc"),
    ]),
    # PowerLoadProfileData is decoded by decode_intraday_power_load, not by a
    # one-to-one field map.
    "IntradayPowerLoad": ("intraday_power_load", []),
    "OIS": ("ois", [
        ("SpotLag", "spot_lag", "int"),
        ("Index", "index", "text"),
        ("FixedDayCounter", "fixed_day_count_fraction", "daycounter"),
        ("FixedCalendar", "fixed_calendar", "text"),
        ("PaymentLag", "payment_lag", "int"),
        ("EOM", "end_of_month", "bool"),
        ("FixedFrequency", "fixed_frequency", "frequency"),
        ("FixedConvention", "fixed_convention", "bdc"),
        ("FixedPaymentConvention", "fixed_payment_convention", "bdc"),
        ("Rule", "rule", "daterule"),
        ("PaymentCalendar", "payment_calendar", "text"),
        ("RateCutoff", "rate_cutoff", "int"),
    ]),
    "OvernightIndex": ("overnight_index", [
        ("FixingCalendar", "fixing_calendar", "text"),
        ("DayCounter", "day_count_fraction", "daycounter"),
        ("SettlementDays", "settlement_days", "int"),
    ]),
    "Swap": ("swap", [
        ("FixedCalendar", "fixed_calendar", "text"),
        ("FixedFrequency", "fixed_frequency", "frequency"),
        ("FixedConvention", "fixed_convention", "bdc"),
        ("FixedDayCounter", "fixed_day_count_fraction", "daycounter"),
        ("Index", "index", "text"),
        ("FloatFrequency", "float_frequency", "frequency"),
        ("SubPeriodsCouponType", "sub_periods_coupon_type", "text"),
    ]),
    "SwapIndex": ("swap_index", [
        ("Conventions", "conventions", "text"),
        ("FixingCalendar", "fixing_calendar", "text"),
    ]),
    "TenorBasisSwap": ("tenor_basis_swap", [
        ("PayIndex", "pay_index", "text"),
        ("PayFrequency", "pay_frequency", "text"),
        ("ReceiveIndex", "receive_index", "text"),
        ("ReceiveFrequency", "receive_frequency", "text"),
        ("SpreadOnRec", "spread_on_rec", "bool"),
        ("IncludeSpread", "include_spread", "bool"),
        ("SubPeriodsCouponType", "sub_periods_coupon_type", "text"),
        ("PayIsAveraged", "pay_is_averaged", "bool"),
        ("RecIsAveraged", "rec_is_averaged", "bool"),
        ("LongIndex", "long_index", "text"),
        ("LongPayTenor", "long_pay_tenor", "text"),
        ("ShortIndex", "short_index", "text"),
        ("ShortPayTenor", "short_pay_tenor", "text"),
        ("SpreadOnShort", "spread_on_short", "bool"),
    ]),
    "TenorBasisTwoSwap": ("tenor_basis_two_swap", [
        ("Calendar", "calendar", "text"),
        ("LongFixedFrequency", "long_fixed_frequency", "frequency"),
        ("LongFixedConvention", "long_fixed_convention", "bdc"),
        ("LongFixedDayCounter", "long_fixed_day_count_fraction", "daycounter"),
        ("LongIndex", "long_index", "text"),
        ("ShortFixedFrequency", "short_fixed_frequency", "frequency"),
        ("ShortFixedConvention", "short_fixed_convention", "bdc"),
        ("ShortFixedDayCounter", "short_fixed_day_count_fraction", "daycounter"),
        ("ShortIndex", "short_index", "text"),
        ("LongMinusShort", "long_minus_short", "bool"),
    ]),
    "Zero": ("zero", [
        ("TenorBased", "tenor_based", "bool"),
        ("DayCounter", "day_count_fraction", "daycounter"),
        ("Compounding", "compounding", "compounding"),
        ("CompoundingFrequency", "compounding_frequency", "frequency"),
        ("TenorCalendar", "tenor_calendar", "text"),
        ("SpotLag", "spot_lag", "int"),
        ("SpotCalendar", "spot_calendar", "text"),
        ("RollConvention", "roll_convention", "bdc"),
        ("EOM", "end_of_month", "bool"),
    ]),
    "ZeroInflationIndex": ("zero_inflation_index", [
        ("RegionName", "region_name", "text"),
        ("RegionCode", "region_code", "text"),
        ("Revised", "revised", "bool"),
        ("Frequency", "frequency", "frequency"),
        ("AvailabilityLag", "availability_lag", "text"),
        ("Currency", "currency", "text"),
    ]),
}

# Kinds deliberately not seeded, with the reason stated in the generated file.
CUT_KINDS = {
    "FX": "FX conventions are world data already carried by the "
          "refdata.currency_pair_conventions dataset; there is no party-scoped "
          "FX convention table.",
    "IntradayPowerLoad": "the PowerLoadProfileData field is a nested load "
          "profile whose stored form is a bespoke encoding; mapping it is a "
          "task of its own.",
}

# Columns that are world data: the live table has no party_id.
WORLD_KINDS = {"ibor_index", "overnight_index"}

# --- The oresmd index map ---------------------------------------------------
# Every ORE index name the canonical set references maps to the oresmd fixing
# URI the two codecs produce for it:
# ore_index_codec::read(name) then oresmd_uri_codec::write_index(index), both in
# projects/ores.marketdata/core. The map is generated once from the codecs,
# committed beside this script, and round-tripped by the marketdata codec tests,
# so a codec change that moved a URI fails that test rather than the seed.

DEFAULT_MAP = Path(__file__).resolve().parent / "oresmd_index_map.tsv"

# The signature fields that hold an ORE index name, and the kinds whose id is
# the index name itself. Together they are the canonical index reference set the
# map must cover.
INDEX_FIELDS = ("Index", "BMAIndex", "FlatIndex", "SpreadIndex", "IndexName",
                "PayIndex", "ReceiveIndex", "LongIndex", "ShortIndex")
INDEX_KINDS = ("IborIndex", "OvernightIndex", "ZeroInflationIndex", "SwapIndex")

# The field a row's single oresmd_uri is written from, in priority order, when
# the kind does not define the index in its id. The model doc string names the
# same field. A kind absent here names no index, so its rows carry no URI: it is
# a requirement, per the model doc string.
PRIMARY_INDEX = {
    "AverageOIS": ("Index",),
    "BMABasisSwap": ("Index", "BMAIndex"),
    "CommodityFuture": ("IndexName",),
    "CrossCurrencyBasis": ("FlatIndex", "SpreadIndex"),
    "CrossCurrencyFixFloat": ("Index",),
    "Deposit": ("Index",),
    "FRA": ("Index",),
    "Future": ("Index",),
    "InflationSwap": ("Index",),
    "OIS": ("Index",),
    "Swap": ("Index",),
    "TenorBasisSwap": ("PayIndex", "ReceiveIndex", "LongIndex", "ShortIndex"),
    "TenorBasisTwoSwap": ("LongIndex", "ShortIndex"),
}


def load_index_map(path):
    """The committed ore_name -> oresmd URI table, or why it cannot be read."""
    try:
        lines = Path(path).read_text(encoding="utf-8").splitlines()
    except OSError as exc:
        raise GenError(f"cannot read the index map {path}: {exc}")
    index_map = {}
    for number, line in enumerate(lines, 1):
        if not line.strip():
            continue
        parts = line.split("\t")
        if len(parts) != 2 or not parts[0] or not parts[1]:
            raise GenError(
                f"{path}:{number}: not 'ore_name<TAB>oresmd_uri': {line!r}")
        index_map[parts[0]] = parts[1]
    return index_map


def primary_index_name(kind, id_value, fields):
    """The index name one row's oresmd_uri is written from, or None."""
    if kind in INDEX_KINDS:
        return id_value
    for field in PRIMARY_INDEX.get(kind, ()):
        if fields.get(field, "").strip():
            return fields[field]
    return None


def referenced_index_names(kind, id_value, fields):
    """Every index name one canonical row references."""
    names = {fields[f] for f in INDEX_FIELDS if fields.get(f, "").strip()}
    if kind in INDEX_KINDS:
        names.add(id_value)
    return names


def check_index_references(rows, index_map):
    """Fail on any index name a seeded row references but the map omits."""
    missing = {}
    for row in rows:
        kind = row["kind"]
        if kind not in KINDS:
            continue
        fields = parse_signature(row["signature"])
        for name in referenced_index_names(kind, row["id"], fields):
            if name not in index_map:
                missing.setdefault(name, f"{kind} {row['id']}")
    if missing:
        detail = ", ".join(f"{n!r} ({w})" for n, w in sorted(missing.items()))
        raise GenError("index names not in the map: " + detail)


def parse_signature(signature):
    """Split a signature into {field: value}. The tool joins fields with ' ; '."""
    out = {}
    for part in signature.split(" ; "):
        if "=" not in part:
            continue
        name, value = part.split("=", 1)
        out[name] = value
    return out


def split_nested(value, sep="/"):
    """Split a nested value into [(key, value)] on the first '=' of each token."""
    pairs = []
    for token in value.split(sep):
        if "=" in token:
            k, v = token.split("=", 1)
            pairs.append((k, v))
    return pairs


def decode_commodity_future(fields):
    """Decode the nested CommodityFuture fields into their stored columns."""
    out = {}

    val = fields.get("AnchorDay")
    if val:
        for k, v in split_nested(val):
            if k == "NthWeekday":
                for k2, v2 in split_nested(v):
                    if k2 == "Nth":
                        out["anchor_nth_nth"] = str(int(v2))
                    elif k2 == "Weekday":
                        out["anchor_nth_weekday"] = v2
            elif k == "DayOfMonth":
                out["anchor_day_of_month"] = str(int(v))
            elif k == "CalendarDaysBefore":
                out["anchor_calendar_days_before"] = str(int(v))
            elif k == "BusinessDaysAfter":
                out["anchor_business_days_after"] = str(int(v))
            elif k == "LastWeekday":
                out["anchor_last_weekday"] = v
            elif k == "WeeklyDayOfTheWeek":
                out["anchor_weekly_day_of_the_week"] = v

    val = fields.get("ValidContractMonths")
    if val:
        months = [v for k, v in split_nested(val) if k == "Month"]
        if months:
            out["valid_contract_months"] = ",".join(months)

    val = fields.get("OffPeakPowerIndexData")
    if val:
        for k, v in split_nested(val):
            if k == "OffPeakHours":
                out["off_peak_hours"] = repr(float(v))
            elif k == "OffPeakIndex":
                out["off_peak_index"] = v
            elif k == "PeakCalendar":
                out["peak_calendar"] = v
            elif k == "PeakIndex":
                out["peak_index"] = v

    val = fields.get("AveragingData")
    if val:
        for k, v in split_nested(val):
            if k == "CommodityName":
                out["averaging_commodity_name"] = v
            elif k == "Conventions":
                out["averaging_conventions"] = v
            elif k == "Period":
                out["averaging_period"] = v
            elif k == "PricingCalendar":
                out["averaging_pricing_calendar"] = v
            elif k == "UseBusinessDays":
                out["averaging_use_business_days"] = parse_bool(
                    "CommodityFuture", "AveragingData.UseBusinessDays", v)
            elif k == "DeliveryRollDays":
                out["averaging_delivery_roll_days"] = str(int(v))
            elif k == "FutureMonthOffset":
                out["averaging_future_month_offset"] = str(int(v))
            elif k == "DailyExpiryOffset":
                out["averaging_daily_expiry_offset"] = str(int(v))

    val = fields.get("ProhibitedExpiries")
    if val:
        if val.startswith("Dates="):
            val = val[len("Dates="):]
        dates = [v for k, v in split_nested(val) if k == "Date"]
        if dates:
            out["prohibited_expiries"] = ",".join(dates)

    for field, column in (("FutureContinuationMappings", "future_continuation_mappings"),
                          ("OptionContinuationMappings", "option_continuation_mappings")):
        val = fields.get(field)
        if not val:
            continue
        pairs = []
        cur = {}
        for k, v in split_nested(val):
            if k == "ContinuationMapping":
                cur = {}
            elif k == "From":
                cur["from"] = v
            elif k == "To":
                cur["to"] = v
                if "from" in cur:
                    pairs.append(f"{int(cur['from'])}:{int(cur['to'])}")
        if pairs:
            out[column] = ",".join(pairs)

    return out


def decode_intraday_power_load(fields):
    """Decode PowerLoadProfileData into the two stored profile columns."""
    val = fields.get("PowerLoadProfileData")
    out = {}
    if not val:
        return out

    # explicit_load_profile: '<date>|<f>,<f>,...;<date>|...'
    explicit = []
    for m in re.finditer(
            r"LoadProfileDatum=Date=(\d{4}-\d{2}-\d{2})"
            r"((?:/LoadFactors=(?:LoadFactor=\d+/?)+)+)", val):
        factors = re.findall(r"LoadFactor=(\d+)", m.group(2))
        explicit.append(m.group(1) + "|" + ",".join(factors))
    if explicit:
        out["explicit_load_profile"] = ";".join(explicit)

    # business_day_load_rules: '<date>|<cal>|<f>,...|<f>,...'
    rules = []
    for m in re.finditer(
            r"LoadProfileBusinessDayRule="
            r"BusinessDayLoadFactors=(?:LoadFactor=\d+/?)+"
            r"/Calendar=([^/]+)/Date=(\d{4}-\d{2}-\d{2})", val):
        rules.append(m.group(2) + "|" + m.group(1) + "||")
    if rules:
        out["business_day_load_rules"] = ";".join(rules)
    return out


NESTED = {
    "CommodityFuture": decode_commodity_future,
    "IntradayPowerLoad": decode_intraday_power_load,
}


def sql_literal(value):
    if value is None:
        return "null"
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, (int, float)):
        return repr(value)
    return "'" + value.replace("'", "''") + "'"


def build_rows(rows, kind, spec, index_map):
    """Return [(id, {column: sql literal})] for one kind."""
    stem, mapping = spec
    nested = NESTED.get(kind)
    out = []
    for row in rows:
        if row["kind"] != kind:
            continue
        fields = parse_signature(row["signature"])
        values = {"id": sql_literal(row["id"])}
        for field, column, ptype in mapping:
            if field not in fields:
                continue
            raw = fields[field]
            if ptype == "text":
                parsed = raw
            else:
                parsed = PARSERS[ptype](kind, field, raw)
            values[column] = sql_literal(parsed)
        if nested:
            for column, raw in nested(fields).items():
                values[column] = sql_literal(raw)
        name = primary_index_name(kind, row["id"], fields)
        if name is not None:
            if name not in index_map:
                raise GenError(
                    f"{kind} {row['id']}: index {name!r} is not in the index "
                    f"map {DEFAULT_MAP.name}; regenerate it from the codec")
            values["oresmd_uri"] = sql_literal(index_map[name])
        out.append((row["id"], values, stem))
    return out


def all_referenced_index_names(rows):
    """Every index name the seeded kinds reference, sorted and unique.

    This is the canonical set the index map must cover, and the input the
    shell's offline codec command turns into the map.
    """
    names = set()
    for row in rows:
        kind = row["kind"]
        if kind not in KINDS:
            continue
        fields = parse_signature(row["signature"])
        names |= referenced_index_names(kind, row["id"], fields)
    return sorted(names)


def read_canonical_rows(tsv_path):
    with open(tsv_path, newline="", encoding="utf-8") as fh:
        return [r for r in csv.DictReader(fh, delimiter="\t") if r.get("kind")]


def generate(tsv_path, index_map_path=DEFAULT_MAP):
    rows = read_canonical_rows(tsv_path)
    kinds = sorted({r["kind"] for r in rows})
    unknown = [k for k in kinds if k not in KINDS and k not in CUT_KINDS]
    if unknown:
        raise GenError(
            "kinds with no mapping: " + ", ".join(unknown)
            + "; add them to KINDS or CUT_KINDS in generate_dq_seed.py")

    index_map = load_index_map(index_map_path)
    check_index_references(rows, index_map)

    seeded = [k for k in kinds if k in KINDS]
    seeded.sort()
    per_kind = {k: build_rows(rows, k, KINDS[k], index_map) for k in seeded}
    return seeded, per_kind


HEADER = """/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
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

-- AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
-- Generated by tools/ore_conventions/generate_dq_seed.py from
-- conventions-canonical.tsv, which tools/ore_conventions/extract.py writes.
-- Regenerate with:
--   ./projects/ores.codegen/venv/bin/python tools/ore_conventions/generate_dq_seed.py
--
-- The single ore.conventions dataset carries the one canonical instrument
-- convention set. One artefact table per convention kind holds the rows; the
-- publish function ores_refdata_publish_conventions_from_dq_fn resolves each
-- artefact table to its live table by name. Each row's oresmd_uri is the oresmd
-- fixing URI of the ORE index the row names, read from
-- tools/ore_conventions/oresmd_index_map.tsv; a kind that names no index (a
-- requirement) carries null.

"""


def render(seeded, per_kind):
    out = [HEADER]
    out.append("""
-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.conventions',
        'ORE Analytics',
        'Trading',
        'Reference Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'OreStudio Code Generation Methodology',
        'ORE Instrument Conventions',
        'The canonical ORE instrument convention set, extracted from the ORE example corpus under external/ore/examples by tools/ore_conventions/extract.py. One row per canonical (kind, Id).',
        'ORESTUDIO',
        'Seed data for report runs that resolve an ORE convention by id',
        current_date,
        'Internal Use Only',
        'conventions'
    );
END $$;

-- =============================================================================
-- Artefact Seed Data
-- =============================================================================

do $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    -- The live tables take their party from the publish, not from the artefact.
    -- The artefact's party_id is a placeholder for its not-null constraint.
    v_party_id uuid := ores_utility_nil_uuid_fn();
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'ore.conventions'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: ore.conventions';
    end if;

    if exists (
        select 1 from ores_dq_fra_conventions_artefact_tbl
        where dataset_id = v_dataset_id
    ) then
        raise debug 'Conventions artefact already populated for dataset %', v_dataset_id;
        return;
    end if;
""")

    for kind in seeded:
        stem, mapping = KINDS[kind]
        table = f"ores_dq_{stem}_conventions_artefact_tbl"
        party = stem not in WORLD_KINDS
        base = ["dataset_id", "tenant_id", "id", "version"]
        if party:
            base.append("party_id")
        columns = base + [c for _, c, _ in mapping]
        # Columns contributed only by the nested decoder.
        nested_cols = {
            "CommodityFuture": [
                "valid_contract_months", "anchor_day_of_month",
                "anchor_calendar_days_before", "anchor_business_days_after",
                "anchor_nth_nth", "anchor_nth_weekday", "anchor_last_weekday",
                "anchor_weekly_day_of_the_week", "off_peak_index", "peak_index",
                "off_peak_hours", "peak_calendar",
                "averaging_commodity_name", "averaging_period",
                "averaging_pricing_calendar", "averaging_conventions",
                "averaging_use_business_days", "averaging_delivery_roll_days",
                "averaging_future_month_offset", "averaging_daily_expiry_offset",
                "prohibited_expiries", "future_continuation_mappings",
                "option_continuation_mappings",
            ],
            "IntradayPowerLoad": ["explicit_load_profile", "business_day_load_rules"],
        }.get(kind, [])
        for c in nested_cols:
            if c not in columns:
                columns.append(c)
        if "oresmd_uri" not in columns:
            columns.append("oresmd_uri")

        rows = per_kind[kind]
        out.append(f"\n    -- {kind}: {len(rows)} rows\n")
        out.append(f"    insert into {table} (\n        "
                   + ", ".join(columns) + "\n    ) values\n")
        tuples = []
        for _, values, _ in rows:
            cells = [f"v_dataset_id", f"v_tenant_id", values["id"], "1"]
            if party:
                cells.append("v_party_id")
            for c in columns[len(base):]:
                cells.append(values.get(c, "null"))
            tuples.append("        (" + ", ".join(cells) + ")")
        out.append(",\n".join(tuples))
        out.append(";\n")

    out.append("end $$;\n")
    return "".join(out)


def main(argv=None):
    default_tsv = "tmp/ore_conventions/conventions-canonical.tsv"
    default_out = ("projects/ores.sql/populate/refdata/"
                   "refdata_conventions_seed_populate.sql")
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--tsv", default=default_tsv,
                        help=f"canonical TSV (default {default_tsv})")
    parser.add_argument("--map", default=str(DEFAULT_MAP),
                        help=f"ore_name to oresmd URI map (default {DEFAULT_MAP})")
    parser.add_argument("--out", default=default_out,
                        help=f"SQL output (default {default_out})")
    parser.add_argument("--dump-index-names",
                        help="write the sorted index names the canonical set "
                             "references and exit; feed the file to "
                             "'ores.shell marketdata oresmd-index' to rebuild "
                             "the map")
    args = parser.parse_args(argv)

    if args.dump_index_names:
        names = all_referenced_index_names(read_canonical_rows(args.tsv))
        Path(args.dump_index_names).write_text(
            "".join(f"{name}\n" for name in names), encoding="utf-8")
        print(f"wrote {args.dump_index_names}: index names={len(names)}")
        return 0

    try:
        seeded, per_kind = generate(args.tsv, args.map)
    except GenError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1

    total = sum(len(v) for v in per_kind.values())
    Path(args.out).write_text(render(seeded, per_kind), encoding="utf-8")
    print(f"wrote {args.out}: kinds={len(seeded)} rows={total}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
