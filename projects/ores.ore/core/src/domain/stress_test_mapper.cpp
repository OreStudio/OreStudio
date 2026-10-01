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
#include "ores.ore.core/domain/stress_test_mapper.hpp"
#include "ores.ore.core/domain/ore_code_tables.hpp"
#include <cctype>
#include <iomanip>
#include <map>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::ore::domain {

namespace {

bool is_true(domain::bool_ v) {
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

/**
 * Splits on a separator. Empty text yields nothing, so an empty encoding
 * reads back as an empty collection and not as one empty item.
 */
std::vector<std::string> split(const std::string& text, char separator) {
    std::vector<std::string> parts;
    if (text.empty())
        return parts;
    std::size_t start = 0;
    while (true) {
        const auto at = text.find(separator, start);
        if (at == std::string::npos) {
            parts.push_back(text.substr(start));
            break;
        }
        parts.push_back(text.substr(start, at - start));
        start = at + 1;
    }
    return parts;
}

std::string join(const std::vector<std::string>& parts, char separator) {
    std::string out;
    for (std::size_t i = 0; i < parts.size(); ++i) {
        if (i != 0)
            out += separator;
        out += parts[i];
    }
    return out;
}

/**
 * The spelling the binding's own writer gives a float, so that a value written
 * into extras and read back is the value the document carried.
 */
std::string float_text(float v) {
    std::ostringstream stream;
    stream << std::setprecision(9) << v;
    return stream.str();
}

std::string bool_text(bool v) {
    return v ? "true" : "false";
}

bool parse_bool(const std::string& text) {
    std::string lower;
    lower.reserve(text.size());
    for (const char c : text)
        lower += static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
    return lower == "true" || lower == "y" || lower == "yes" || lower == "1";
}

using extras_map = std::map<std::string, std::string>;

/**
 * @brief Collects the entry's non-dedicated fields in the struct's own order.
 *
 * A field is written as name=value and the fields are joined by ';'. A vector
 * is written as its items joined by ',' and a nested struct as its own fields
 * joined by '|', so no value needs quoting: no key, tenor or expiry in the
 * corpus carries any of those three separators. The inherited value of a
 * keyed entry is named value, so shiftSizeEntry is value=0.5|key=3M.
 */
struct extras_builder {
    std::vector<std::pair<std::string, std::string>> fields;

    void add(const std::string& name, const std::string& value) {
        fields.emplace_back(name, value);
    }

    void add_bool(const std::string& name, bool value) {
        fields.emplace_back(name, bool_text(value));
    }

    std::optional<std::string> build() const {
        if (fields.empty())
            return std::nullopt;
        std::string out;
        for (const auto& [name, value] : fields) {
            if (!out.empty())
                out += ';';
            out += name;
            out += '=';
            out += value;
        }
        return out;
    }
};

extras_map decode_extras(const std::optional<std::string>& text) {
    extras_map out;
    if (!text)
        return out;
    for (const auto& piece : split(*text, ';')) {
        const auto at = piece.find('=');
        if (at != std::string::npos)
            out[piece.substr(0, at)] = piece.substr(at + 1);
    }
    return out;
}

std::string encode_shift_type_entry(const domain::shiftTypeEntry& v) {
    std::string out = to_string(static_cast<domain::shiftType>(v));
    if (v.key) {
        out += "|key=";
        out += *v.key;
    }
    return out;
}

domain::shiftTypeEntry decode_shift_type_entry(const std::string& text) {
    domain::shiftTypeEntry out;
    const auto fields = split(text, '|');
    if (!fields.empty())
        static_cast<domain::shiftType&>(out) = parse_shift_type(fields.front());
    for (std::size_t i = 1; i < fields.size(); ++i) {
        const auto at = fields[i].find('=');
        if (at != std::string::npos && fields[i].substr(0, at) == "key")
            out.key = fields[i].substr(at + 1);
    }
    return out;
}

std::string encode_shift_size_entry(const domain::shiftSizeEntry& v) {
    std::string out = "value=";
    out += float_text(static_cast<float>(v));
    if (v.key) {
        out += "|key=";
        out += *v.key;
    }
    return out;
}

domain::shiftSizeEntry decode_shift_size_entry(const std::string& text) {
    domain::shiftSizeEntry out;
    for (const auto& field : split(text, '|')) {
        const auto at = field.find('=');
        if (at == std::string::npos)
            continue;
        const auto name = field.substr(0, at);
        const auto value = field.substr(at + 1);
        if (name == "value")
            static_cast<float&>(out) = std::stof(value);
        else if (name == "key")
            out.key = value;
    }
    return out;
}

std::string encode_shift_scheme_entry(const domain::shiftSchemeEntry& v) {
    std::string out = "value=";
    out += to_string(static_cast<domain::shiftScheme>(v));
    if (v.key) {
        out += "|key=";
        out += *v.key;
    }
    return out;
}

domain::shiftSchemeEntry decode_shift_scheme_entry(const std::string& text) {
    domain::shiftSchemeEntry out;
    for (const auto& field : split(text, '|')) {
        const auto at = field.find('=');
        if (at == std::string::npos)
            continue;
        const auto name = field.substr(0, at);
        const auto value = field.substr(at + 1);
        if (name == "value") {
            if (value == to_string(domain::shiftScheme::Forward))
                static_cast<domain::shiftScheme&>(out) = domain::shiftScheme::Forward;
            else if (value == to_string(domain::shiftScheme::Backward))
                static_cast<domain::shiftScheme&>(out) = domain::shiftScheme::Backward;
            else if (value == to_string(domain::shiftScheme::Central))
                static_cast<domain::shiftScheme&>(out) = domain::shiftScheme::Central;
        } else if (name == "key") {
            out.key = value;
        }
    }
    return out;
}

/**
 * ParConversion keeps every sub-field, with Conventions' own vector of keyed
 * conventions nested in the same name=value form: Instruments=Swap,Deposit,
 * SingleCurve=true,false, Conventions=value=ACT/360|id=one.
 */
std::string encode_parconversion(const domain::parconversion& v) {
    std::vector<std::string> fields;
    if (!v.Instruments.empty()) {
        std::vector<std::string> items;
        for (const auto& instrument : v.Instruments)
            items.push_back(std::string(instrument));
        fields.push_back("Instruments=" + join(items, ','));
    }
    if (!v.SingleCurve.empty()) {
        std::vector<std::string> items;
        for (const auto& single : v.SingleCurve)
            items.push_back(bool_text(single != 0));
        fields.push_back("SingleCurve=" + join(items, ','));
    }
    if (v.DiscountCurve)
        fields.push_back("DiscountCurve=" + std::string(*v.DiscountCurve));
    if (v.OtherCurrency)
        fields.push_back("OtherCurrency=" + to_string(*v.OtherCurrency));
    if (v.RateComputationPeriod)
        fields.push_back("RateComputationPeriod=" + std::string(*v.RateComputationPeriod));
    if (v.Conventions) {
        std::vector<std::string> items;
        for (const auto& convention : v.Conventions->Convention) {
            std::string item = "value=" + std::string(convention);
            if (convention.id) {
                item += "~id=";
                item += *convention.id;
            }
            items.push_back(std::move(item));
        }
        fields.push_back("Conventions=" + join(items, ','));
    }
    return join(fields, '|');
}

domain::parconversion decode_parconversion(const std::string& text) {
    domain::parconversion out;
    for (const auto& field : split(text, '|')) {
        const auto at = field.find('=');
        if (at == std::string::npos)
            continue;
        const auto name = field.substr(0, at);
        const auto value = field.substr(at + 1);
        if (name == "Instruments") {
            for (const auto& item : split(value, ','))
                out.Instruments.push_back(domain::parconversion_Instruments_t(item));
        } else if (name == "SingleCurve") {
            for (const auto& item : split(value, ','))
                out.SingleCurve.push_back(parse_bool(item));
        } else if (name == "DiscountCurve") {
            out.DiscountCurve = domain::parconversion_DiscountCurve_t(value);
        } else if (name == "OtherCurrency") {
            out.OtherCurrency = parse_currency_code(value);
        } else if (name == "RateComputationPeriod") {
            out.RateComputationPeriod = domain::parconversion_RateComputationPeriod_t(value);
        } else if (name == "Conventions") {
            domain::parconversion_Conventions_t conventions;
            for (const auto& item : split(value, ',')) {
                domain::parconversion_Conventions_t_Convention_t convention;
                for (const auto& sub : split(item, '~')) {
                    const auto sub_at = sub.find('=');
                    if (sub_at == std::string::npos)
                        continue;
                    const auto sub_name = sub.substr(0, sub_at);
                    const auto sub_value = sub.substr(sub_at + 1);
                    if (sub_name == "value")
                        static_cast<xsd::string&>(convention) = sub_value;
                    else if (sub_name == "id")
                        convention.id = sub_value;
                }
                conventions.Convention.push_back(std::move(convention));
            }
            out.Conventions = conventions;
        }
    }
    return out;
}

std::string encode_weighted_shifts(const domain::stressfxvolatility_WeightedShifts_t& v) {
    std::vector<std::string> fields;
    if (!v.WeightingSchema.empty())
        fields.push_back("WeightingSchema=" + std::string(v.WeightingSchema));
    if (!v.Shift.empty())
        fields.push_back("Shift=" + std::string(v.Shift));
    if (!v.Tenor.empty())
        fields.push_back("Tenor=" + std::string(v.Tenor));
    if (v.ShiftWeights)
        fields.push_back("ShiftWeights=" + std::string(*v.ShiftWeights));
    if (v.WeightTenors)
        fields.push_back("WeightTenors=" + std::string(*v.WeightTenors));
    return join(fields, '|');
}

domain::stressfxvolatility_WeightedShifts_t
decode_weighted_shifts(const std::string& text) {
    domain::stressfxvolatility_WeightedShifts_t out;
    for (const auto& field : split(text, '|')) {
        const auto at = field.find('=');
        if (at == std::string::npos)
            continue;
        const auto name = field.substr(0, at);
        const auto value = field.substr(at + 1);
        if (name == "WeightingSchema")
            static_cast<xsd::string&>(out.WeightingSchema) = value;
        else if (name == "Shift")
            static_cast<xsd::string&>(out.Shift) = value;
        else if (name == "Tenor")
            static_cast<xsd::string&>(out.Tenor) = value;
        else if (name == "ShiftWeights")
            out.ShiftWeights = domain::stressfxvolatility_WeightedShifts_t_ShiftWeights_t(value);
        else if (name == "WeightTenors")
            out.WeightTenors = domain::stressfxvolatility_WeightedShifts_t_WeightTenors_t(value);
    }
    return out;
}

/**
 * SwaptionVolatility and CapFloorVolatility carry their Shifts as a struct of
 * keyed values rather than as text, so the struct travels in the shifts column:
 * each Shift is value=<text> with its expiry, term or tenor after a '|', and
 * the entries are joined by ','. Everything the entry holds survives.
 */
std::string encode_swaption_shifts(const domain::stressswaptionvolatility_Shifts_t& v) {
    std::vector<std::string> items;
    for (const auto& shift : v.Shift) {
        std::string item = "value=" + std::string(shift);
        if (shift.expiry) {
            item += "|expiry=";
            item += *shift.expiry;
        }
        if (shift.term) {
            item += "|term=";
            item += *shift.term;
        }
        items.push_back(std::move(item));
    }
    return join(items, ',');
}

domain::stressswaptionvolatility_Shifts_t
decode_swaption_shifts(const std::string& text) {
    domain::stressswaptionvolatility_Shifts_t out;
    for (const auto& item : split(text, ',')) {
        domain::stressswaptionvolatility_Shifts_t_Shift_t shift;
        for (const auto& field : split(item, '|')) {
            const auto at = field.find('=');
            if (at == std::string::npos)
                continue;
            const auto name = field.substr(0, at);
            const auto value = field.substr(at + 1);
            if (name == "value")
                static_cast<xsd::string&>(shift) = value;
            else if (name == "expiry")
                shift.expiry = value;
            else if (name == "term")
                shift.term = value;
        }
        out.Shift.push_back(std::move(shift));
    }
    return out;
}

std::string encode_cap_floor_shifts(const domain::stresscapfloorvolatility_Shifts_t& v) {
    std::vector<std::string> items;
    for (const auto& shift : v.Shift) {
        std::string item = "value=" + std::string(shift);
        if (shift.tenor) {
            item += "|tenor=";
            item += *shift.tenor;
        }
        items.push_back(std::move(item));
    }
    return join(items, ',');
}

domain::stresscapfloorvolatility_Shifts_t
decode_cap_floor_shifts(const std::string& text) {
    domain::stresscapfloorvolatility_Shifts_t out;
    for (const auto& item : split(text, ',')) {
        domain::stresscapfloorvolatility_Shifts_t_Shift_t shift;
        for (const auto& field : split(item, '|')) {
            const auto at = field.find('=');
            if (at == std::string::npos)
                continue;
            const auto name = field.substr(0, at);
            const auto value = field.substr(at + 1);
            if (name == "value")
                static_cast<xsd::string&>(shift) = value;
            else if (name == "tenor")
                shift.tenor = value;
        }
        out.Shift.push_back(std::move(shift));
    }
    return out;
}

/**
 * The first ShiftType is the shift_type column. Any further entries, and the
 * first entry's own key attribute, are named in extras so nothing is lost.
 */
void map_shift_types(const xsd::vector<domain::shiftTypeEntry>& values,
                     analytics::domain::stress_test_shift& shift,
                     extras_builder& extras) {
    if (values.empty())
        return;
    shift.shift_type = to_string(static_cast<domain::shiftType>(values.front()));
    if (values.front().key)
        extras.add("ShiftTypeKey", *values.front().key);
    if (values.size() > 1) {
        std::vector<std::string> rest;
        for (std::size_t i = 1; i < values.size(); ++i)
            rest.push_back(encode_shift_type_entry(values[i]));
        extras.add("ExtraShiftTypes", join(rest, ','));
    }
}

void reverse_shift_types(const analytics::domain::stress_test_shift& shift,
                         const extras_map& extras,
                         xsd::vector<domain::shiftTypeEntry>& values) {
    if (shift.shift_type) {
        domain::shiftTypeEntry entry;
        static_cast<domain::shiftType&>(entry) = parse_shift_type(*shift.shift_type);
        const auto key = extras.find("ShiftTypeKey");
        if (key != extras.end())
            entry.key = key->second;
        values.push_back(std::move(entry));
    }
    const auto extra = extras.find("ExtraShiftTypes");
    if (extra != extras.end()) {
        for (const auto& item : split(extra->second, ','))
            values.push_back(decode_shift_type_entry(item));
    }
}

void add_shift_size(const xsd::vector<domain::shiftSizeEntry>& values, extras_builder& extras) {
    std::vector<std::string> items;
    for (const auto& value : values)
        items.push_back(encode_shift_size_entry(value));
    if (!items.empty())
        extras.add("ShiftSize", join(items, ','));
}

void reverse_shift_size(const extras_map& extras, xsd::vector<domain::shiftSizeEntry>& values) {
    const auto it = extras.find("ShiftSize");
    if (it == extras.end())
        return;
    for (const auto& item : split(it->second, ','))
        values.push_back(decode_shift_size_entry(item));
}

void add_shift_scheme(const xsd::vector<domain::shiftSchemeEntry>& values,
                      extras_builder& extras) {
    std::vector<std::string> items;
    for (const auto& value : values)
        items.push_back(encode_shift_scheme_entry(value));
    if (!items.empty())
        extras.add("ShiftScheme", join(items, ','));
}

void reverse_shift_scheme(const extras_map& extras,
                          xsd::vector<domain::shiftSchemeEntry>& values) {
    const auto it = extras.find("ShiftScheme");
    if (it == extras.end())
        return;
    for (const auto& item : split(it->second, ','))
        values.push_back(decode_shift_scheme_entry(item));
}

/**
 * SwaptionVolatility and CapFloorVolatility name their object by ccy and key,
 * both optional, so the present parts are joined by '|' in that order.
 */
std::string encode_composite_key(const std::optional<std::string>& ccy,
                                 const std::optional<std::string>& key) {
    if (ccy && key)
        return *ccy + "|" + *key;
    if (ccy)
        return *ccy;
    if (key)
        return "|" + *key;
    return {};
}

/**
 * The leading separator marks a key with no currency. Without it a lone part
 * would have to be guessed from the currency vocabulary, and a key that spells
 * a currency would be silently read as the currency and lost.
 */
void decode_composite_key(const std::string& object_key,
                          std::optional<std::string>& ccy,
                          std::optional<std::string>& key) {
    if (object_key.empty())
        return;
    const auto at = object_key.find('|');
    if (at == std::string::npos) {
        ccy = object_key;
        return;
    }
    if (at > 0)
        ccy = object_key.substr(0, at);
    key = object_key.substr(at + 1);
}

analytics::domain::stress_test_shift
map_discount_curve(const stressdiscountcurve& entry, int position) {
    analytics::domain::stress_test_shift shift;
    shift.family = "DiscountCurves";
    shift.object_key = to_string(entry.ccy);
    shift.position = position;
    extras_builder extras;
    map_shift_types(entry.ShiftType, shift, extras);
    if (entry.ShiftingZeros)
        extras.add_bool("ShiftingZeros", *entry.ShiftingZeros);
    add_shift_size(entry.ShiftSize, extras);
    if (entry.Shifts)
        shift.shifts = std::string(*entry.Shifts);
    add_shift_scheme(entry.ShiftScheme, extras);
    shift.shift_tenors = std::string(entry.ShiftTenors);
    if (entry.ParConversion)
        extras.add("ParConversion", encode_parconversion(*entry.ParConversion));
    shift.extras = extras.build();
    return shift;
}

analytics::domain::stress_test_shift
map_index_curve(const stressindexcurve& entry, int position) {
    analytics::domain::stress_test_shift shift;
    shift.family = "IndexCurves";
    shift.object_key = std::string(entry.index);
    shift.position = position;
    extras_builder extras;
    map_shift_types(entry.ShiftType, shift, extras);
    if (entry.ShiftingZeros)
        extras.add_bool("ShiftingZeros", *entry.ShiftingZeros);
    add_shift_size(entry.ShiftSize, extras);
    if (entry.Shifts)
        shift.shifts = std::string(*entry.Shifts);
    add_shift_scheme(entry.ShiftScheme, extras);
    shift.shift_tenors = std::string(entry.ShiftTenors);
    if (entry.ParConversion)
        extras.add("ParConversion", encode_parconversion(*entry.ParConversion));
    shift.extras = extras.build();
    return shift;
}

analytics::domain::stress_test_shift
map_yield_curve(const stressyieldcurve& entry, int position) {
    analytics::domain::stress_test_shift shift;
    shift.family = "YieldCurves";
    shift.object_key = std::string(entry.name);
    shift.position = position;
    extras_builder extras;
    if (entry.CurveType)
        extras.add("CurveType", std::string(*entry.CurveType));
    map_shift_types(entry.ShiftType, shift, extras);
    if (entry.ShiftingZeros)
        extras.add_bool("ShiftingZeros", *entry.ShiftingZeros);
    add_shift_size(entry.ShiftSize, extras);
    if (entry.Shifts)
        shift.shifts = std::string(*entry.Shifts);
    add_shift_scheme(entry.ShiftScheme, extras);
    shift.shift_tenors = std::string(entry.ShiftTenors);
    if (entry.ParConversion)
        extras.add("ParConversion", encode_parconversion(*entry.ParConversion));
    shift.extras = extras.build();
    return shift;
}

analytics::domain::stress_test_shift map_fx_spot(const fxspot& entry, int position) {
    analytics::domain::stress_test_shift shift;
    shift.family = "FxSpots";
    shift.object_key = std::string(entry.ccypair);
    shift.position = position;
    extras_builder extras;
    map_shift_types(entry.ShiftType, shift, extras);
    add_shift_size(entry.ShiftSize, extras);
    if (entry.Shifts)
        shift.shifts = std::string(*entry.Shifts);
    add_shift_scheme(entry.ShiftScheme, extras);
    shift.extras = extras.build();
    return shift;
}

analytics::domain::stress_test_shift
map_fx_volatility(const stressfxvolatility& entry, int position) {
    analytics::domain::stress_test_shift shift;
    shift.family = "FxVolatilities";
    shift.object_key = std::string(entry.ccypair);
    shift.position = position;
    extras_builder extras;
    map_shift_types(entry.ShiftType, shift, extras);
    if (entry.Shifts)
        shift.shifts = std::string(*entry.Shifts);
    if (entry.ShiftExpiries)
        shift.shift_expiries = std::string(*entry.ShiftExpiries);
    if (entry.WeightedShifts)
        extras.add("WeightedShifts", encode_weighted_shifts(*entry.WeightedShifts));
    shift.extras = extras.build();
    return shift;
}

analytics::domain::stress_test_shift
map_swaption_volatility(const stressswaptionvolatility& entry, int position) {
    analytics::domain::stress_test_shift shift;
    shift.family = "SwaptionVolatilities";
    std::optional<std::string> ccy;
    if (entry.ccy)
        ccy = *entry.ccy;
    std::optional<std::string> key;
    if (entry.key)
        key = *entry.key;
    shift.object_key = encode_composite_key(ccy, key);
    shift.position = position;
    extras_builder extras;
    shift.shift_type = to_string(static_cast<domain::shiftType>(entry.ShiftType));
    if (entry.ShiftType.key)
        extras.add("ShiftTypeKey", *entry.ShiftType.key);
    shift.shifts = encode_swaption_shifts(entry.Shifts);
    shift.shift_expiries = std::string(entry.ShiftExpiries);
    if (!entry.ShiftTerms.empty())
        extras.add("ShiftTerms", std::string(entry.ShiftTerms));
    shift.extras = extras.build();
    return shift;
}

analytics::domain::stress_test_shift
map_cap_floor_volatility(const stresscapfloorvolatility& entry, int position) {
    analytics::domain::stress_test_shift shift;
    shift.family = "CapFloorVolatilities";
    std::optional<std::string> ccy;
    if (entry.ccy)
        ccy = to_string(*entry.ccy);
    std::optional<std::string> key;
    if (entry.key)
        key = *entry.key;
    shift.object_key = encode_composite_key(ccy, key);
    shift.position = position;
    extras_builder extras;
    map_shift_types(entry.ShiftType, shift, extras);
    shift.shifts = encode_cap_floor_shifts(entry.Shifts);
    shift.shift_expiries = std::string(entry.ShiftExpiries);
    if (entry.ShiftStrikes)
        extras.add("ShiftStrikes", std::string(*entry.ShiftStrikes));
    if (entry.Index)
        extras.add("Index", std::string(*entry.Index));
    if (entry.IsRelative)
        extras.add_bool("IsRelative", *entry.IsRelative);
    shift.extras = extras.build();
    return shift;
}

analytics::domain::stress_test_shift map_equity_spot(const equityspot& entry, int position) {
    analytics::domain::stress_test_shift shift;
    shift.family = "EquitySpots";
    shift.object_key = std::string(entry.equity);
    shift.position = position;
    extras_builder extras;
    map_shift_types(entry.ShiftType, shift, extras);
    add_shift_size(entry.ShiftSize, extras);
    if (entry.Shifts)
        shift.shifts = std::string(*entry.Shifts);
    add_shift_scheme(entry.ShiftScheme, extras);
    shift.extras = extras.build();
    return shift;
}

analytics::domain::stress_test_shift
map_equity_volatility(const equityvolatility& entry, int position) {
    analytics::domain::stress_test_shift shift;
    shift.family = "EquityVolatilities";
    shift.object_key = std::string(entry.equity);
    shift.position = position;
    extras_builder extras;
    map_shift_types(entry.ShiftType, shift, extras);
    add_shift_size(entry.ShiftSize, extras);
    if (entry.Shifts)
        shift.shifts = std::string(*entry.Shifts);
    add_shift_scheme(entry.ShiftScheme, extras);
    shift.shift_expiries = std::string(entry.ShiftExpiries);
    if (entry.ShiftStrikes)
        extras.add("ShiftStrikes", std::string(*entry.ShiftStrikes));
    shift.extras = extras.build();
    return shift;
}

analytics::domain::stress_test_shift
map_commodity_curve(const stresscommoditycurve& entry, int position) {
    analytics::domain::stress_test_shift shift;
    shift.family = "CommodityCurves";
    shift.object_key = std::string(entry.commodity);
    shift.position = position;
    extras_builder extras;
    extras.add("Currency", to_string(entry.Currency));
    map_shift_types(entry.ShiftType, shift, extras);
    shift.shifts = std::string(entry.Shifts);
    shift.shift_tenors = std::string(entry.ShiftTenors);
    shift.extras = extras.build();
    return shift;
}

analytics::domain::stress_test_shift
map_intraday_power_curve(const stressintradaypowercurve& entry, int position) {
    analytics::domain::stress_test_shift shift;
    shift.family = "IntradayPowerCurves";
    shift.object_key = std::string(entry.name);
    shift.position = position;
    extras_builder extras;
    map_shift_types(entry.ShiftType, shift, extras);
    shift.shifts = std::string(entry.Shifts);
    shift.shift_tenors = std::string(entry.ShiftTenors);
    shift.extras = extras.build();
    return shift;
}

analytics::domain::stress_test_shift
map_commodity_volatility(const stresscommodityvolatility& entry, int position) {
    analytics::domain::stress_test_shift shift;
    shift.family = "CommodityVolatilities";
    shift.object_key = std::string(entry.commodity);
    shift.position = position;
    extras_builder extras;
    map_shift_types(entry.ShiftType, shift, extras);
    shift.shifts = std::string(entry.Shifts);
    shift.shift_expiries = std::string(entry.ShiftExpiries);
    if (!entry.ShiftMoneyness.empty())
        extras.add("ShiftMoneyness", std::string(entry.ShiftMoneyness));
    shift.extras = extras.build();
    return shift;
}

analytics::domain::stress_test_shift map_security_spread(const securityspread& entry, int position) {
    analytics::domain::stress_test_shift shift;
    shift.family = "SecuritySpreads";
    shift.object_key = std::string(entry.security);
    shift.position = position;
    extras_builder extras;
    map_shift_types(entry.ShiftType, shift, extras);
    add_shift_size(entry.ShiftSize, extras);
    if (entry.Shifts)
        shift.shifts = std::string(*entry.Shifts);
    add_shift_scheme(entry.ShiftScheme, extras);
    shift.extras = extras.build();
    return shift;
}

analytics::domain::stress_test_shift map_recovery_rate(const recoveryrate& entry, int position) {
    analytics::domain::stress_test_shift shift;
    shift.family = "RecoveryRates";
    shift.object_key = std::string(entry.name);
    shift.position = position;
    extras_builder extras;
    map_shift_types(entry.ShiftType, shift, extras);
    add_shift_size(entry.ShiftSize, extras);
    if (entry.Shifts)
        shift.shifts = std::string(*entry.Shifts);
    add_shift_scheme(entry.ShiftScheme, extras);
    shift.extras = extras.build();
    return shift;
}

analytics::domain::stress_test_shift
map_survival_probability(const survivalprobability& entry, int position) {
    analytics::domain::stress_test_shift shift;
    shift.family = "SurvivalProbabilities";
    shift.object_key = std::string(entry.name);
    shift.position = position;
    extras_builder extras;
    map_shift_types(entry.ShiftType, shift, extras);
    add_shift_size(entry.ShiftSize, extras);
    if (entry.Shifts)
        shift.shifts = std::string(*entry.Shifts);
    add_shift_scheme(entry.ShiftScheme, extras);
    shift.shift_tenors = std::string(entry.ShiftTenors);
    if (entry.ParConversion)
        extras.add("ParConversion", encode_parconversion(*entry.ParConversion));
    shift.extras = extras.build();
    return shift;
}

analytics::domain::stress_test_shift map_par_shifts(const stresstestparshifts& entry) {
    analytics::domain::stress_test_shift shift;
    shift.family = "ParShifts";
    shift.object_key = "ParShifts";
    shift.position = 1;
    extras_builder extras;
    if (entry.IRCurves)
        extras.add_bool("IRCurves", is_true(*entry.IRCurves));
    if (entry.CapFloorVolatilities)
        extras.add_bool("CapFloorVolatilities", is_true(*entry.CapFloorVolatilities));
    if (entry.SurvivalProbability)
        extras.add_bool("SurvivalProbability", is_true(*entry.SurvivalProbability));
    shift.extras = extras.build();
    return shift;
}

}

mapped_stress_test stress_test_mapper::map(const stresstesting& v) {
    mapped_stress_test r;

    if (v.UseSpreadedTermStructures)
        r.library.use_spreaded_term_structures = is_true(*v.UseSpreadedTermStructures);

    int position = 0;
    for (const auto& scenario : v.StressTest) {
        ++position;

        mapped_stress_scenario mapped_scenario;
        mapped_scenario.scenario.name = scenario.id;
        mapped_scenario.scenario.position = position;
        if (scenario.Date)
            mapped_scenario.scenario.date = std::string(*scenario.Date);

        if (scenario.ParShifts)
            mapped_scenario.shifts.push_back(map_par_shifts(*scenario.ParShifts));

        if (scenario.DiscountCurves) {
            int entry_position = 0;
            for (const auto& entry : scenario.DiscountCurves->DiscountCurve)
                mapped_scenario.shifts.push_back(map_discount_curve(entry, ++entry_position));
        }

        if (scenario.IndexCurves) {
            int entry_position = 0;
            for (const auto& entry : scenario.IndexCurves->IndexCurve)
                mapped_scenario.shifts.push_back(map_index_curve(entry, ++entry_position));
        }

        if (scenario.YieldCurves) {
            int entry_position = 0;
            for (const auto& entry : scenario.YieldCurves->YieldCurve)
                mapped_scenario.shifts.push_back(map_yield_curve(entry, ++entry_position));
        }

        if (scenario.FxSpots) {
            int entry_position = 0;
            for (const auto& entry : scenario.FxSpots->FxSpot)
                mapped_scenario.shifts.push_back(map_fx_spot(entry, ++entry_position));
        }

        if (scenario.FxVolatilities) {
            int entry_position = 0;
            for (const auto& entry : scenario.FxVolatilities->FxVolatility)
                mapped_scenario.shifts.push_back(map_fx_volatility(entry, ++entry_position));
        }

        if (scenario.SwaptionVolatilities) {
            int entry_position = 0;
            for (const auto& entry : scenario.SwaptionVolatilities->SwaptionVolatility)
                mapped_scenario.shifts.push_back(
                    map_swaption_volatility(entry, ++entry_position));
        }

        if (scenario.CapFloorVolatilities) {
            int entry_position = 0;
            for (const auto& entry : scenario.CapFloorVolatilities->CapFloorVolatility)
                mapped_scenario.shifts.push_back(
                    map_cap_floor_volatility(entry, ++entry_position));
        }

        if (scenario.EquitySpots) {
            int entry_position = 0;
            for (const auto& entry : scenario.EquitySpots->EquitySpot)
                mapped_scenario.shifts.push_back(map_equity_spot(entry, ++entry_position));
        }

        if (scenario.EquityVolatilities) {
            int entry_position = 0;
            for (const auto& entry : scenario.EquityVolatilities->EquityVolatility)
                mapped_scenario.shifts.push_back(map_equity_volatility(entry, ++entry_position));
        }

        if (scenario.CommodityCurves) {
            int entry_position = 0;
            for (const auto& entry : scenario.CommodityCurves->CommodityCurve)
                mapped_scenario.shifts.push_back(map_commodity_curve(entry, ++entry_position));
        }

        if (scenario.IntradayPowerCurves) {
            int entry_position = 0;
            for (const auto& entry : scenario.IntradayPowerCurves->IntradayPowerCurve)
                mapped_scenario.shifts.push_back(
                    map_intraday_power_curve(entry, ++entry_position));
        }

        if (scenario.CommodityVolatilities) {
            int entry_position = 0;
            for (const auto& entry : scenario.CommodityVolatilities->CommodityVolatility)
                mapped_scenario.shifts.push_back(
                    map_commodity_volatility(entry, ++entry_position));
        }

        if (scenario.SecuritySpreads) {
            int entry_position = 0;
            for (const auto& entry : scenario.SecuritySpreads->SecuritySpread)
                mapped_scenario.shifts.push_back(map_security_spread(entry, ++entry_position));
        }

        if (scenario.RecoveryRates) {
            int entry_position = 0;
            for (const auto& entry : scenario.RecoveryRates->RecoverRate)
                mapped_scenario.shifts.push_back(map_recovery_rate(entry, ++entry_position));
        }

        if (scenario.SurvivalProbabilities) {
            int entry_position = 0;
            for (const auto& entry : scenario.SurvivalProbabilities->SurvivalProbability)
                mapped_scenario.shifts.push_back(
                    map_survival_probability(entry, ++entry_position));
        }

        r.scenarios.push_back(std::move(mapped_scenario));
    }

    return r;
}

stresstesting stress_test_mapper::reverse(const mapped_stress_test& v) {
    stresstesting r;

    if (v.library.use_spreaded_term_structures)
        r.UseSpreadedTermStructures =
            *v.library.use_spreaded_term_structures ? domain::bool_::true_ : domain::bool_::false_;

    for (const auto& row : v.scenarios) {
        stresstest scenario;
        scenario.id = row.scenario.name;
        if (row.scenario.date)
            scenario.Date = stresstest_Date_t(*row.scenario.date);

        for (const auto& shift : row.shifts) {
            const auto extras = decode_extras(shift.extras);

            if (shift.family == "ParShifts") {
                stresstestparshifts par;
                const auto ir = extras.find("IRCurves");
                if (ir != extras.end())
                    par.IRCurves =
                        parse_bool(ir->second) ? domain::bool_::true_ : domain::bool_::false_;
                const auto cap = extras.find("CapFloorVolatilities");
                if (cap != extras.end())
                    par.CapFloorVolatilities =
                        parse_bool(cap->second) ? domain::bool_::true_ : domain::bool_::false_;
                const auto survival = extras.find("SurvivalProbability");
                if (survival != extras.end())
                    par.SurvivalProbability =
                        parse_bool(survival->second) ? domain::bool_::true_
                                                     : domain::bool_::false_;
                scenario.ParShifts = par;
            } else if (shift.family == "DiscountCurves") {
                stressdiscountcurve entry;
                entry.ccy = parse_currency_code(shift.object_key);
                reverse_shift_types(shift, extras, entry.ShiftType);
                const auto zeros = extras.find("ShiftingZeros");
                if (zeros != extras.end())
                    entry.ShiftingZeros = parse_bool(zeros->second);
                reverse_shift_size(extras, entry.ShiftSize);
                if (shift.shifts)
                    entry.Shifts = stressdiscountcurve_Shifts_t(*shift.shifts);
                reverse_shift_scheme(extras, entry.ShiftScheme);
                if (shift.shift_tenors)
                    entry.ShiftTenors = stressdiscountcurve_ShiftTenors_t(*shift.shift_tenors);
                const auto conversion = extras.find("ParConversion");
                if (conversion != extras.end())
                    entry.ParConversion = decode_parconversion(conversion->second);
                if (!scenario.DiscountCurves)
                    scenario.DiscountCurves = stressdiscountcurves{};
                scenario.DiscountCurves->DiscountCurve.push_back(std::move(entry));
            } else if (shift.family == "IndexCurves") {
                stressindexcurve entry;
                entry.index = shift.object_key;
                reverse_shift_types(shift, extras, entry.ShiftType);
                const auto zeros = extras.find("ShiftingZeros");
                if (zeros != extras.end())
                    entry.ShiftingZeros = parse_bool(zeros->second);
                reverse_shift_size(extras, entry.ShiftSize);
                if (shift.shifts)
                    entry.Shifts = stressindexcurve_Shifts_t(*shift.shifts);
                reverse_shift_scheme(extras, entry.ShiftScheme);
                if (shift.shift_tenors)
                    entry.ShiftTenors = stressindexcurve_ShiftTenors_t(*shift.shift_tenors);
                const auto conversion = extras.find("ParConversion");
                if (conversion != extras.end())
                    entry.ParConversion = decode_parconversion(conversion->second);
                if (!scenario.IndexCurves)
                    scenario.IndexCurves = stressindexcurves{};
                scenario.IndexCurves->IndexCurve.push_back(std::move(entry));
            } else if (shift.family == "YieldCurves") {
                stressyieldcurve entry;
                entry.name = shift.object_key;
                const auto curve = extras.find("CurveType");
                if (curve != extras.end())
                    entry.CurveType = stressyieldcurve_CurveType_t(curve->second);
                reverse_shift_types(shift, extras, entry.ShiftType);
                const auto zeros = extras.find("ShiftingZeros");
                if (zeros != extras.end())
                    entry.ShiftingZeros = parse_bool(zeros->second);
                reverse_shift_size(extras, entry.ShiftSize);
                if (shift.shifts)
                    entry.Shifts = stressyieldcurve_Shifts_t(*shift.shifts);
                reverse_shift_scheme(extras, entry.ShiftScheme);
                if (shift.shift_tenors)
                    entry.ShiftTenors = stressyieldcurve_ShiftTenors_t(*shift.shift_tenors);
                const auto conversion = extras.find("ParConversion");
                if (conversion != extras.end())
                    entry.ParConversion = decode_parconversion(conversion->second);
                if (!scenario.YieldCurves)
                    scenario.YieldCurves = stressyieldcurves{};
                scenario.YieldCurves->YieldCurve.push_back(std::move(entry));
            } else if (shift.family == "FxSpots") {
                fxspot entry;
                entry.ccypair = shift.object_key;
                reverse_shift_types(shift, extras, entry.ShiftType);
                reverse_shift_size(extras, entry.ShiftSize);
                if (shift.shifts)
                    entry.Shifts = fxspot_Shifts_t(*shift.shifts);
                reverse_shift_scheme(extras, entry.ShiftScheme);
                if (!scenario.FxSpots)
                    scenario.FxSpots = fxspots{};
                scenario.FxSpots->FxSpot.push_back(std::move(entry));
            } else if (shift.family == "FxVolatilities") {
                stressfxvolatility entry;
                entry.ccypair = shift.object_key;
                reverse_shift_types(shift, extras, entry.ShiftType);
                if (shift.shifts)
                    entry.Shifts = stressfxvolatility_Shifts_t(*shift.shifts);
                if (shift.shift_expiries)
                    entry.ShiftExpiries = stressfxvolatility_ShiftExpiries_t(*shift.shift_expiries);
                const auto weighted = extras.find("WeightedShifts");
                if (weighted != extras.end())
                    entry.WeightedShifts = decode_weighted_shifts(weighted->second);
                if (!scenario.FxVolatilities)
                    scenario.FxVolatilities = stressfxvolatilities{};
                scenario.FxVolatilities->FxVolatility.push_back(std::move(entry));
            } else if (shift.family == "SwaptionVolatilities") {
                stressswaptionvolatility entry;
                std::optional<std::string> ccy;
                std::optional<std::string> key;
                decode_composite_key(shift.object_key, ccy, key);
                if (ccy)
                    entry.ccy = *ccy;
                if (key)
                    entry.key = *key;
                if (shift.shift_type) {
                    static_cast<domain::shiftType&>(entry.ShiftType) =
                        parse_shift_type(*shift.shift_type);
                    const auto type_key = extras.find("ShiftTypeKey");
                    if (type_key != extras.end())
                        entry.ShiftType.key = type_key->second;
                }
                if (shift.shifts)
                    entry.Shifts = decode_swaption_shifts(*shift.shifts);
                if (shift.shift_expiries)
                    entry.ShiftExpiries =
                        stressswaptionvolatility_ShiftExpiries_t(*shift.shift_expiries);
                const auto terms = extras.find("ShiftTerms");
                if (terms != extras.end())
                    entry.ShiftTerms = stressswaptionvolatility_ShiftTerms_t(terms->second);
                if (!scenario.SwaptionVolatilities)
                    scenario.SwaptionVolatilities = stressswaptionvolatilities{};
                scenario.SwaptionVolatilities->SwaptionVolatility.push_back(std::move(entry));
            } else if (shift.family == "CapFloorVolatilities") {
                stresscapfloorvolatility entry;
                std::optional<std::string> ccy;
                std::optional<std::string> key;
                decode_composite_key(shift.object_key, ccy, key);
                if (ccy)
                    entry.ccy = parse_currency_code(*ccy);
                if (key)
                    entry.key = *key;
                reverse_shift_types(shift, extras, entry.ShiftType);
                if (shift.shifts)
                    entry.Shifts = decode_cap_floor_shifts(*shift.shifts);
                if (shift.shift_expiries)
                    entry.ShiftExpiries =
                        stresscapfloorvolatility_ShiftExpiries_t(*shift.shift_expiries);
                const auto strikes = extras.find("ShiftStrikes");
                if (strikes != extras.end())
                    entry.ShiftStrikes =
                        stresscapfloorvolatility_ShiftStrikes_t(strikes->second);
                const auto index = extras.find("Index");
                if (index != extras.end())
                    entry.Index = index->second;
                const auto relative = extras.find("IsRelative");
                if (relative != extras.end())
                    entry.IsRelative = parse_bool(relative->second);
                if (!scenario.CapFloorVolatilities)
                    scenario.CapFloorVolatilities = stresscapfloorvolatilities{};
                scenario.CapFloorVolatilities->CapFloorVolatility.push_back(std::move(entry));
            } else if (shift.family == "EquitySpots") {
                equityspot entry;
                entry.equity = shift.object_key;
                reverse_shift_types(shift, extras, entry.ShiftType);
                reverse_shift_size(extras, entry.ShiftSize);
                if (shift.shifts)
                    entry.Shifts = equityspot_Shifts_t(*shift.shifts);
                reverse_shift_scheme(extras, entry.ShiftScheme);
                if (!scenario.EquitySpots)
                    scenario.EquitySpots = equityspots{};
                scenario.EquitySpots->EquitySpot.push_back(std::move(entry));
            } else if (shift.family == "EquityVolatilities") {
                equityvolatility entry;
                entry.equity = shift.object_key;
                reverse_shift_types(shift, extras, entry.ShiftType);
                reverse_shift_size(extras, entry.ShiftSize);
                if (shift.shifts)
                    entry.Shifts = equityvolatility_Shifts_t(*shift.shifts);
                reverse_shift_scheme(extras, entry.ShiftScheme);
                if (shift.shift_expiries)
                    entry.ShiftExpiries = equityvolatility_ShiftExpiries_t(*shift.shift_expiries);
                const auto strikes = extras.find("ShiftStrikes");
                if (strikes != extras.end())
                    entry.ShiftStrikes = equityvolatility_ShiftStrikes_t(strikes->second);
                if (!scenario.EquityVolatilities)
                    scenario.EquityVolatilities = equityvolatilities{};
                scenario.EquityVolatilities->EquityVolatility.push_back(std::move(entry));
            } else if (shift.family == "CommodityCurves") {
                stresscommoditycurve entry;
                entry.commodity = shift.object_key;
                const auto currency = extras.find("Currency");
                if (currency != extras.end())
                    entry.Currency = parse_currency_code(currency->second);
                reverse_shift_types(shift, extras, entry.ShiftType);
                if (shift.shifts)
                    entry.Shifts = stresscommoditycurve_Shifts_t(*shift.shifts);
                if (shift.shift_tenors)
                    entry.ShiftTenors = stresscommoditycurve_ShiftTenors_t(*shift.shift_tenors);
                if (!scenario.CommodityCurves)
                    scenario.CommodityCurves = stresscommoditycurves{};
                scenario.CommodityCurves->CommodityCurve.push_back(std::move(entry));
            } else if (shift.family == "IntradayPowerCurves") {
                stressintradaypowercurve entry;
                entry.name = shift.object_key;
                reverse_shift_types(shift, extras, entry.ShiftType);
                if (shift.shifts)
                    entry.Shifts = stressintradaypowercurve_Shifts_t(*shift.shifts);
                if (shift.shift_tenors)
                    entry.ShiftTenors =
                        stressintradaypowercurve_ShiftTenors_t(*shift.shift_tenors);
                if (!scenario.IntradayPowerCurves)
                    scenario.IntradayPowerCurves = stressintradaypowercurves{};
                scenario.IntradayPowerCurves->IntradayPowerCurve.push_back(std::move(entry));
            } else if (shift.family == "CommodityVolatilities") {
                stresscommodityvolatility entry;
                entry.commodity = shift.object_key;
                reverse_shift_types(shift, extras, entry.ShiftType);
                if (shift.shifts)
                    entry.Shifts = stresscommodityvolatility_Shifts_t(*shift.shifts);
                if (shift.shift_expiries)
                    entry.ShiftExpiries =
                        stresscommodityvolatility_ShiftExpiries_t(*shift.shift_expiries);
                const auto moneyness = extras.find("ShiftMoneyness");
                if (moneyness != extras.end())
                    entry.ShiftMoneyness =
                        stresscommodityvolatility_ShiftMoneyness_t(moneyness->second);
                if (!scenario.CommodityVolatilities)
                    scenario.CommodityVolatilities = stresscommodityvolatilities{};
                scenario.CommodityVolatilities->CommodityVolatility.push_back(std::move(entry));
            } else if (shift.family == "SecuritySpreads") {
                securityspread entry;
                entry.security = shift.object_key;
                reverse_shift_types(shift, extras, entry.ShiftType);
                reverse_shift_size(extras, entry.ShiftSize);
                if (shift.shifts)
                    entry.Shifts = securityspread_Shifts_t(*shift.shifts);
                reverse_shift_scheme(extras, entry.ShiftScheme);
                if (!scenario.SecuritySpreads)
                    scenario.SecuritySpreads = securityspreads{};
                scenario.SecuritySpreads->SecuritySpread.push_back(std::move(entry));
            } else if (shift.family == "RecoveryRates") {
                recoveryrate entry;
                entry.name = shift.object_key;
                reverse_shift_types(shift, extras, entry.ShiftType);
                reverse_shift_size(extras, entry.ShiftSize);
                if (shift.shifts)
                    entry.Shifts = recoveryrate_Shifts_t(*shift.shifts);
                reverse_shift_scheme(extras, entry.ShiftScheme);
                if (!scenario.RecoveryRates)
                    scenario.RecoveryRates = recoveryrates{};
                scenario.RecoveryRates->RecoverRate.push_back(std::move(entry));
            } else if (shift.family == "SurvivalProbabilities") {
                survivalprobability entry;
                entry.name = shift.object_key;
                reverse_shift_types(shift, extras, entry.ShiftType);
                reverse_shift_size(extras, entry.ShiftSize);
                if (shift.shifts)
                    entry.Shifts = survivalprobability_Shifts_t(*shift.shifts);
                reverse_shift_scheme(extras, entry.ShiftScheme);
                if (shift.shift_tenors)
                    entry.ShiftTenors = survivalprobability_ShiftTenors_t(*shift.shift_tenors);
                const auto conversion = extras.find("ParConversion");
                if (conversion != extras.end())
                    entry.ParConversion = decode_parconversion(conversion->second);
                if (!scenario.SurvivalProbabilities)
                    scenario.SurvivalProbabilities = survivalprobabilities{};
                scenario.SurvivalProbabilities->SurvivalProbability.push_back(std::move(entry));
            }
        }

        r.StressTest.push_back(std::move(scenario));
    }

    return r;
}

}
