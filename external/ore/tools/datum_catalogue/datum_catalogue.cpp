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
#include <ql/time/date.hpp>
#include <boost/core/demangle.hpp>
#include <boost/optional.hpp>
#include <boost/variant.hpp>
#include <format>
#include <iostream>
#include <ored/marketdata/expiry.hpp>
#include <ored/marketdata/marketdatum.hpp>
#include <ored/marketdata/marketdatumparser.hpp>
#include <ored/marketdata/strike.hpp>
#include <ored/utilities/to_string.hpp>
#include <string>
#include <typeinfo>
#include <utility>
#include <vector>

/**
 * @file datum_catalogue.cpp
 * @brief Reads ORE market data keys through ORE's own parser and prints what it
 * made of each one, as JSON lines.
 *
 * One key per input line; blank lines and lines starting with '#' are skipped.
 * For a key ORE accepts, the line names the instrument type, the quote type, the
 * datum class and every inspector that class declares. For a key ORE refuses,
 * the line carries ORE's own error. ORE Studio's key codec is tested against this
 * output field by field.
 */

namespace {

using field_list = std::vector<std::pair<std::string, std::string>>;

// Declared together so that each template sees every overload, whatever order the
// definitions below come in.
std::string fmt(const std::string& v);
std::string fmt(bool v);
std::string fmt(int v);
std::string fmt(QuantLib::Size v);
std::string fmt(QuantLib::Real v);
std::string fmt(const QuantLib::Date& v);
std::string fmt(const QuantLib::Period& v);
std::string fmt(ore::data::FXForwardQuote::FxFwdString v);
template <class T>
std::string fmt(const boost::optional<T>& v);
template <class T>
std::string fmt(const QuantLib::ext::shared_ptr<T>& v);
template <class... Ts>
std::string fmt(const boost::variant<Ts...>& v);
template <class T>
std::string fmt(const T& v);

std::string fmt(const std::string& v) {
    return v;
}
std::string fmt(bool v) {
    return v ? "true" : "false";
}
std::string fmt(int v) {
    return std::to_string(v);
}
std::string fmt(QuantLib::Size v) {
    return std::to_string(v);
}
std::string fmt(QuantLib::Real v) {
    return v == QuantLib::Null<QuantLib::Real>() ? "null" : std::format("{}", v);
}
std::string fmt(const QuantLib::Date& v) {
    return v == QuantLib::Date() ? "null" : ore::data::to_string(v);
}
std::string fmt(const QuantLib::Period& v) {
    return ore::data::to_string(v);
}
std::string fmt(ore::data::FXForwardQuote::FxFwdString v) {
    using enum ore::data::FXForwardQuote::FxFwdString;
    switch (v) {
        case ON:
            return "ON";
        case TN:
            return "TN";
        case SN:
            return "SN";
    }
    return "unknown";
}

template <class T>
std::string fmt(const boost::optional<T>& v) {
    return v ? fmt(*v) : "none";
}

/// A strike or an expiry: its concrete class matters as much as its text, because
/// the class is what says ATM, delta, moneyness or absolute.
template <class T>
std::string fmt(const QuantLib::ext::shared_ptr<T>& v) {
    if (!v)
        return "none";
    const auto& concrete = *v;
    return boost::core::demangle(typeid(concrete).name()) + ":" + v->toString();
}

template <class... Ts>
std::string fmt(const boost::variant<Ts...>& v) {
    return boost::apply_visitor([](const auto& x) { return fmt(x); }, v);
}

/// Everything else ORE can print: its enums, and QuantLib's.
template <class T>
std::string fmt(const T& v) {
    return ore::data::to_string(v);
}

std::string json_escape(const std::string& s) {
    std::string out;
    out.reserve(s.size());
    for (const char c : s) {
        switch (c) {
            case '"':
                out += "\\\"";
                break;
            case '\\':
                out += "\\\\";
                break;
            case '\n':
                out += "\\n";
                break;
            case '\t':
                out += "\\t";
                break;
            default:
                if (static_cast<unsigned char>(c) < 0x20)
                    out += std::format("\\u{:04x}", static_cast<unsigned char>(c));
                else
                    out += c;
        }
    }
    return out;
}

field_list fields_of(const QuantLib::ext::shared_ptr<ore::data::MarketDatum>& d) {
    field_list fields;
    const auto& concrete = *d;
    fields.emplace_back("class", boost::core::demangle(typeid(concrete).name()));
    fields.emplace_back("instrumentType", fmt(d->instrumentType()));
    fields.emplace_back("quoteType", fmt(d->quoteType()));
#include "datum_dump.inc"
    return fields;
}

}

int main() {
    // The earliest date QuantLib allows. ORE refuses a dated forward or option
    // whose expiry falls before the as-of date, and the corpus carries expiries
    // back to 2016, so a later date would refuse keys ORE reads every day.
    const QuantLib::Date asof = QuantLib::Date::minDate();
    std::string key;
    while (std::getline(std::cin, key)) {
        if (key.empty() || key.front() == '#')
            continue;
        std::cout << "{\"key\":\"" << json_escape(key) << "\"";
        try {
            const auto datum = ore::data::parseMarketDatum(asof, key, 1.0);
            std::cout << ",\"accepted\":\"true\"";
            for (const auto& [name, value] : fields_of(datum))
                std::cout << ",\"" << name << "\":\"" << json_escape(value) << "\"";
        } catch (const std::exception& e) {
            std::cout << ",\"accepted\":\"false\",\"error\":\"" << json_escape(e.what()) << "\"";
        }
        std::cout << "}\n";
    }
    return 0;
}
