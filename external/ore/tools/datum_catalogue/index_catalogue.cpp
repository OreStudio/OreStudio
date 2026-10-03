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
#include <boost/core/demangle.hpp>
#include <iostream>
#include <ored/configuration/conventions.hpp>
#include <ored/utilities/indexparser.hpp>
#include <ored/utilities/to_string.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/indexes/inflationindex.hpp>
#include <ql/indexes/swapindex.hpp>
#include <ql/settings.hpp>
#include <qle/indexes/bondindex.hpp>
#include <qle/indexes/commodityindex.hpp>
#include <qle/indexes/equityindex.hpp>
#include <qle/indexes/fxindex.hpp>
#include <qle/indexes/genericindex.hpp>
#include <qle/indexes/intradaypowerindex.hpp>
#include <string>
#include <typeinfo>
#include <utility>
#include <vector>

/**
 * @file index_catalogue.cpp
 * @brief Reads ORE index names through ORE's own parseIndex and prints what it
 * made of each one, as JSON lines.
 *
 * One name per input line; blank lines and lines starting with '#' are skipped.
 * An optional argument names a conventions file to load first: ORE reads an
 * IBOR or inflation index a conventions file defines, such as CZK-CZEONIA, only
 * once that convention is loaded.
 * For a name ORE accepts, the line gives the index class, the name ORE's index
 * reports for itself, which is ORE's canonical spelling, and the inspectors of
 * the family the class belongs to. For a name ORE refuses, the line carries ORE's
 * own error. ORE Studio's index codec is tested against this output.
 */

namespace {

using field_list = std::vector<std::pair<std::string, std::string>>;

std::string json_escape(const std::string& s) {
    std::string out;
    for (const char c : s) {
        if (c == '"' || c == '\\')
            out += '\\';
        if (static_cast<unsigned char>(c) < 0x20)
            continue;
        out += c;
    }
    return out;
}

std::string date_text(const QuantLib::Date& d) {
    return d == QuantLib::Date() ? "null" : ore::data::to_string(d);
}

/// The inspectors of the family @p index belongs to, most specific first.
field_list family_fields(const QuantLib::ext::shared_ptr<QuantLib::Index>& index) {
    field_list f;
    if (auto i = QuantLib::ext::dynamic_pointer_cast<QuantLib::OvernightIndex>(index))
        f.emplace_back("overnight", "true");
    if (auto i = QuantLib::ext::dynamic_pointer_cast<QuantLib::InterestRateIndex>(index)) {
        f.emplace_back("familyName", i->familyName());
        f.emplace_back("tenor", ore::data::to_string(i->tenor()));
        f.emplace_back("currency", i->currency().code());
    }
    if (auto i = QuantLib::ext::dynamic_pointer_cast<QuantLib::ZeroInflationIndex>(index)) {
        f.emplace_back("familyName", i->familyName());
        f.emplace_back("currency", i->currency().code());
        f.emplace_back("region", i->region().name());
    }
    if (auto i = QuantLib::ext::dynamic_pointer_cast<QuantExt::FxIndex>(index)) {
        f.emplace_back("familyName", i->familyName());
        f.emplace_back("sourceCurrency", i->sourceCurrency().code());
        f.emplace_back("targetCurrency", i->targetCurrency().code());
    }
    if (auto i = QuantLib::ext::dynamic_pointer_cast<QuantExt::EquityIndex2>(index))
        f.emplace_back("familyName", i->familyName());
    if (auto i = QuantLib::ext::dynamic_pointer_cast<QuantExt::CommodityIndex>(index)) {
        f.emplace_back("underlyingName", i->underlyingName());
        f.emplace_back("isFuturesIndex", i->isFuturesIndex() ? "true" : "false");
        f.emplace_back("expiryDate", date_text(i->expiryDate()));
    }
    if (auto i = QuantLib::ext::dynamic_pointer_cast<QuantExt::IntradayPowerIndex>(index))
        f.emplace_back("deliveryDate", date_text(i->deliveryDate()));
    if (auto i = QuantLib::ext::dynamic_pointer_cast<QuantExt::BondIndex>(index))
        f.emplace_back("securityName", i->securityName());
    if (auto i = QuantLib::ext::dynamic_pointer_cast<QuantExt::BondFuturesIndex>(index)) {
        f.emplace_back("futureContract", i->futureContract());
        f.emplace_back("futureExpiryDate", date_text(i->futureExpiryDate()));
    }
    if (auto i = QuantLib::ext::dynamic_pointer_cast<QuantExt::GenericIndex>(index))
        f.emplace_back("expiry", date_text(i->expiry()));
    return f;
}

void print(const std::string& name, const field_list& fields) {
    std::cout << "{\"key\":\"" << json_escape(name) << "\"";
    for (const auto& [k, v] : fields)
        std::cout << ",\"" << k << "\":\"" << json_escape(v) << "\"";
    std::cout << "}\n";
}

}

int main(int argc, char* argv[]) {
    // A name ORE completes from the evaluation date, such as a power index with no
    // delivery date, must print the same on every run, so the date is QuantLib's
    // earliest, as the datum catalogue's as-of date is.
    QuantLib::Settings::instance().evaluationDate() = QuantLib::Date::minDate();
    if (argc > 1) {
        auto conventions = QuantLib::ext::make_shared<ore::data::Conventions>();
        conventions->fromFile(argv[1]);
        ore::data::InstrumentConventions::instance().setConventions(conventions);
    }
    std::string line;
    while (std::getline(std::cin, line)) {
        if (line.empty() || line.front() == '#')
            continue;
        try {
            const auto index = ore::data::parseIndex(line);
            field_list fields{{"accepted", "true"},
                              {"class", boost::core::demangle(typeid(*index).name())},
                              {"name", index->name()}};
            for (auto& f : family_fields(index))
                fields.push_back(std::move(f));
            print(line, fields);
        } catch (const std::exception& e) {
            print(line, {{"accepted", "false"}, {"error", e.what()}});
        }
    }
    return 0;
}
