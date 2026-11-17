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
#include "ores.ore.core/xml/forwarding_curves.hpp"
#include "ores.ore.core/domain/curve_configuration_projection.hpp"
#include "ores.ore.core/domain/run_document_projection.hpp"
#include "ores.ore.core/domain/todays_market_projection.hpp"
#include <algorithm>
#include <cctype>
#include <format>
#include <map>
#include <set>
#include <sstream>

namespace ores::ore::xml {

namespace {

constexpr std::string_view discount_prefix = "DISCOUNT/RATE/";

/**
 * @brief The file a Setup parameter of the run document names, empty when none.
 */
std::string named_file(const input_files& files, std::string_view parameter) {
    const auto run = files.find(std::string(run_document_file));
    if (run == files.end())
        return {};
    const auto setup = domain::run_document_projection::map_setup(
        domain::run_document_projection::read(run->second, std::string(run_document_file)));
    const auto file = std::ranges::find_if(
        setup.files, [parameter](const auto& f) { return f.parameter == parameter; });
    return file == setup.files.end() ? std::string{} : file->name;
}

/**
 * @brief The zero convention a curve of a currency reads its tenor pillars
 * with: the currency's own, or the generic one, or none.
 */
std::string zero_convention(std::string_view conventions, std::string_view currency) {
    for (const auto& id : {std::format("{}-ZERO-TENOR-BASED", currency),
                           std::string("CCY-ZERO-CONVENTIONS-TENOR-BASED")})
        if (conventions.find(std::format("<Id>{}</Id>", id)) != std::string_view::npos)
            return id;
    return {};
}

/**
 * @brief Whether a character ends an index name.
 */
bool bounded(char c) {
    return !(std::isalnum(static_cast<unsigned char>(c)) || c == '-' || c == '_');
}

/**
 * @brief Whether the portfolio names an index as a whole word, so GBP-LIBOR-3M
 * is not found inside GBP-LIBOR-3MX.
 */
bool names(std::string_view portfolio, std::string_view index) {
    for (auto at = portfolio.find(index); at != std::string_view::npos;
         at = portfolio.find(index, at + 1)) {
        const auto end = at + index.size();
        if ((at == 0 || bounded(portfolio[at - 1])) &&
            (end == portfolio.size() || bounded(portfolio[end])))
            return true;
    }
    return false;
}

/**
 * @brief The term rates of an overnight index the portfolio names, such as
 * GBP-SONIA-3M for GBP-SONIA: the index, a dash and a tenor of digits and one
 * of D, W, M or Y.
 */
std::set<std::string> term_rates(std::string_view portfolio, std::string_view index) {
    std::set<std::string> out;
    const auto prefix = std::format("{}-", index);
    for (auto at = portfolio.find(prefix); at != std::string_view::npos;
         at = portfolio.find(prefix, at + 1)) {
        if (at > 0 && !bounded(portfolio[at - 1]))
            continue;
        auto end = at + prefix.size();
        const auto digits = end;
        while (end < portfolio.size() && std::isdigit(static_cast<unsigned char>(portfolio[end])))
            ++end;
        if (end == digits || end == portfolio.size() ||
            std::string_view("DWMY").find(portfolio[end]) == std::string_view::npos)
            continue;
        ++end;
        if (end == portfolio.size() || bounded(portfolio[end]))
            out.insert(std::string(portfolio.substr(at, end - at)));
    }
    return out;
}

std::string yield_curve_xml(const published_curve& c, std::string_view convention) {
    std::string quotes;
    for (const auto& q : c.quotes)
        quotes += std::format("<Quote>{}</Quote>", q);
    return std::format(
        "<YieldCurve><CurveId>{0}</CurveId>"
        "<CurveDescription>{0} from the discount factors the party publishes</CurveDescription>"
        "<Currency>{1}</Currency><DiscountCurve/>"
        "<Segments><Direct><Type>Discount</Type><Quotes>{2}</Quotes>"
        "<Conventions>{3}</Conventions></Direct></Segments>"
        "<InterpolationVariable>Discount</InterpolationVariable>"
        "<InterpolationMethod>LogLinear</InterpolationMethod>"
        "<YieldCurveDayCounter>A365</YieldCurveDayCounter><Tolerance>1e-12</Tolerance>"
        "</YieldCurve>",
        c.name,
        c.currency,
        quotes,
        convention);
}

}

std::vector<published_curve> published_curves(std::string_view market_data) {
    std::map<std::string, published_curve> by_name;
    std::istringstream in{std::string(market_data)};
    std::string date, key, value;
    while (in >> date >> key >> value) {
        if (!key.starts_with(discount_prefix))
            continue;
        const auto rest = std::string_view(key).substr(discount_prefix.size());
        const auto first = rest.find('/');
        const auto second = rest.find('/', first + 1);
        if (first == std::string_view::npos || second == std::string_view::npos ||
            rest.find('/', second + 1) != std::string_view::npos)
            continue;
        const auto name = std::string(rest.substr(first + 1, second - first - 1));
        auto& curve = by_name[name];
        curve.currency = std::string(rest.substr(0, first));
        curve.name = name;
        curve.quotes.push_back(key);
    }
    std::vector<published_curve> out;
    for (auto& [_, c] : by_name) {
        std::ranges::sort(c.quotes);
        c.quotes.erase(std::ranges::unique(c.quotes).begin(), c.quotes.end());
        out.push_back(std::move(c));
    }
    return out;
}

input_files name_forwarding_curves(const input_files& files,
                                   std::string_view market_data,
                                   std::string_view portfolio) {
    const auto market_file = named_file(files, "marketConfigFile");
    const auto curve_file = named_file(files, "curveConfigFile");
    const auto conventions_file = named_file(files, "conventionsFile");
    const auto market_it = files.find(market_file);
    const auto curve_it = files.find(curve_file);
    const auto conventions_it = files.find(conventions_file);
    if (market_it == files.end() || curve_it == files.end() || conventions_it == files.end())
        return files;

    auto market = domain::todays_market_projection::read(market_it->second, market_file);
    auto curves = domain::curve_configuration_projection::read(curve_it->second, curve_file);

    std::set<std::string> forwarded;
    for (const auto& collection : market.index_forwarding_curves)
        for (const auto& i : collection.index)
            forwarded.insert(i.name);
    std::set<std::string> built;
    for (const auto& y : curves.yield_curves.yield_curve)
        built.insert(y.curve_id);

    std::string forwarding_id = "default";
    for (const auto& c : market.configuration)
        if (c.id == "default" && !c.index_forwarding_curves_id.empty())
            forwarding_id = c.index_forwarding_curves_id;
    const auto collection = std::ranges::find_if(
        market.index_forwarding_curves, [&](const auto& c) { return c.id == forwarding_id; });
    if (collection == market.index_forwarding_curves.end())
        return files;

    std::string yield_curves;
    std::string indices;
    for (const auto& c : published_curves(market_data)) {
        if (forwarded.contains(c.name) || built.contains(c.name) || !names(portfolio, c.name))
            continue;
        const auto convention = zero_convention(conventions_it->second, c.currency);
        if (convention.empty())
            continue;
        yield_curves += yield_curve_xml(c, convention);
        indices += std::format("<Index name=\"{0}\">Yield/{1}/{0}</Index>", c.name, c.currency);
    }
    // A term rate is projected off its overnight index's curve, so a term rate
    // the portfolio names is forwarded on the curve its overnight index uses.
    for (const auto& i : collection->index)
        for (const auto& term : term_rates(portfolio, i.name))
            if (!forwarded.contains(term))
                indices += std::format("<Index name=\"{}\">{}</Index>", term, i.value);
    if (indices.empty())
        return files;

    auto out = files;
    if (!yield_curves.empty()) {
        const auto added_curves = domain::curve_configuration_projection::read(
            std::format("<CurveConfiguration><YieldCurves>{}</YieldCurves></CurveConfiguration>",
                        yield_curves));
        std::ranges::copy(added_curves.yield_curves.yield_curve,
                          std::back_inserter(curves.yield_curves.yield_curve));
        curves.present |= added_curves.present;
        curves.yield_curves.present |= added_curves.yield_curves.present;
        out[curve_file] = domain::curve_configuration_projection::write(curves);
    }

    const auto added_indices = domain::todays_market_projection::read(std::format(
        "<TodaysMarket><IndexForwardingCurves id=\"{}\">{}</IndexForwardingCurves></TodaysMarket>",
        forwarding_id,
        indices));
    const auto& added_collection = added_indices.index_forwarding_curves.front();
    std::ranges::copy(added_collection.index, std::back_inserter(collection->index));
    collection->present |= added_collection.present;

    out[market_file] = domain::todays_market_projection::write(market);
    return out;
}

}
