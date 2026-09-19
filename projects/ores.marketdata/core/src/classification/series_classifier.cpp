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
#include "ores.marketdata.core/classification/series_classifier.hpp"
#include <algorithm>
#include <stdexcept>
#include <string_view>
#include <utility>

namespace ores::marketdata::core {

namespace {

// The two values the asset_class_source column allows. "literal" rows carry
// their class in the row; "correlation_operands" rows derive it from the
// operands the qualifier names, so they carry no class of their own.
constexpr std::string_view k_literal_source = "literal";
constexpr std::string_view k_correlation_operands_source = "correlation_operands";

/**
 * ORE names a correlation operand after the factor class it belongs to: an
 * equity index is EQ-RIC:.SPX, an FX rate is FX-GENERIC-USD-EUR, a commodity
 * future is COMM-NYMEX:CL. A rates swap rate is the exception — it carries no
 * prefix and names its currency instead, as in EUR-CMS-10Y.
 */
std::optional<std::string> operand_asset_class(const std::string& operand) {
    if (operand.rfind("EQ-", 0) == 0)
        return "equity";
    if (operand.rfind("FX-", 0) == 0)
        return "fx";
    if (operand.rfind("COMM-", 0) == 0)
        return "commodity";
    if (operand.find("-CMS-") != std::string::npos)
        return "interest_rates";
    return std::nullopt;
}

std::vector<std::string_view> split_slash(const std::string& s) {
    std::vector<std::string_view> parts;
    std::string_view view(s);
    std::size_t start = 0;
    while (true) {
        const auto pos = view.find('/', start);
        if (pos == std::string_view::npos) {
            parts.push_back(view.substr(start));
            break;
        }
        parts.push_back(view.substr(start, pos - start));
        start = pos + 1;
    }
    return parts;
}

/**
 * The first two qualifier segments of a correlation key are its two operands;
 * any further segments are surface coordinates. Classes keep operand order and
 * appear once each, so FX against FX yields one class, not two.
 */
std::vector<std::string> correlation_asset_classes(const std::string& qualifier) {
    const auto parts = split_slash(qualifier);
    std::vector<std::string> classes;
    for (std::size_t i = 0; i < parts.size() && i < 2; ++i) {
        const auto code = operand_asset_class(std::string(parts[i]));
        if (!code)
            continue;
        if (std::find(classes.begin(), classes.end(), *code) == classes.end())
            classes.push_back(*code);
    }
    return classes;
}

}

series_classifier::series_classifier(std::vector<domain::series_classification_rule> rules) {
    if (rules.empty())
        throw std::invalid_argument(
            "ores_marketdata_series_classification_rules_tbl is unseeded: no series "
            "classification rules were supplied, so no ORE series key could be classified and "
            "every import that creates a series would abort.");

    for (auto& rule : rules) {
        if (rule.asset_class_source != k_literal_source &&
            rule.asset_class_source != k_correlation_operands_source)
            throw std::invalid_argument("series classification rule for '" + rule.series_type +
                                        "/" + rule.metric + "' names the asset class source '" +
                                        rule.asset_class_source + "'; the table allows only '" +
                                        std::string(k_literal_source) + "' and '" +
                                        std::string(k_correlation_operands_source) + "'.");

        if (rule.asset_class_source == k_literal_source && !rule.asset_class_code)
            throw std::invalid_argument(
                "series classification rule for '" + rule.series_type + "/" + rule.metric +
                "' reads its asset class from the row but carries no asset class code.");

        if (rule.asset_class_source == k_correlation_operands_source && rule.asset_class_code)
            throw std::invalid_argument(
                "series classification rule for '" + rule.series_type + "/" + rule.metric +
                "' derives its asset classes from the qualifier operands and also carries the "
                "asset class code '" +
                *rule.asset_class_code + "'; a reader would not know which of the two to record.");

        by_type_[rule.series_type][rule.metric] = std::move(rule);
    }
}

series_classification series_classifier::classify(const std::string& series_type,
                                                  const std::string& metric,
                                                  const std::string& qualifier) const {
    const auto result = try_classify(series_type, metric, qualifier);
    if (!result)
        throw std::invalid_argument("No classification rule for ORE series key: " + series_type +
                                    "/" + metric + "/" + qualifier);
    return *result;
}

std::optional<series_classification> series_classifier::try_classify(
    const std::string& series_type, const std::string& metric, const std::string& qualifier) const {
    const auto type = by_type_.find(series_type);
    if (type == by_type_.end())
        return std::nullopt;

    const auto& by_metric = type->second;
    auto rule = by_metric.find(metric);
    if (rule == by_metric.end())
        rule = by_metric.find(std::string{});
    if (rule == by_metric.end())
        return std::nullopt;

    const auto& row = rule->second;
    if (row.asset_class_source == k_correlation_operands_source)
        return series_classification{correlation_asset_classes(qualifier),
                                     row.series_subclass_code};

    return series_classification{{*row.asset_class_code}, row.series_subclass_code};
}

std::vector<std::string> series_classifier::known_series_types() const {
    std::vector<std::string> types;
    types.reserve(by_type_.size());
    for (const auto& [series_type, by_metric] : by_type_)
        types.push_back(series_type);
    std::sort(types.begin(), types.end());
    return types;
}

}
